module Hegel.Property.Internal
  ( -- * Property monad
    PropertyT (..),
    Property,
    Env (..),
    Journal (..),
    Scope (..),
    withScope,
    hoist,

    -- * Draws
    forAll,
    forAllWith,
    forAllWithLabel,
    forAllSilent,

    -- * Notes
    note,
    noteFailure,
    nested,
    annotate,
    annotateShow,
    footnote,

    -- * Discards
    assume,
    discard,

    -- * Finalizers
    Finalizers,
    newFinalizers,
    registerFinalizer,
    drainFinalizers,
    resource,
    resource_,

    -- * Open forks
    OpenForks,
    ForkState (..),
    ForkEntry (..),
    newOpenForks,
    registerFork,
    deregisterFork,
    collectLeaks,
    closeOpenForks,
    checkCloneDepth,

    -- * Concurrent-branch mechanics shared by "Hegel.Property.Branch" and

    -- "Hegel.Property.Fork"
    withBaseRunInIO,
    foldForkNotes,
    foldBranchNotes,
    runBranch,

    -- * Runner hooks
    runPropertyT,
    propertyAction,
    observeProperty,
    tryProperty,
    failureDetails,

    -- * Env access
    askEnv,
  )
where

import Control.Exception (SomeException, fromException)
import Control.Exception qualified as E
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT (..), ask, local)
import Data.Foldable (for_, toList, traverse_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (catMaybes, listToMaybe)
import Data.Sequence ((|>))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Stack (HasCallStack, SrcLoc, callStack, withFrozenCallStack)
import Hegel.Assertion (AssertionFailure (..), callSite)
import Hegel.Diff (Diff)
import Hegel.Gen.Internal (AssumeRejected (..), Gen, draw)
import Hegel.Internal.Control (MalformedTest (..), NoBacktrace (..), isControlSignal, isFailure)
import Hegel.Internal.Event qualified as Event
import Hegel.Internal.TestCase (TestCase (..))
import Hegel.Internal.TestCase qualified as TestCase
import Hegel.Internal.Tick qualified as Tick
import Hegel.Report.Note (Note (..), NoteKind (..), renderValue)
import UnliftIO (MonadUnliftIO, withRunInIO)
import UnliftIO.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)

-- | Whether the current run records its journal.
--
-- Ordinary cases (including every shrink replay) run 'Silent'; only the
-- final reconstruction replay ('observeProperty') runs 'Recording'. Under
-- 'Silent', 'journalNote' never constructs the 'Note' at all, so its 'Text'
-- and 'SrcLoc' arguments stay unevaluated thunks. Rendering work for the
-- journal is then paid once per failure, not once per step of every case.
data Journal
  = Silent
  | Recording !(Note -> IO ())

-- | How restricted the ambient context is for primitives like 'resource'
-- whose release is deferred to the case boundary.
--
-- Ordered least to most restrictive. 'withScope' only ever raises the
-- ambient scope via 'max', so 'InStep', the type's maximum, absorbs any other
-- scope once a call chain has entered it and cannot be downgraded by
-- anything nested inside.
data Scope
  = -- | The default: no restriction from this mechanism.
    Unrestricted
  | -- | A stateful machine's per-case setup, e.g. @Machine.initial@.
    CaseSetup
  | -- | A stateful rule's @apply@ or an invariant's @check@, either of which
    -- may run any number of times in one case.
    InStep
  deriving stock (Eq, Ord, Show)

-- | The per-test-case environment a property runs against.
data Env = Env
  { testCase :: !TestCase,
    journal :: !Journal,
    -- | Ambient nesting level stamped onto each journaled 'Note'; raised by
    -- 'nested'.
    noteDepth :: !Int,
    -- | Cleanup actions registered by 'registerFinalizer'.
    finalizers :: !Finalizers,
    -- | Forks spawned in this scope, settled at scope exit.
    openForks :: !OpenForks,
    -- | Clone-stream nesting depth of this scope: 0 at the top level, one
    -- higher in each concurrent branch or fork body.
    cloneDepth :: !Int,
    -- | Ceiling 'cloneDepth' is checked against before acquiring another
    -- clone, constant for the whole run.
    cloneDepthLimit :: !Int,
    -- | The ambient 'Scope' for the current context, raised by 'withScope'.
    scope :: !Scope
  }

-- | A property: test logic interleaved with generator draws against a live
-- test case.
--
-- Unlike @'Hegel.Property.forEach' gen body@, where all draws happen up
-- front, a 'PropertyT' may draw ('forAll'), perform effects, and make
-- assertions in any order.
--
-- Failure is exception-based ('Hegel.Assertion.AssertionFailure' from
-- 'Hegel.Assertion.assert'\/'Hegel.Assertion.failure', or any other
-- exception), so assertions work unchanged under any transformer stack
-- layered on top.
--
-- __NOTE__: The entire body of a 'Property' is re-run on every shrink attempt,
-- and once more to reconstruct the failure report; effects must tolerate
-- repetition.
newtype PropertyT m a = PropertyT (ReaderT Env m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFail, MonadUnliftIO)

-- | A property over 'IO', as consumed by the runners' @check@ entry points.
type Property = PropertyT IO

instance MonadTrans PropertyT where
  lift = PropertyT . lift

-- | Run a property in a different base monad.
--
-- > check settings (hoist (runAppM env) myProp)
hoist :: (forall x. m x -> n x) -> PropertyT m a -> PropertyT n a
hoist f (PropertyT (ReaderT g)) = PropertyT $ ReaderT (f . g)

-- | Expose the full 'Env' to a caller.
askEnv :: (Monad m) => PropertyT m Env
askEnv = PropertyT ask
{-# INLINE askEnv #-}

-- | Send a note to the journal. The primitive underneath 'annotate' and
-- 'footnote', for library-internal callers that need to control the recorded
-- 'SrcLoc' (or omit it) explicitly.
note :: (MonadIO m) => NoteKind -> Maybe SrcLoc -> Text -> PropertyT m ()
note = journalNote
{-# INLINE note #-}

-- | Journal a 'Failure': an assertion's message, source location, and diff,
-- to be rendered in-band in the report.
--
-- See 'Hegel.Report.Failure'.
noteFailure :: (MonadIO m) => Maybe SrcLoc -> Maybe Diff -> Text -> PropertyT m ()
noteFailure loc diff = journalNote (Failure diff) loc
{-# INLINE noteFailure #-}

-- | The sole 'Note' construction site: stamp the ambient 'noteDepth' and a
-- fresh clock from the shared event-stream counter onto the note, and hand it
-- to the journal.
--
-- Under 'Silent' the 'Note' is never constructed, so its strict fields never
-- force @loc@ or @text@ and the clock is never ticked. 'Journal' explains why
-- silent notes must cost nothing.
journalNote :: (MonadIO m) => NoteKind -> Maybe SrcLoc -> Text -> PropertyT m ()
journalNote kind loc text = PropertyT do
  env <- ask
  case env.journal of
    Silent -> pure ()
    Recording sink -> liftIO do
      clock <- Tick.next env.testCase.recording
      sink Note {kind, text, loc, depth = env.noteDepth, clock}
{-# INLINEABLE journalNote #-}

-- | Run a property with its journaled notes recorded one level deeper.
--
-- 'Hegel.Stateful' uses this to nest a rule\/invariant's draws under the step
-- that produced them. Purely a reporting concern: draw behavior is unchanged.
nested :: PropertyT m a -> PropertyT m a
nested (PropertyT r) = PropertyT $ local (\e -> e {noteDepth = e.noteDepth + 1}) r
{-# INLINE nested #-}

-- | Run a property with its ambient 'Scope' raised to at least @s@.
--
-- 'Hegel.Stateful' uses this to mark a rule's @apply@ and an invariant's
-- @check@ as 'InStep', so 'resource' can refuse to run somewhere its release
-- would never fire between applications.
withScope :: Scope -> PropertyT m a -> PropertyT m a
withScope s (PropertyT r) = PropertyT $ local (\e -> e {scope = max s e.scope}) r
{-# INLINE withScope #-}

-- | Draw a value from a generator mid-test.
--
-- The drawn value is rendered to the journal so it can show up in the failure
-- report.
forAll :: (HasCallStack, MonadIO m, Show a) => Gen a -> PropertyT m a
forAll = withFrozenCallStack (forAllWith renderValue)
{-# INLINEABLE forAll #-}

-- | 'forAll' with an explicit renderer, for values without a 'Show'
-- instance (or with an unhelpful one).
forAllWith :: (HasCallStack, MonadIO m) => (a -> Text) -> Gen a -> PropertyT m a
forAllWith render gen = do
  a <- drawGen gen
  -- Bind the pool 'Var's this draw resolved to its 'Drawn' note, so the trace
  -- can render @h₁=value@.
  --
  -- Empty for generators not associated with a pool.
  --
  -- See Note [Draw provenance] in "Hegel.Report.Trace".
  provenance <- takeDraws
  note (Drawn provenance) (callSite callStack) (render a)
  pure a
{-# INLINEABLE forAllWith #-}

-- | 'forAll' with a display label, for rule draws whose bare value reads as
-- noise in the report. @qty <- forAllWithLabel \"qty\" g@ journals @qty=5@, so
-- the event log renders @restock item=\"apple\" qty=5@ rather than
-- @restock \"apple\" 5@. A specialization of 'forAllWith' whose renderer
-- prefixes the label; the label lives in the journal text, not the source (no
-- source parsing).
forAllWithLabel :: (HasCallStack, MonadIO m, Show a) => Text -> Gen a -> PropertyT m a
forAllWithLabel label = withFrozenCallStack (forAllWith (\v -> label <> "=" <> renderValue v))
{-# INLINEABLE forAllWithLabel #-}

-- | Draw a value without journaling it.
--
-- For bookkeeping draws that would only add noise to the report.
forAllSilent :: (MonadIO m) => Gen a -> PropertyT m a
forAllSilent gen = do
  a <- drawGen gen
  -- Discard any pool provenance this draw accumulated: a silent draw journals
  -- no note, so its 'Var's must not leak forward onto the next draw's note.
  _ <- takeDraws
  pure a
{-# INLINEABLE forAllSilent #-}

-- | The raw draw: sample the generator, leaving any pool 'Var's it resolved in
-- the test case's draw-provenance scratch for the caller to 'takeDraws'.
drawGen :: (MonadIO m) => Gen a -> PropertyT m a
drawGen gen = PropertyT do
  env <- ask
  liftIO (draw env.testCase gen)
{-# INLINE drawGen #-}

-- | Take and clear the pending pool-draw provenance for the current case.
takeDraws :: (MonadIO m) => PropertyT m [Event.Var]
takeDraws = PropertyT do
  env <- ask
  liftIO (TestCase.takeDraws env.testCase)
{-# INLINE takeDraws #-}

-- | Attach context to the failure report, rendered at the point it was
-- recorded.
annotate :: (HasCallStack, MonadIO m) => Text -> PropertyT m ()
annotate = note Annotation (callSite callStack)
{-# INLINE annotate #-}

-- | 'annotate' a value via its 'Show' instance.
annotateShow :: (HasCallStack, MonadIO m, Show a) => a -> PropertyT m ()
annotateShow = withFrozenCallStack (annotate . renderValue)
{-# INLINE annotateShow #-}

-- | Attach context rendered after the report body.
footnote :: (MonadIO m) => Text -> PropertyT m ()
footnote = note Footnote Nothing
{-# INLINE footnote #-}

-- | Discard the current test case when the condition is 'False'.
--
-- Use this to enforce preconditions discovered mid-test; the case is reported
-- to the engine as invalid rather than failed.
assume :: (MonadIO m) => Bool -> m ()
assume cond = if cond then pure () else discard
{-# INLINEABLE assume #-}

-- NOTE: This function _needs_ to use 'Control.Exception.throwIO' so that
-- 'AssumeRejected' can be thrown as a proper async exception.

-- | Discard the current test case unconditionally.
--
-- The discard signal is delivered as an asynchronous exception
-- ('Hegel.Internal.Control.AssumeRejected') so that catch-all handlers in the
-- property body may pass it through to the runner instead of silently ignoring
-- them.
--
-- __NOTE__: A bare 'Control.Exception.try' @\@SomeException@ will catch
-- asynchronous exceptions, which will produce undefined behavior from this
-- library.
discard :: (MonadIO m) => m a
discard = liftIO (E.throwIO AssumeRejected)
{-# INLINE discard #-}

-- * Finalizers

-- | A per-case stack of cleanup actions, drained (LIFO) at the case boundary.
--
-- Opaque, so only 'newFinalizers'\/'registerFinalizer'\/'drainFinalizers' touch
-- the underlying reference.
newtype Finalizers = Finalizers (IORef [IO ()])

-- | A fresh, empty registry.
newFinalizers :: IO Finalizers
newFinalizers = Finalizers <$> newIORef []

-- | Register a cleanup action to run at the end of the current test case.
--
-- The primitive for releasing resources acquired mid-property, when release
-- must happen outside your lexical scope; the canonical case is a resource
-- acquired in a stateful 'Hegel.Stateful.Machine'\'s @initial@, torn down at
-- the case boundary the engine controls:
--
-- > initial = do
-- >   mc <- liftIO (spawnMockCore ...)
-- >   registerFinalizer (cancel mc.thread)   -- closes over the resource
-- >   pure (Model mc ...)
--
-- Semantics are as follows:
--
-- * __Runs on exit for every test case__
--
-- * __Runs on every replay__: each shrink attempt and the reconstruction
--   replay is a fresh case with its own registrations and its own drain, so
--   nothing accumulates across replays.
--
-- * __LIFO__: last registered, first run, so nested resources release in
--   reverse acquisition order.
--
-- * __Must not draw, and must not touch the 'TestCase'\/engine__: registration
--   is a plain list push that replays identically, and a finalizer that drew
--   ('forAll') would misalign the choice sequence. Finalizers also run after
--   the case has been reported to the engine, so the borrowed test-case handle
--   is stale — do not call back into generation or the FFI from one.
--
-- * __Acquire, then register, with no draw in between__: a draw ('forAll') can
--   discard or stop the case, so acquiring a resource and then drawing before
--   'registerFinalizer' runs leaks it on that path. Register immediately after
--   acquisition, or use 'resource' to pair the two atomically.
--
-- * __Must return promptly__: finalizers drain under
--   'Control.Exception.uninterruptibleMask_', so one that blocks indefinitely
--   hangs the run un-interruptibly.
--
-- * __A finalizer that throws aborts the run__ as 'Hegel.Report.Errored': a
--   failed teardown means per-case isolation may be broken and later
--   cases\/replays can no longer be trusted. If the case had already failed,
--   the abort discards that counterexample — but a persisted database has
--   already stored its blob (so it replays next run); under the default
--   settings (database disabled) the drawn values are lost.
registerFinalizer :: (MonadIO m) => IO () -> PropertyT m ()
registerFinalizer act = do
  env <- askEnv
  let Finalizers ref = env.finalizers
  -- Newest-first, so 'drainFinalizers' can run the stack LIFO. The push is
  -- atomic so that concurrent registrations against one shared registry do
  -- not lose each other's entries.
  liftIO (atomicModifyIORef' ref \xs -> (act : xs, ()))
{-# INLINEABLE registerFinalizer #-}

-- | Acquire a resource and register its release as a per-case finalizer in
-- one step, so no draw can slip in between acquisition and registration.
--
-- Release runs at the end of the enclosing scope: the case boundary on the
-- live run, every shrink probe, and the reconstruction replay, or a branch's
-- own exit when called inside 'Hegel.Property.Branch.concurrently' or
-- 'Hegel.Property.Fork.spawn'. That is what gives a resource like a database
-- transaction its per-case isolation.
--
-- Throws 'MalformedTest' when called from a stateful rule's @apply@ or an
-- invariant's @check@. Either may run any number of times in one case, so a
-- release deferred to scope end would never fire between applications;
-- acquire case-scoped resources in a stateful 'Hegel.Stateful.Machine'\'s
-- @initial@ instead, or use
-- 'Control.Exception.bracket'\/'Control.Exception.finally' for cleanup
-- scoped to one step.
resource :: (MonadIO m) => IO a -> (a -> IO ()) -> PropertyT m a
resource open close = do
  env <- askEnv
  when (env.scope >= InStep) $ liftIO (E.throwIO (MalformedTest inStepMessage))
  let Finalizers ref = env.finalizers
  -- Acquire and register as one step under 'E.mask_': an async exception
  -- landing between the two, e.g. a sibling branch failing or a fork being
  -- cancelled, would otherwise leak the resource with nothing registered yet
  -- to release it.
  liftIO $ E.mask_ do
    a <- open
    atomicModifyIORef' ref \xs -> (close a : xs, ())
    pure a
{-# INLINEABLE resource #-}

-- | 'resource' for setup/teardown with no handle to thread through.
resource_ :: (MonadIO m) => IO () -> IO () -> PropertyT m ()
resource_ open close = resource open (const close)
{-# INLINEABLE resource_ #-}

-- | Describe why 'resource' refused to run.
inStepMessage :: Text
inStepMessage =
  "resource: called inside a stateful rule's apply or an invariant's check. \
  \A rule or invariant can run any number of times per case, so its \
  \case-scoped release would never fire between applications. Acquire it in \
  \Machine.initial instead, or use Control.Exception.bracket/finally for \
  \cleanup scoped to one step."

-- * Open forks

-- | Whether a registered fork has been joined, cancelled, or is still
-- running.
data ForkState = NotJoined | JoinedOk | JoinedFailure | Cancelled
  deriving stock (Eq, Show)

-- | A fork's registry entry. Its result type is erased behind 'settle' so
-- 'OpenForks' can hold forks of differing result types uniformly.
data ForkEntry = ForkEntry
  { -- | The fork's current lifecycle state.
    state :: !(IORef ForkState),
    -- | Await-or-cancel the fork so its clone is freed, idempotently against
    -- a fork already joined or cancelled by its owner. Returns the fork's
    -- failure message when it was still 'NotJoined' and had already failed;
    -- 'Nothing' otherwise, including when it was still running and had to be
    -- cancelled outright.
    settle :: IO (Maybe Text)
  }

-- | A scope's live forks, spawned by 'Hegel.Property.Fork.spawn' or
-- 'Hegel.Property.Fork.scoped', settled at scope exit via 'closeOpenForks'.
--
-- Opaque, so only this module's registry operations touch the underlying
-- reference.
newtype OpenForks = OpenForks (IORef (IntMap ForkEntry))

-- | A fresh, empty registry.
newOpenForks :: IO OpenForks
newOpenForks = OpenForks <$> newIORef IntMap.empty

-- | Register a fork, returning the 1-based key its entry is settled under
-- and shown by in a @Fork N@ header.
registerFork :: OpenForks -> ForkEntry -> IO Int
registerFork (OpenForks ref) entry =
  atomicModifyIORef' ref \m ->
    let k = maybe 1 (succ . fst) (IntMap.lookupMax m)
     in (IntMap.insert k entry m, k)

-- | Remove a fork from the registry once its owner has joined or cancelled
-- it, so 'closeOpenForks' no longer considers it a leak.
deregisterFork :: OpenForks -> Int -> IO ()
deregisterFork (OpenForks ref) k = atomicModifyIORef' ref \m -> (IntMap.delete k m, ())

-- | Settle every fork still in the registry, in creation order, and report
-- whether any of them had never been joined or cancelled by their owner.
--
-- Always settles regardless of the outcome, so every clone is freed and no
-- forked thread survives the scope closing; callers decide whether an
-- unjoined fork should abort the run, as 'closeOpenForks' does, or be
-- discarded quietly because a more pressing exception is already in flight.
collectLeaks :: OpenForks -> IO (Maybe Text)
collectLeaks (OpenForks ref) = do
  leaked <- atomicModifyIORef' ref \m -> (IntMap.empty, IntMap.toAscList m)
  results <- traverse (\(_, e) -> e.settle) leaked
  pure case leaked of
    [] -> Nothing
    _ -> Just (leakMessage (length leaked) (listToMaybe (catMaybes results)))

-- | Describe how many forks leaked and, when the first of them had already
-- failed, what it said.
leakMessage :: Int -> Maybe Text -> Text
leakMessage n mFailure =
  "Hegel.Property.Fork.spawn left "
    <> T.pack (show n)
    <> (if n == 1 then " fork" else " forks")
    <> " unjoined when its scope ended; every fork must be joined with"
    <> " Hegel.Property.Fork.join or abandoned with Hegel.Property.Fork.cancel"
    <> " before the property finishes"
    <> maybe "" ("\nthe first unjoined fork had failed: " <>) mFailure

-- | Settle every fork still in the registry and abort the run as a malformed
-- test if any of them were never joined or cancelled by their owner.
--
-- Must run before the case is reported complete: a fork still drawing
-- against its clone when the parent completes fails with an engine error of
-- its own, rather than the well-formed 'MalformedTest' this produces.
closeOpenForks :: OpenForks -> IO ()
closeOpenForks forks = collectLeaks forks >>= traverse_ (E.throwIO . MalformedTest)

-- * Concurrent-branch mechanics

-- | Fail the case immediately when the ambient clone-stream nesting depth has
-- already reached 'cloneDepthLimit', rather than letting the engine's own
-- clone-depth limit invalidate the whole clone family.
checkCloneDepth :: Env -> IO ()
checkCloneDepth env =
  when (env.cloneDepth >= env.cloneDepthLimit) $
    E.throwIO (MalformedTest (cloneDepthMessage env.cloneDepthLimit))

-- | Describe a tripped clone-depth guard.
cloneDepthMessage :: Int -> Text
cloneDepthMessage limit =
  "clone-stream nesting exceeded Settings.maxCloneDepth ("
    <> T.pack (show limit)
    <> "); raise it for a test that nests Hegel.Property.Fork.spawn or"
    <> " Hegel.Property.Branch.concurrently this deeply on purpose, or use"
    <> " Hegel.Property.Branch.replicateConcurrently for fan-out"

-- | Obtain a way to run the base monad's actions in 'IO', independent of the
-- ambient 'Env'. "Hegel.Property.Branch" and "Hegel.Property.Fork" use
-- this to step outside the parent's environment before assembling each
-- branch's own.
withBaseRunInIO :: (MonadUnliftIO m) => ((forall x. m x -> IO x) -> IO b) -> PropertyT m b
withBaseRunInIO inner = withRunInIO \run -> inner (run . lift)

-- | Fold one branch's or fork's buffered notes into the ambient journal, one
-- level deeper than the caller's own 'noteDepth', under a header labeled
-- with the given word ("Branch" or "Fork") and index.
--
-- A no-op under a 'Silent' journal, matching every other journaling
-- primitive's cost discipline.
foldForkNotes :: Env -> Text -> Int -> [Note] -> IO ()
foldForkNotes env label i notes = case env.journal of
  Silent -> pure ()
  Recording sink -> do
    clock <- Tick.next env.testCase.recording
    sink Note {kind = BranchHeader i, text = label <> " " <> T.pack (show i), loc = Nothing, depth = env.noteDepth, clock}
    for_ notes sink

-- | 'foldForkNotes' over every branch of a fixed-arity combinator
-- ('Hegel.Property.Branch.concurrently' and its siblings), labeled
-- "Branch" and numbered in call order.
foldBranchNotes :: Env -> [[Note]] -> IO ()
foldBranchNotes env branchNotes =
  for_ (zip [1 :: Int ..] branchNotes) \(i, notes) -> foldForkNotes env "Branch" i notes

-- | Run one branch of a concurrent combinator or fork body against its own
-- clone, in a fresh 'Env' nested one level deeper than the parent's.
--
-- Neither a control signal nor an 'Hegel.Assertion.AssertionFailure' escapes
-- as an exception: each comes back as 'Left' so a caller running several
-- branches at once (as 'Hegel.Property.Branch.concurrently' does) can let
-- every branch run to completion regardless of a sibling's fate, and decide
-- deterministically which failure, if any, to report as the case's shrink
-- target.
--
-- A real failure additionally gets journaled in-band as a 'BranchFailure', so
-- a branch that does not win the shrink target still shows its own message,
-- location, and diff in the report. A discard or budget stop does not, since
-- neither is a failure to explain.
--
-- A finalizer registered inside the branch is drained at the branch's own
-- exit, and any fork spawned inside it is settled there too; a teardown
-- failure or a leaked fork propagates immediately, the same severity a
-- top-level occurrence of either carries.
runBranch ::
  (forall x. m x -> IO x) ->
  Env ->
  TestCase ->
  PropertyT m a ->
  IO (Either E.SomeException a, [Note])
runBranch runBase parentEnv testCase body = do
  finalizers <- newFinalizers
  openForks <- newOpenForks
  notesRef <- newIORef Seq.empty
  let branchJournal = case parentEnv.journal of
        Silent -> Silent
        Recording _ -> Recording \n -> modifyIORef' notesRef (|> n)
      branchEnv =
        Env
          { testCase,
            journal = branchJournal,
            noteDepth = parentEnv.noteDepth + 1,
            scope = parentEnv.scope,
            finalizers,
            openForks,
            cloneDepth = parentEnv.cloneDepth + 1,
            cloneDepthLimit = parentEnv.cloneDepthLimit
          }
  eRes <-
    tryProperty (runBase (runPropertyT branchEnv body))
      `E.onException` (drainFinalizers finalizers *> void (collectLeaks openForks))
  failures <- drainFinalizers finalizers
  case failures of
    [] -> pure ()
    e : _ -> E.throwIO e
  closeOpenForks openForks
  notes <- toList <$> readIORef notesRef
  failureNotes <- case (branchJournal, eRes) of
    (Recording _, Left e) | isFailure e -> do
      clock <- Tick.next testCase.recording
      let (msg, mloc, diff) = failureDetails e
      pure [Note {kind = BranchFailure diff, text = msg, loc = mloc, depth = branchEnv.noteDepth, clock}]
    _ -> pure []
  pure (eRes, notes <> failureNotes)

-- * Runner hooks

-- | Run a property against the given 'Env'.
runPropertyT :: Env -> PropertyT m a -> m a
runPropertyT env (PropertyT r) = runReaderT r env
{-# INLINE runPropertyT #-}

-- | Lower a property to a per-case run loop, against a caller-owned finalizer
-- registry and fork registry, checking fork/branch nesting against the given
-- clone-depth ceiling.
--
-- Ordinary cases run with a no-op journal; failing cases are journaled later
-- via 'observeProperty' on the engine's minimal counterexample.
propertyAction :: Int -> Property () -> Finalizers -> OpenForks -> TestCase -> IO ()
propertyAction cloneDepthLimit prop finalizers openForks testCase =
  runPropertyT
    Env {testCase, journal = Silent, noteDepth = 0, finalizers, openForks, cloneDepth = 0, cloneDepthLimit, scope = Unrestricted}
    prop

-- | Run every registered finalizer, LIFO, capturing each one's exception so a
-- thrower does not skip the rest, and returning them all.
--
-- __NOTE__: Runs under 'E.uninterruptibleMask_'; finalizers /must/ execute
-- promptly.
drainFinalizers :: Finalizers -> IO [SomeException]
drainFinalizers (Finalizers ref) = E.uninterruptibleMask_ do
  -- Newest-first already (registration conses onto the head), so a head-to-tail
  -- walk runs finalizers LIFO.
  fs <- atomicModifyIORef' ref \xs -> ([], xs)
  go [] fs
  where
    go acc [] = pure (reverse acc)
    go acc (f : rest) =
      E.try f >>= \case
        Right () -> go acc rest
        Left (e :: SomeException) -> go (e : acc) rest

-- | Run a property against a test case with a recording journal, returning
-- how the run ended together with the journal contents and the test case's
-- event stream (empty unless @tc@ was built with a recording
-- 'Hegel.Internal.Tick.Recording').
observeProperty :: Int -> TestCase -> Property () -> IO (Either SomeException (), [Note], [Event.Event])
observeProperty cloneDepthLimit testCase prop = do
  j <- newIORef Seq.empty
  finalizers <- newFinalizers
  openForks <- newOpenForks
  let record n = modifyIORef' j (|> n)
  eRes <-
    tryProperty
      ( runPropertyT
          Env {testCase, journal = Recording record, noteDepth = 0, finalizers, openForks, cloneDepth = 0, cloneDepthLimit, scope = Unrestricted}
          prop
      )
      `E.onException` (drainFinalizers finalizers *> void (collectLeaks openForks))
  -- This is the terminal replay: nothing runs after it, so a failed teardown
  -- cannot contaminate another case.
  failures <- drainFinalizers finalizers
  for_ failures \e -> do
    clock <- Tick.next testCase.recording
    record
      Note
        { kind = Footnote,
          text = "finalizer failed during replay: " <> T.pack (E.displayException e),
          loc = Nothing,
          depth = 0,
          clock
        }
  -- A leak here should never actually happen: if the original run had left a
  -- fork unjoined, it would have aborted the whole run as a 'MalformedTest'
  -- before ever reaching a stored reproduction blob for this replay to
  -- reconstruct. Settle unconditionally regardless, for the same resource
  -- safety 'closeOpenForks' provides elsewhere, and note it rather than
  -- override the counterexample already captured in 'eRes'.
  mLeak <- collectLeaks openForks
  for_ mLeak \msg -> do
    clock <- Tick.next testCase.recording
    record Note {kind = Footnote, text = "fork leak during replay: " <> msg, loc = Nothing, depth = 0, clock}
  notes <- toList <$> readIORef j
  events <- Tick.drain testCase.events
  pure (eRes, notes, events)

-- NOTE: This function _needs_ to use 'Control.Exception.throwIO' so that
-- all non-Hegel async exceptions are rethrown _as_ async exceptions (and not
-- re-wrapped in a synchronous exception wrapper by safe-exceptions).
--
-- The canonical home of this discipline is 'Hegel.Internal.Control'
-- ('isFailure'\/'Hegel.Internal.Control.onFailure').

-- | Like 'UnliftIO.Exception.tryAny', but additionally catches Hegel's
-- control signals ('Hegel.Internal.Control.AssumeRejected',
-- 'Hegel.Internal.Control.TestStopped'), which are async exceptions precisely so that
-- user catch-alls pass them through.
tryProperty :: IO a -> IO (Either SomeException a)
tryProperty act =
  E.try act >>= \res -> case res of
    Right a -> pure (Right a)
    Left e
      | isControlSignal e || isFailure e -> pure (Left e)
      -- Base 'E.throwIO' to preserve the exception's async flavor on rethrow;
      -- 'E.NoBacktrace' because the original throw already collected any
      -- backtrace it wanted (see the same wrapper in
      -- 'Hegel.Internal.Control.onFailure').
      | otherwise -> E.throwIO $ NoBacktrace e

-- | Attempt to recover an 'AssertionFailure' from the given exception, and (if
-- present) extract the message, callsite, and diff associated with it.
--
-- If the given exception is /not/ an 'AssertionFailure', render it with
-- 'displayException' and return that on its own.
failureDetails :: SomeException -> (Text, Maybe SrcLoc, Maybe Diff)
failureDetails e = case fromException e of
  Just (af :: AssertionFailure) -> (af.message, callSite af.callStack, af.diff)
  Nothing -> (T.pack (E.displayException e), Nothing, Nothing)
