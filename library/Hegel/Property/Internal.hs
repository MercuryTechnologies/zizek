module Hegel.Property.Internal
  ( -- * Property monad
    PropertyT (..),
    Property,
    Env (..),
    Journal (..),
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

    -- * Runner hooks
    runPropertyT,
    propertyAction,
    observeProperty,
    failureDetails,

    -- * Env access
    askEnv,
  )
where

import Control.Exception (SomeException, fromException)
import Control.Exception qualified as E
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT (..), ask, local)
import Data.Foldable (toList)
import Data.Sequence ((|>))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Stack (HasCallStack, SrcLoc, callStack, withFrozenCallStack)
import Hegel.Assertion (AssertionFailure (..), callSite)
import Hegel.Diff (Diff)
import Hegel.Gen.Internal (AssumeRejected (..), Gen, draw)
import Hegel.Internal.Control (NoBacktrace (..), isControlSignal, isFailure)
import Hegel.Internal.Event qualified as Event
import Hegel.Internal.TestCase (TestCase (..))
import Hegel.Internal.TestCase qualified as TestCase
import Hegel.Internal.Tick qualified as Tick
import Hegel.Report.Note (Note (..), NoteKind (..), renderValue)
import UnliftIO (MonadUnliftIO)
import UnliftIO.IORef (modifyIORef', newIORef, readIORef)

-- | Whether the current run records its journal.
--
-- Ordinary cases (including every shrink replay) run 'Silent'; only the
-- final reconstruction replay ('observeProperty') runs 'Recording'. The
-- distinction is load-bearing for performance: under 'Silent',
-- 'journalNote' never constructs the 'Note' at all, so its 'Text' and
-- 'SrcLoc' arguments stay unevaluated thunks — rendering work for the
-- journal is paid once per failure, not once per step of every case.
data Journal
  = Silent
  | Recording !(Note -> IO ())

-- | The per-test-case environment a property runs against.
data Env = Env
  { testCase :: !TestCase,
    journal :: !Journal,
    -- | Ambient nesting level stamped onto each journaled 'Note'; raised by
    -- 'nested'.
    noteDepth :: !Int
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
hoist f (PropertyT (ReaderT g)) = PropertyT (ReaderT (f . g))

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
-- Under 'Silent' the 'Note' is never constructed, so @loc@ and @text@ are
-- never forced (the strict 'Note' fields would otherwise evaluate them) and
-- the clock is never ticked — the zero-cost property is load-bearing (see
-- 'Journal').
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
nested (PropertyT r) = PropertyT (local (\e -> e {noteDepth = e.noteDepth + 1}) r)
{-# INLINE nested #-}

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

-- * Runner hooks

-- | Run a property against the given 'Env'.
runPropertyT :: Env -> PropertyT m a -> m a
runPropertyT env (PropertyT r) = runReaderT r env
{-# INLINE runPropertyT #-}

-- | Lower a property to a per-case run loop.
--
-- Ordinary cases run with a no-op journal; failing cases are journaled later
-- via 'observeProperty' on the engine's minimal counterexample.
propertyAction :: Property () -> TestCase -> IO ()
propertyAction prop tc =
  runPropertyT Env {testCase = tc, journal = Silent, noteDepth = 0} prop

-- | Run a property against a test case with a recording journal, returning
-- how the run ended together with the journal contents and the test case's
-- event stream (empty unless @tc@ was built with a recording
-- 'Hegel.Internal.Tick.Recording').
observeProperty :: TestCase -> Property () -> IO (Either SomeException (), [Note], [Event.Event])
observeProperty tc prop = do
  j <- newIORef Seq.empty
  let record n = modifyIORef' j (|> n)
  eRes <- tryProperty (runPropertyT Env {testCase = tc, journal = Recording record, noteDepth = 0} prop)
  notes <- toList <$> readIORef j
  events <- Tick.drain tc.events
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
      | otherwise -> E.throwIO (NoBacktrace e)

-- | Attempt to recover an 'AssertionFailure' from the given exception, and (if
-- present) extract the message, callsite, and diff associated with it.
--
-- If the given exception is /not/ an 'AssertionFailure', render it with
-- 'displayException' and return that on its own.
failureDetails :: SomeException -> (Text, Maybe SrcLoc, Maybe Diff)
failureDetails e = case fromException e of
  Just (af :: AssertionFailure) -> (af.message, callSite af.callStack, af.diff)
  Nothing -> (T.pack (E.displayException e), Nothing, Nothing)
