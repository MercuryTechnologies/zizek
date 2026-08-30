-- | Concurrent stateful testing: several workers apply rules to one shared
-- model at once, each on its own worker.
--
-- Define a 'Machine' and run it with 'run', choosing how many workers the
-- engine may use via 'fixed', 'upTo', or 'between'.
--
-- Build a 'Rule' with 'rule', or 'grouped' to place it in a named concurrency
-- group.
module Hegel.Stateful.Concurrent
  ( -- * Specification
    Rule (..),
    rule,
    grouped,
    Invariant (..),
    Machine (..),

    -- * Concurrency groups
    anonymousGroup,
    internGroups,

    -- * Concurrency
    Concurrency,
    fixed,
    upTo,
    between,

    -- * Execution
    run,
  )
where

import Control.Exception (mask_)
import Control.Exception qualified as E
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Sequence ((|>))
import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Internal.Control (MalformedTest (..), onFailure)
import Hegel.Internal.DataSource (freeStateMachine, newConcurrentStateMachine, stateMachineNextGroup)
import Hegel.Internal.Event (Event (..))
import Hegel.Internal.StatefulRound (RoundVerdict (..), Worker (..), runRound)
import Hegel.Internal.TestCase (TestCase (..), withClones)
import Hegel.Internal.Tick (Tick)
import Hegel.Internal.Tick qualified as Tick
import Hegel.Property.Internal
  ( Env (..),
    Journal (..),
    PropertyT,
    Scope (CaseSetup, InStep),
    askEnv,
    checkCloneDepth,
    closeOpenForks,
    collectLeaks,
    failureDetails,
    nested,
    newOpenForks,
    note,
    noteFailure,
    registerFinalizer,
    runPropertyT,
    withBaseRunInIO,
    withScope,
  )
import Hegel.Report (Note (..), NoteKind (Annotation, StepHeader))
import Hegel.Stateful (Invariant (..))
import UnliftIO (MonadUnliftIO, throwIO, withRunInIO)
import UnliftIO.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef)

-- * Specification

-- | A rule applied to the shared model during a concurrent stateful test.
data Rule s m = Rule
  { -- | The rule's name.
    name :: !Text,
    -- | The concurrency group this rule belongs to.
    group :: !(Maybe Text),
    -- | The action this rule performs.
    apply :: s -> PropertyT m ()
  }

-- | Construct a 'Rule' not associated with any named concurrency group.
rule :: Text -> (s -> PropertyT m ()) -> Rule s m
rule name apply = Rule {name, group = Nothing, apply}

-- | Construct a 'Rule' in the given concurrency group.
--
-- it may run concurrently with any other rule sharing that group, and never
-- alongside a rule in a different one.
grouped :: Text -> Text -> (s -> PropertyT m ()) -> Rule s m
grouped name group apply = Rule {name, group = Just group, apply}

-- | A complete concurrent stateful test specification.
data Machine s m = Machine
  { -- | Construct the model shared by every worker.
    initial :: PropertyT m s,
    rules :: [Rule s m],
    invariants :: [Invariant s m]
  }

-- * Concurrency groups

-- | The display name used to render ungrouped rules.
anonymousGroup :: Text
anonymousGroup = "<anonymous>"

-- | The distinct group labels of a rule list, in first-appearance order,
-- and the dense identifier parallel to each input label that
-- @libhegel@'s @rule_groups@ array wants.
internGroups :: [Maybe Text] -> ([Maybe Text], [Int64])
internGroups labels = (reverse labelsRev, reverse idsRev)
  where
    (labelsRev, idsRev, _seen) = foldl' step ([], [], Map.empty) labels
    step :: ([Maybe Text], [Int64], Map.Map (Maybe Text) Int64) -> Maybe Text -> ([Maybe Text], [Int64], Map.Map (Maybe Text) Int64)
    step (ls, ids, seen) label = case Map.lookup label seen of
      Just gid -> (ls, gid : ids, seen)
      Nothing ->
        let gid = fromIntegral (Map.size seen)
         in (label : ls, gid : ids, Map.insert label gid seen)

-- * Concurrency

-- | The concurrency bounds a 'run' asks the engine for: the number of
-- workers to run each case, drawn from @[minWorkers, maxWorkers]@ and
-- weighted toward the maximum.
--
-- The engine decides the level; the caller runs exactly as many workers as it
-- draws.
--
-- Construct one with 'fixed', 'upTo', or 'between'.
data Concurrency = Concurrency
  { minWorkers :: Int,
    maxWorkers :: Int
  }
  deriving stock (Eq, Show)

-- | Run exactly @n@ workers every case. Draws no entropy to fix the level,
-- and @n == 1@ keeps the whole run deterministic.
fixed :: Int -> Concurrency
fixed n = Concurrency {minWorkers = n, maxWorkers = n}

-- | Run between 1 and @n@ workers, the level drawn per case.
upTo :: Int -> Concurrency
upTo = between 1

-- | Run between @lo@ and @hi@ workers, the level drawn per case.
between :: Int -> Int -> Concurrency
between lo hi = Concurrency {minWorkers = lo, maxWorkers = hi}

-- * Reporting

-- | Journal a real failure as an in-band 'Failure' note under @journal@, then
-- re-throw so the caller still sees the counterexample and the round\/case
-- protocol still concludes on it.
withFailureNoteIn :: (MonadUnliftIO m) => Journal -> PropertyT m a -> PropertyT m a
withFailureNoteIn journal = case journal of
  Silent -> id
  Recording _ -> \act ->
    withRunInIO \runInIO ->
      runInIO act `onFailure` \e ->
        let (message, loc, diff) = failureDetails e
         in runInIO (noteFailure loc diff message)

-- | One entry drained from a worker's round-local buffer, tagged so its
-- local clock can be read uniformly for 'mergeByClock'.
data WorkerEntry = EntryNote !Note | EntryEvent !Event

entryClock :: WorkerEntry -> Tick
entryClock (EntryNote n) = n.clock
entryClock (EntryEvent e) = e.clock

-- | Order a worker's own notes and pool events back into the single
-- sequence they occurred in.
--
-- Both streams are stamped from the same per-worker clock, so no two entries
-- entries ever share a clock value and the merge is unambiguous.
mergeByClock :: [Note] -> [Event] -> [WorkerEntry]
mergeByClock notes events = sortOn entryClock (map EntryNote notes <> map EntryEvent events)

-- | Fold one worker's notes and pool events into the root journal and event
-- buffer.
foldWorkerRound :: TestCase -> (Note -> IO ()) -> IORef Int -> Int -> Int -> [Note] -> [Event] -> IO ()
foldWorkerRound rootTc sink stepCounter roundIdx workerIdx notes events =
  for_ (mergeByClock notes events) \case
    EntryNote n -> case n.kind of
      StepHeader _ ruleName -> do
        idx <- atomicModifyIORef' stepCounter \i -> (i + 1, i + 1)
        headerClock <- Tick.next rootTc.recording
        sink Note {kind = StepHeader idx ruleName, text = stepText idx ruleName, loc = n.loc, depth = n.depth, clock = headerClock}
        annotationClock <- Tick.next rootTc.recording
        sink
          Note
            { kind = Annotation,
              text = roundWorkerText roundIdx workerIdx,
              loc = Nothing,
              depth = n.depth + 1,
              clock = annotationClock
            }
      _ -> do
        clock <- Tick.next rootTc.recording
        sink Note {kind = n.kind, text = n.text, loc = n.loc, depth = n.depth, clock}
    EntryEvent e -> do
      clock <- Tick.next rootTc.recording
      modifyIORef' rootTc.events (|> Event {clock, var = e.var, kind = e.kind})

-- | The display string for a folded step's header:
--
-- e.g. @\"Step 4: restock\"@.
stepText :: Int -> Text -> Text
stepText idx ruleName = "Step " <> T.pack (show idx) <> ": " <> ruleName

-- | The round\/worker detail line folded in alongside a step's header:
--
-- e.g. @\"round 2, worker 2\"@.
roundWorkerText :: Int -> Int -> Text
roundWorkerText roundIdx workerIdx =
  "round " <> T.pack (show roundIdx) <> ", worker " <> T.pack (show (workerIdx + 1))

-- * Execution

-- | Run a concurrent stateful test.
run :: forall s m. (MonadUnliftIO m) => Concurrency -> Machine s m -> PropertyT m ()
run bounds machine = do
  when (null machine.rules) $
    throwIO (MalformedTest "Hegel.Stateful.Concurrent.run: a Machine must have at least one rule")
  when (bounds.minWorkers < 1 || bounds.maxWorkers < bounds.minWorkers) $
    throwIO (MalformedTest "Hegel.Stateful.Concurrent.run: concurrency bounds must satisfy 1 <= min <= max")

  env <- askEnv
  liftIO (checkCloneDepth env)

  let tc = env.testCase
      (_groupNames, groupIds) = internGroups (map (.group) machine.rules)
      indexedRules = zip [0 ..] machine.rules

      -- Every invariant's draws (and any failure) run on the root test
      -- case, ambient journal untouched: unlike a worker's dispatch, only
      -- this one root handle ever touches it, so nothing here needs the
      -- concurrency safety a worker's env forces.
      checkInvariants :: s -> PropertyT m ()
      checkInvariants s =
        for_ machine.invariants \invariant ->
          nested (withFailureNoteIn env.journal (withScope InStep (invariant.check s)))

  -- Acquire the state-machine handle and register its release atomically
  -- under 'mask_', the same fix 'Hegel.Property.Internal.resource' applies.
  (sm, concurrency) <-
    withRunInIO \runInIO ->
      mask_ do
        acquired <-
          newConcurrentStateMachine
            tc
            (map (.name) machine.rules)
            groupIds
            (map (.name) machine.invariants)
            (fromIntegral bounds.minWorkers)
            (fromIntegral bounds.maxWorkers)
        runInIO (registerFinalizer (freeStateMachine tc (fst acquired)))
        pure acquired

  s0 <- withFailureNoteIn env.journal (withScope CaseSetup machine.initial)
  note Annotation Nothing "Initial invariant check."
  checkInvariants s0

  withBaseRunInIO \runBase ->
    withClones concurrency tc \clones -> do
      stepCounter <- newIORef (0 :: Int)
      roundCounter <- newIORef (0 :: Int)
      -- One private notes buffer per clone.
      workerNotes <- traverse (const (newIORef mempty)) clones
      let mkWorker (clone, notesRef) =
            let workerJournal = case env.journal of
                  Silent -> Silent
                  Recording _ -> Recording \n -> modifyIORef' notesRef (|> n)
                workerEnv =
                  env
                    { testCase = clone,
                      journal = workerJournal,
                      cloneDepth = env.cloneDepth + 1
                    }
                dispatch ruleIndex = do
                  let matchedRule = case lookup ruleIndex indexedRules of
                        Just r -> r
                        -- @libhegel@ guarantees indices in @[0, num_rules)@,
                        -- so this is unreachable unless the engine itself is
                        -- misbehaving.
                        Nothing ->
                          error
                            ( "Hegel.Stateful.Concurrent.run: libhegel returned rule index "
                                <> show ruleIndex
                                <> " for a machine with "
                                <> show (length machine.rules)
                                <> " rules. This should be impossible; please report it as a libhegel bug."
                            )
                  workerForks <- newOpenForks
                  let dispatchEnv = workerEnv {openForks = workerForks}
                  runBase
                    ( runPropertyT dispatchEnv do
                        -- The index here is a placeholder: only the root
                        -- driver, folding every worker's dispatches for the
                        -- round once the round has run, knows this step's true
                        -- global number (see 'foldWorkerRound').
                        note (StepHeader 0 matchedRule.name) Nothing matchedRule.name
                        nested (withFailureNoteIn workerJournal (withScope InStep (matchedRule.apply s0)))
                    )
                    `E.onException` void (collectLeaks workerForks)
                  closeOpenForks workerForks

                onRejected =
                  runBase (runPropertyT workerEnv (note Annotation Nothing "Rule stopped early due to violated assumption."))
             in Worker {testCase = clone, dispatch, onRejected}

          workers = map mkWorker (zip clones workerNotes)

          -- Drain every worker's per-round notes and pool events, in
          -- ascending worker order, and fold them into the root's journal
          -- and event buffer.
          foldRound = case env.journal of
            Silent -> pure ()
            Recording sink -> do
              roundIdx <- atomicModifyIORef' roundCounter \r -> (r + 1, r + 1)
              for_ (zip3 [0 :: Int ..] clones workerNotes) \(workerIdx, clone, notesRef) -> do
                notes <- Tick.drainAndReset notesRef
                events <- Tick.drainAndReset clone.events
                foldWorkerRound tc sink stepCounter roundIdx workerIdx notes events

          roundLoop = do
            mGroupId <- stateMachineNextGroup tc sm
            case mGroupId of
              -- HEGEL_STATE_MACHINE_DONE: the whole machine is done stepping.
              Nothing -> pure ()
              Just _groupId -> do
                (verdict, _dropped) <- runRound sm workers
                foldRound
                case verdict of
                  ContinueRound -> do
                    runBase (runPropertyT env (checkInvariants s0))
                    roundLoop
                  -- Base 'Control.Exception.throwIO', not the 'UnliftIO'
                  -- import above: 'e' may carry 'TestStopped'\/'AssumeRejected',
                  -- async-classified so a user's catch-all cannot swallow
                  -- them, and 'UnliftIO.throwIO' would wrap an async
                  -- exception so 'Hegel.Runner.runTestCase's 'catchControl'
                  -- no longer recognizes it.
                  Conclude e -> E.throwIO e

      roundLoop
{-# INLINEABLE run #-}
{-# SPECIALIZE run :: Concurrency -> Machine s IO -> PropertyT IO () #-}
