-- | The engine's round-based state-machine protocol, generalized over any
-- number of workers.
module Hegel.Internal.StatefulRound
  ( -- * Workers
    RoundSpan (..),
    Worker (..),
    runWorkerRound,

    -- * Round resolution
    WorkerOutcome (..),
    classifyWorkerOutcome,
    RoundVerdict (..),
    resolveRound,

    -- * Fan-out
    runRound,

    -- * Reporting
    lookupRule,
    stepText,
  )
where

import Control.Exception (SomeException, fromException, mask, onException, throw, throwIO, toException)
import Data.Foldable (traverse_)
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable (for)
import Foreign (Ptr)
import Hegel.Exception (InvariantViolation (..))
import Hegel.Internal.Control (AssumeRejected (..), ControlSignal (Assume, Stop), TestStopped (..), catchControl, isAborting)
import Hegel.Internal.DataSource (Label (LabelStatefulRule), startSpan, stateMachineNextRule, stateMachineRuleRejected, stopSpan)
import Hegel.Internal.Foreign.Raw (HegelStateMachine)
import Hegel.Internal.TestCase (TestCase)
import UnliftIO.Async (Async)
import UnliftIO.Async qualified as Async

-- * Workers

-- | Who opens and closes the 'LabelStatefulRule' span around a worker's pull
-- for the round.
data RoundSpan
  = Own
  | Caller

-- | One worker's connection into a round: its own test case to draw against,
-- how to apply the rule at a given index, what to do once a rejected rule has
-- been reported to the engine, and who owns this worker's 'RoundSpan'.
data Worker = Worker
  { testCase :: !TestCase,
    dispatch :: !(Int -> IO ()),
    onRejected :: !(IO ()),
    roundSpan :: !RoundSpan
  }

-- | Pull rules for one worker from @sm@ until the round's own budget is
-- exhausted (the join point) or a terminal outcome ends it early.
runWorkerRound :: Ptr HegelStateMachine -> Worker -> Int -> IO ()
runWorkerRound sm w workerIndex = case w.roundSpan of
  Own -> do
    startSpan w.testCase LabelStatefulRule
    loop False `onException` stopSpan w.testCase False
  Caller -> loop False
  where
    loop rejected = do
      mRuleIndex <- stateMachineNextRule w.testCase sm workerIndex
      case mRuleIndex of
        Nothing -> case w.roundSpan of
          Own -> stopSpan w.testCase rejected
          Caller -> pure ()
        Just ruleIndex -> do
          verdict <- (Right <$> w.dispatch ruleIndex) `catchControl` (pure . Left)
          case verdict of
            Right () -> loop rejected
            Left Assume -> stateMachineRuleRejected w.testCase sm workerIndex *> w.onRejected *> loop True
            Left Stop -> throwIO TestStopped

-- * Round resolution

-- | How one worker's round ended.
data WorkerOutcome
  = -- | The round's own budget was exhausted normally; more rounds may
    -- follow.
    RoundDone
  | -- | The control draw itself rejected an assumption; the engine has
    -- concluded the whole family invalid.
    RoundInvalid
  | -- | The choice budget ran out.
    RoundOverrun
  | -- | A @libhegel@ error escaped: a framework or usage bug.
    RoundControlError !SomeException
  | -- | An ordinary failure, such as an assertion, or some other exception.
    RoundPanicked !SomeException

-- | Classify an exception that ended a worker's round, per 'WorkerOutcome'.
classifyWorkerOutcome :: SomeException -> WorkerOutcome
classifyWorkerOutcome e
  | Just AssumeRejected <- fromException e = RoundInvalid
  | Just TestStopped <- fromException e = RoundOverrun
  | isAborting e = RoundControlError e
  | otherwise = RoundPanicked e

-- | The conclusion of a given round.
data RoundVerdict
  = -- | Every worker finished its round normally; run invariants and start
    -- the next round.
    ContinueRound
  | -- | The round ends the whole test case; (re)throw this exception.
    Conclude !SomeException

-- | Resolve one round's worker outcomes by precedence.
--
-- A control error outranks an overrun or an invalid conclusion, which outrank
-- a panic; among several panics, the lowest-indexed worker's wins.
--
-- Returns the panics that lost, for the caller to note rather than drop
-- silently.
resolveRound :: [(Int, WorkerOutcome)] -> (RoundVerdict, [(Int, SomeException)])
resolveRound outcomes = case [e | (_, RoundControlError e) <- outcomes] of
  e : _ -> (Conclude e, panics)
  [] ->
    if anyOverrun || anyInvalid
      then (Conclude (if anyOverrun then toException TestStopped else toException AssumeRejected), panics)
      else case panics of
        (_, e) : rest -> (Conclude e, rest)
        [] -> (ContinueRound, [])
  where
    anyOverrun = any (isOverrun . snd) outcomes
    anyInvalid = any (isInvalid . snd) outcomes
    isOverrun RoundOverrun = True
    isOverrun _ = False
    isInvalid RoundInvalid = True
    isInvalid _ = False
    panics = sortOn fst [(i, e) | (i, RoundPanicked e) <- outcomes]

-- * Fan-out

-- | Run one round.
runRound :: Ptr HegelStateMachine -> [Worker] -> IO (RoundVerdict, [(Int, SomeException)])
runRound sm workers = mask \restore -> do
  handles <- for (zip [0 ..] workers) \(i, w) -> (,) i <$> Async.async (runWorkerRound sm w i)
  outcomes <-
    restore (for handles (\(i, h) -> (,) i <$> waitWorkerOutcome h))
      `onException` traverse_ (Async.uninterruptibleCancel . snd) handles
  pure (resolveRound outcomes)

-- | Wait for one worker's round to finish, classifying however it ended.
waitWorkerOutcome :: Async () -> IO WorkerOutcome
waitWorkerOutcome h =
  Async.waitCatch h >>= \case
    Right () -> pure RoundDone
    Left e -> pure (classifyWorkerOutcome e)

-- * Reporting

-- | Resolve a registered rule, throwing 'InvariantViolation' for an unknown index.
lookupRule :: String -> Int -> [(Int, a)] -> a
lookupRule caller ruleIndex indexedRules = case lookup ruleIndex indexedRules of
  Just r -> r
  Nothing ->
    throw
      InvariantViolation
        { detail =
            T.pack caller
              <> ": unknown rule index "
              <> T.pack (show ruleIndex)
              <> " for "
              <> T.pack (show (length indexedRules))
              <> " registered rules"
        }

-- | The display string for a stateful step's header:
--
-- e.g. @\"Step 4: restock\"@.
stepText :: Int -> Text -> Text
stepText idx ruleName = "Step " <> T.pack (show idx) <> ": " <> ruleName
