-- | The engine's round-based state-machine protocol, generalized over any
-- number of workers.
module Hegel.Internal.StatefulRound
  ( -- * Workers
    Worker (..),
    runWorkerRound,

    -- * Round resolution
    WorkerOutcome (..),
    classifyWorkerOutcome,
    RoundVerdict (..),
    resolveRound,

    -- * Fan-out
    runRound,
  )
where

import Control.Exception (SomeException, fromException, mask, onException, throwIO, toException)
import Data.Foldable (traverse_)
import Data.List (sortOn)
import Data.Traversable (for)
import Foreign (Ptr)
import Hegel.Internal.Control (AssumeRejected (..), ControlSignal (Assume, Stop), TestStopped (..), catchControl)
import Hegel.Internal.DataSource (stateMachineNextRule, stateMachineRuleRejected)
import Hegel.Internal.Foreign.Raw (HegelError, HegelStateMachine)
import Hegel.Internal.TestCase (TestCase)
import UnliftIO.Async (Async)
import UnliftIO.Async qualified as Async

-- * Workers

-- | One worker's connection into a round: its own test case to draw against,
-- how to apply the rule at a given index, and what to do once a rejected
-- rule has been reported to the engine.
data Worker = Worker
  { testCase :: !TestCase,
    dispatch :: !(Int -> IO ()),
    onRejected :: !(IO ())
  }

-- | Pull rules for one worker from @sm@ until the round's own budget is
-- exhausted (the join point) or a terminal outcome ends it early.
runWorkerRound :: Ptr HegelStateMachine -> Worker -> Int -> IO ()
runWorkerRound sm w workerIndex = loop
  where
    loop = do
      mRuleIndex <- stateMachineNextRule w.testCase sm workerIndex
      case mRuleIndex of
        Nothing -> pure ()
        Just ruleIndex -> do
          verdict <- (Right <$> w.dispatch ruleIndex) `catchControl` (pure . Left)
          case verdict of
            Right () -> loop
            Left Assume -> stateMachineRuleRejected w.testCase sm workerIndex *> w.onRejected *> loop
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
  | Just he <- fromException @HegelError e = RoundControlError (toException he)
  | otherwise = RoundPanicked e

-- | What a round concludes to, once every worker has reported in.
data RoundVerdict
  = -- | Every worker finished its round normally; run invariants and start
    -- the next round.
    ContinueRound
  | -- | The round ends the whole test case; (re)throw this exception.
    Conclude !SomeException

-- | Resolve one round's worker outcomes by precedence:
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

-- | Run one round: fan every worker's 'runWorkerRound' out concurrently, wait
-- for each to finish or end early, and resolve the round.
--
-- Every worker's outcome is classified rather than let escape, so an
-- external async exception arriving while this waits is the only thing that
-- can leave a worker still running once 'runRound' returns; that path
-- cancels every handle before propagating, so no worker thread survives a
-- call to this function. Spawning itself runs 'mask'ed so the same exception
-- can't land between two 'Async.async' calls and strand an already-spawned
-- worker outside the handle list the wait's own guard cancels.
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
