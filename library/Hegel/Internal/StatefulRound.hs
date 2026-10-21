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

    -- * Join points
    selectInvariants,

    -- * Reporting
    lookupRule,
    stepText,

    -- * Validation
    checkRuleWeights,
  )
where

import Control.Exception (SomeException, fromException, mask, onException, throw, throwIO, toException)
import Control.Monad (filterM)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable (for)
import Foreign (Ptr)
import Hegel.Exception (InvariantViolation (..))
import Hegel.Internal.Control (AssumeRejected (..), ControlSignal (Assume, Stop), TestStopped (..), catchControl, isAborting, malformedTest)
import Hegel.Internal.DataSource (Label (LabelStatefulRule), discardSpansTo, openSpanDepth, spanLabel, startSpan, stateMachineNextRule, stateMachineRuleRejected, stateMachineShouldCheckInvariant, stopSpan)
import Hegel.Internal.Foreign.Raw (HegelStateMachine)
import Hegel.Internal.TestCase (TestCase)
import Hegel.Internal.TestCase qualified as TestCase
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
    roundSpan :: !RoundSpan,
    restoresRejected :: !Bool
  }

-- | Pull rules for one worker from @sm@ until the round's own budget is
-- exhausted (the join point) or a terminal outcome ends it early.
runWorkerRound :: Ptr HegelStateMachine -> Worker -> Int -> IO ()
runWorkerRound sm w workerIndex = case w.roundSpan of
  Own -> do
    startSpan w.testCase (spanLabel LabelStatefulRule)
    loop False `onException` stopSpan w.testCase False
  Caller -> loop False
  where
    -- A rejected rule's undo actions run inside 'TestCase.undoOnReject' before
    -- its 'AssumeRejected' reaches the handler below.
    attempt
      | w.restoresRejected = TestCase.undoOnReject w.testCase
      | otherwise = id
    loop rejected = do
      mRuleIndex <- stateMachineNextRule w.testCase sm workerIndex
      case mRuleIndex of
        Nothing -> case w.roundSpan of
          Own -> stopSpan w.testCase rejected
          Caller -> pure ()
        Just ruleIndex -> do
          depth <- openSpanDepth w.testCase
          verdict <- (fmap Right . attempt $ w.dispatch ruleIndex) `catchControl` (pure . Left)
          case verdict of
            Right () -> loop rejected
            -- A rejected rule unwinds out of whatever draws it was making, so
            -- their spans are closed here before the round span is.
            Left Assume -> do
              discardSpansTo w.testCase depth
              stateMachineRuleRejected w.testCase sm workerIndex
              w.onRejected
              loop True
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
  = -- | Every worker finished its round normally; check invariants and start
    -- the next round.
    ContinueRound
  | -- | The round ends the whole test case; (re)throw this exception.
    Conclude !SomeException

-- | Resolve one round's worker outcomes by precedence.
--
-- A control error outranks an overrun or an invalid conclusion, which outrank
-- a panic; among several panics, the lowest-indexed worker's is returned.
--
-- Returns the remaining panics (if any).
resolveRound :: [(Int, WorkerOutcome)] -> (RoundVerdict, [(Int, SomeException)])
resolveRound outcomes
  | e : _ <- controlErrors = (Conclude e, panics)
  | any (isOverrun . snd) outcomes = (Conclude (toException TestStopped), panics)
  | any (isInvalid . snd) outcomes = (Conclude (toException AssumeRejected), panics)
  | (_, e) : rest <- panics = (Conclude e, rest)
  | otherwise = (ContinueRound, [])
  where
    controlErrors = [e | (_, RoundControlError e) <- outcomes]
    panics = sortOn fst [(i, e) | (i, RoundPanicked e) <- outcomes]
    isOverrun = \case RoundOverrun -> True; _ -> False
    isInvalid = \case RoundInvalid -> True; _ -> False

-- * Fan-out

-- | Run one round.
runRound :: Ptr HegelStateMachine -> [Worker] -> IO (RoundVerdict, [(Int, SomeException)])
runRound sm workers = mask \restore -> do
  handles <- for (zip [0 ..] workers) \(i, w) -> (,) i <$> Async.async (runWorkerRound sm w i)
  outcomes <-
    restore (for handles \(i, h) -> (,) i <$> waitWorkerOutcome h)
      `onException` traverse_ (Async.uninterruptibleCancel . snd) handles
  pure $ resolveRound outcomes

-- | Wait for one worker's round to finish, classifying however it ended.
waitWorkerOutcome :: Async () -> IO WorkerOutcome
waitWorkerOutcome h =
  Async.waitCatch h <&> \case
    Right () -> RoundDone
    Left e -> classifyWorkerOutcome e

-- * Join points

-- | The invariants to run at the join point the root test case @tc@ has just
-- reached, in registration order.
--
-- The engine is asked about every invariant before any of them runs, so call
-- this exactly once per join point on generation and replay alike.
selectInvariants :: TestCase -> Ptr HegelStateMachine -> [a] -> IO [a]
selectInvariants tc sm invariants =
  map snd <$> filterM (\(i, _) -> stateMachineShouldCheckInvariant tc sm i) (zip [0 ..] invariants)

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

-- * Validation

-- | Throw a malformed-test error naming @context@ for the first rule whose
-- weight is not finite and positive.
checkRuleWeights :: Text -> [(Text, Double)] -> IO ()
checkRuleWeights context rules =
  case filter (not . validWeight . snd) rules of
    [] -> pure ()
    (name, w) : _ ->
      let message :: Text
          message = "a Rule's weight must be finite and positive"
          fields = [("rule", name), ("weight", T.pack $ show w)]
       in throwIO $ malformedTest context message fields
  where
    validWeight :: Double -> Bool
    validWeight w = not (isNaN w || isInfinite w) && w > 0
