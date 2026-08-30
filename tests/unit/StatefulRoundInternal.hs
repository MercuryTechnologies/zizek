-- | Unit tests for 'Hegel.Internal.StatefulRound', the round-based
-- state-machine protocol generalized over any number of workers.
--
-- 'Hegel.Stateful.run' only ever drives this module with one worker, so the
-- multi-worker fan-out ('fanOutSpec' below) is exercised only here, against
-- a state machine created with a real concurrency level above 1 via
-- 'Hegel.Internal.DataSource.newConcurrentStateMachine'.
module StatefulRoundInternal (spec) where

import Control.Concurrent.MVar (modifyMVar_, newMVar, readMVar)
import Control.Exception (ErrorCall (..), SomeException, fromException, toException)
import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (def)
import Data.Int (Int64)
import Data.Set qualified as Set
import Hegel.Internal.Control (AssumeRejected (..), TestStopped (..))
import Hegel.Internal.DataSource (freeStateMachine, newConcurrentStateMachine, stateMachineNextGroup)
import Hegel.Internal.Foreign.Raw (HegelError (..))
import Hegel.Internal.StatefulRound
  ( RoundVerdict (..),
    Worker (..),
    WorkerOutcome (..),
    classifyWorkerOutcome,
    resolveRound,
    runRound,
  )
import Hegel.Internal.TestCase (withClones)
import Hegel.Property (registerFinalizer)
import Hegel.Property.Internal (Env (..), askEnv)
import Hegel.Report (Report (..), Result (..))
import Hegel.Runner (check)
import Hegel.Settings (Settings (..))
import Test.Hspec
import UnliftIO.IORef (atomicModifyIORef', newIORef, readIORef)

isOk :: Result -> Bool
isOk = \case
  Ok -> True
  _ -> False

boom :: SomeException
boom = toException (ErrorCall "boom")

anotherHegelError :: SomeException
anotherHegelError = toException HegelError {code = 2, message = Nothing}

spec :: Spec
spec = do
  classifySpec
  resolveSpec
  fanOutSpec

-- ---------------------------------------------------------------------------
-- classifyWorkerOutcome

classifySpec :: Spec
classifySpec = describe "classifyWorkerOutcome" do
  it "classifies AssumeRejected as RoundInvalid" do
    case classifyWorkerOutcome (toException AssumeRejected) of
      RoundInvalid -> pure ()
      _ -> expectationFailure "expected RoundInvalid"

  it "classifies TestStopped as RoundOverrun" do
    case classifyWorkerOutcome (toException TestStopped) of
      RoundOverrun -> pure ()
      _ -> expectationFailure "expected RoundOverrun"

  it "classifies a HegelError as RoundControlError" do
    case classifyWorkerOutcome (toException HegelError {code = 1, message = Nothing}) of
      RoundControlError _ -> pure ()
      _ -> expectationFailure "expected RoundControlError"

  it "classifies anything else as RoundPanicked" do
    case classifyWorkerOutcome boom of
      RoundPanicked _ -> pure ()
      _ -> expectationFailure "expected RoundPanicked"

-- ---------------------------------------------------------------------------
-- resolveRound

resolveSpec :: Spec
resolveSpec = describe "resolveRound" do
  it "continues when every worker finished its round normally" do
    let (verdict, dropped) = resolveRound [(0, RoundDone), (1, RoundDone)]
    case verdict of
      ContinueRound -> pure ()
      Conclude _ -> expectationFailure "expected ContinueRound"
    map fst dropped `shouldBe` ([] :: [Int])

  it "concludes with TestStopped when any worker's budget ran out" do
    let (verdict, _dropped) = resolveRound [(0, RoundDone), (1, RoundOverrun)]
    case verdict of
      Conclude e | Just TestStopped <- fromException e -> pure ()
      _ -> expectationFailure "expected Conclude TestStopped"

  it "prefers an overrun over an invalid conclusion in the same round" do
    let (verdict, _dropped) = resolveRound [(0, RoundInvalid), (1, RoundOverrun)]
    case verdict of
      Conclude e | Just TestStopped <- fromException e -> pure ()
      _ -> expectationFailure "expected Conclude TestStopped"

  it "concludes with AssumeRejected when the control draw rejected and nothing overran" do
    let (verdict, _dropped) = resolveRound [(0, RoundInvalid), (1, RoundDone)]
    case verdict of
      Conclude e | Just AssumeRejected <- fromException e -> pure ()
      _ -> expectationFailure "expected Conclude AssumeRejected"

  it "drops every panic once an overrun or invalid conclusion also occurred" do
    let (verdict, dropped) = resolveRound [(0, RoundPanicked boom), (1, RoundOverrun)]
    case verdict of
      Conclude e | Just TestStopped <- fromException e -> pure ()
      _ -> expectationFailure "expected Conclude TestStopped"
    map fst dropped `shouldBe` [0]

  it "lets a control error outrank an overrun and every panic" do
    let (verdict, dropped) =
          resolveRound [(0, RoundPanicked boom), (1, RoundOverrun), (2, RoundControlError anotherHegelError)]
    case verdict of
      Conclude e | Just (HegelError {code}) <- fromException e -> code `shouldBe` 2
      _ -> expectationFailure "expected Conclude the control error"
    map fst dropped `shouldBe` [0]

  it "picks the lowest-indexed panic and drops the rest, regardless of input order" do
    let e0 = toException (ErrorCall "zero")
        e1 = toException (ErrorCall "one")
        (verdict, dropped) = resolveRound [(1, RoundPanicked e1), (0, RoundPanicked e0)]
    case verdict of
      Conclude e | Just (ErrorCall msg) <- fromException e -> msg `shouldBe` "zero"
      _ -> expectationFailure "expected Conclude the lowest-indexed panic"
    map fst dropped `shouldBe` [1]

-- ---------------------------------------------------------------------------
-- Real multi-worker fan-out

-- | Drive a genuinely concurrent state machine's round protocol directly,
-- bypassing 'Hegel.Stateful.run' and 'Hegel.Stateful.Rule' entirely: this
-- test's whole point is to exercise 'runRound' with more than one worker,
-- which no public entry point can reach yet.
fanOutSpec :: Spec
fanOutSpec = describe "runRound (real engine, several workers)" do
  it "distributes rule dispatches across multiple workers with no lost or duplicated increment" do
    counter <- newIORef (0 :: Int)
    dispatches <- newIORef (0 :: Int)
    seenWorkers <- newMVar Set.empty
    let n = 3 :: Int64
    report <- check def {testCases = 5, statefulStepCount = 300} do
      env <- askEnv
      let tc = env.testCase
      (sm, _concurrency) <- liftIO (newConcurrentStateMachine tc ["increment"] [0] [] n n)
      registerFinalizer (freeStateMachine tc sm)
      liftIO $ withClones (fromIntegral n) tc \clones -> do
        let mkWorker i clone =
              Worker
                { testCase = clone,
                  dispatch = \_ruleIndex -> do
                    atomicModifyIORef' counter \c -> (c + 1, ())
                    atomicModifyIORef' dispatches \d -> (d + 1, ())
                    modifyMVar_ seenWorkers (pure . Set.insert i),
                  onRejected = pure ()
                }
            workers = zipWith mkWorker [0 :: Int ..] clones
            roundLoop = do
              mGroup <- stateMachineNextGroup tc sm
              case mGroup of
                Nothing -> pure ()
                Just _groupId -> do
                  -- The rule body never throws, so 'Conclude' is unreachable
                  -- here; only 'ContinueRound' is expected until the machine
                  -- reports done.
                  (verdict, _dropped) <- runRound sm workers
                  case verdict of
                    ContinueRound -> roundLoop
                    Conclude _ -> pure ()
        roundLoop
    -- The very first max_concurrency > 1 state-machine creation in this
    -- whole run discards as an ordinary invalid case (the engine's own
    -- documented way of flipping the run nondeterministic); every case
    -- after that runs with real concurrency. Nothing here ever fails, so
    -- the run reports 'Ok' either way.
    report.result `shouldSatisfy` isOk
    finalCounter <- readIORef counter
    finalDispatches <- readIORef dispatches
    finalCounter `shouldBe` finalDispatches
    workers <- readMVar seenWorkers
    Set.size workers `shouldSatisfy` (>= 2)
