-- | Unit tests for 'Hegel.Pool' and 'Hegel.Stateful'.
module Stateful (spec) where

import Control.Exception (fromException)
import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (def)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Hegel (Gen)
import Hegel.Exception (Diagnostic (..), MalformedTest (..))
import Hegel.Gen qualified as Gen
import Hegel.HealthCheck (HealthCheck (..))
import Hegel.Pool (Pool)
import Hegel.Pool qualified as Pool
import Hegel.Property (assert, assume, forAll, forAllSilent)
import Hegel.Report (Abort (..), FailureEvidence (..), FailureOutcome (..), Note (..), NoteKind (..), Report (..), Result (..), isFailureNote, renderReportRich)
import Hegel.Runner (check, replay)
import Hegel.Seed (Seed (..))
import Hegel.Settings (Settings (..))
import Hegel.Stateful qualified as Stateful
import Test.Hspec
import TestSupport (allFailureOutcomes, expectCaptured, expectToken, singleCapturedEvidence)

-- ---------------------------------------------------------------------------
-- Helpers

intGen :: Gen Int
intGen = Gen.int & Gen.min 0 & Gen.max 100 & Gen.build

-- | A counter model used by several tests.
newtype Counter = Counter Int

increment :: Stateful.Rule Counter IO
increment =
  Stateful.rule "increment" \(Counter n) ->
    pure (Counter (n + 1))

-- | Run a single-rule machine and return the number of steps each test case
-- took, mirroring the Rust reference's @run_step_recorder@.
--
-- With @failAssumption@ set the rule rejects via 'assume' on every step, so
-- the model never advances though every dispatch still counts.
stepRecorder :: Bool -> Int -> Settings -> IO [Int]
stepRecorder failAssumption steps settings = do
  perCase <- newIORef ([] :: [Int])
  let bump =
        atomicModifyIORef' perCase \case
          (top : rest) -> (top + 1 : rest, ())
          [] -> ([1], ())
      recording :: Stateful.Rule Counter IO
      recording =
        Stateful.rule "step" \s -> do
          liftIO bump
          when failAssumption (assume False)
          pure s
      machine =
        Stateful.Machine
          { initial = do
              liftIO (atomicModifyIORef' perCase \cs -> (0 : cs, ()))
              pure (Counter 0),
            rules = [recording],
            stepCount = steps,
            invariants = []
          }
  _ <- check settings (Stateful.run machine)
  readIORef perCase

-- | A deliberately correct invariant.
alwaysNonNegative :: Stateful.Invariant Counter IO
alwaysNonNegative =
  Stateful.invariant "always_non_negative" \(Counter n) ->
    assert (n >= 0) "counter is non-negative"

-- | A deliberately violated invariant: triggers once counter exceeds 5.
neverAboveFive :: Stateful.Invariant Counter IO
neverAboveFive =
  Stateful.invariant "never_above_five" \(Counter n) ->
    assert (n <= 5) "counter does not exceed 5"

-- | A stack model whose rules draw values, so a counterexample only reproduces
-- when the replayed choice sequence stays aligned.
newtype Stack = Stack [Int]

pushValue :: Gen Int
pushValue = Gen.int & Gen.min (-100) & Gen.max 100 & Gen.build

push :: Stateful.Rule Stack IO
push =
  Stateful.rule "push" \(Stack xs) -> do
    n <- forAll pushValue
    pure (Stack (n : xs))

-- | Draws a value and asserts it is zero — a bug that fails for any nonzero
-- draw. The counterexample therefore depends on a specific drawn value.
pushNonZeroBug :: Stateful.Rule Stack IO
pushNonZeroBug =
  Stateful.rule "push_nonzero_bug" \(Stack xs) -> do
    n <- forAll pushValue
    assert (n == 0) "drawn value is zero (bug)"
    pure (Stack (n : xs))

-- ---------------------------------------------------------------------------
-- Pool tests

poolSpec :: Spec
poolSpec = describe "Pool" do
  it "empty pool draw is Invalid, not Interesting" do
    report <- check def do
      pool <- Pool.new
      -- Immediately draw from an empty pool → AssumeRejected → Invalid.
      _ <- forAllSilent (Pool.reuse pool)
      assert False "should not be reached"
    -- Every case is discarded, so we expect GaveUp (all Invalid), never a failure.
    case report.result of
      GaveUp _ -> pure ()
      Failures _ -> expectationFailure "expected GaveUp, got a failure"
      other -> expectationFailure ("expected GaveUp (all invalid), got: " <> show other)

  it "reuse returns an added value without removing it" do
    report <- check def do
      pool <- Pool.new
      n <- forAll intGen
      Pool.add pool n
      a <- forAll (Pool.reuse pool)
      b <- forAll (Pool.reuse pool)
      assert (a == n && b == n) "reusable draw returns the added value each time"
    report.result `shouldSatisfy` \case
      Ok -> True
      _ -> False

  it "consume returns and removes the value" do
    report <- check def do
      pool <- Pool.new
      n <- forAll intGen
      Pool.add pool n
      v <- forAll (Pool.consume pool)
      assert (v == n) "consumed value matches what was added"
      empty <- liftIO (Pool.isEmpty pool)
      assert empty "pool is empty after consuming the only value"
    report.result `shouldSatisfy` \case
      Ok -> True
      _ -> False

-- ---------------------------------------------------------------------------
-- Stateful machine tests

statefulSpec :: Spec
statefulSpec = describe "Machine" do
  it "trivial machine passes" do
    let machine =
          Stateful.Machine
            { initial = pure (Counter 0),
              rules = [increment],
              stepCount = Stateful.defaultStepCount,
              invariants = [alwaysNonNegative]
            }
    report <- check def (Stateful.run machine)
    report.result `shouldSatisfy` \case
      Ok -> True
      _ -> False

  it "buggy machine finds a counterexample" do
    let machine =
          Stateful.Machine
            { initial = pure (Counter 0),
              rules = [increment],
              stepCount = Stateful.defaultStepCount,
              invariants = [neverAboveFive]
            }
    report <- check def (Stateful.run machine)
    evidence <- expectCaptured report.result
    evidence.message `shouldBe` "counter does not exceed 5"

  it "machinery annotations carry no source location" do
    -- The 'Step N: ...' / invariant-check annotations are emitted by
    -- 'Stateful.run' itself; a call-stack loc would point inside
    -- @library/Hegel/Stateful.hs@, which the rich renderer would then try
    -- to splice into the report as if it were the user's test source.
    let machine =
          Stateful.Machine
            { initial = pure (Counter 0),
              rules = [increment],
              stepCount = Stateful.defaultStepCount,
              invariants = [neverAboveFive]
            }
    report <- check def (Stateful.run machine)
    case singleCapturedEvidence report.result of
      Just FailureEvidence {notes} -> do
        let machinery = [n | n <- notes, isMachinery n.kind]
            isMachinery = \case
              Annotation -> True
              StepHeader _ _ -> True
              _ -> False
        machinery `shouldNotSatisfy` null
        [n | n <- machinery, StepHeader _ _ <- [n.kind]] `shouldNotSatisfy` null
        machinery `shouldSatisfy` all (isNothing . (.loc))
      other -> expectationFailure ("expected failure, got: " <> show other)

  it "journals the failing assertion in-band as a nested Failure note" do
    -- End-to-end: the caught failure is journaled in-band and still re-thrown,
    -- so the runner reports a counterexample.
    let machine =
          Stateful.Machine
            { initial = pure (Counter 0),
              rules = [increment],
              stepCount = Stateful.defaultStepCount,
              invariants = [neverAboveFive]
            }
    report <- check def (Stateful.run machine)
    case singleCapturedEvidence report.result of
      Just FailureEvidence {notes} ->
        case filter isFailureNote notes of
          [f] -> do
            f.text `shouldBe` "counter does not exceed 5"
            f.depth `shouldBe` 1
            f.loc `shouldSatisfy` (not . isNothing)
          fs -> expectationFailure ("expected exactly one Failure note, got: " <> show (length fs))
      other -> expectationFailure ("expected failure, got: " <> show other)

  it "rich report splices the failing invariant's source" do
    -- End-to-end through 'renderReportRich': the failing step's notes splice
    -- into this file's declarations (requires cwd = repo root, as under
    -- `just test`).
    let machine =
          Stateful.Machine
            { initial = pure (Counter 0),
              rules = [increment],
              stepCount = Stateful.defaultStepCount,
              invariants = [neverAboveFive]
            }
    report <- check def (Stateful.run machine)
    rich <- renderReportRich report
    -- The invariant's assert line (in 'neverAboveFive') is spliced.
    ("assert (n <= 5)" `T.isInfixOf` rich) `shouldBe` True
    ("┏━━ tests/unit/Stateful.hs" `T.isInfixOf` rich) `shouldBe` True

  it "value-drawing counterexample reproduces on replay" do
    -- Multiple drawing rules require choice alignment during replay.
    let machine =
          Stateful.Machine
            { initial = pure (Stack []),
              rules = [push, pushNonZeroBug],
              stepCount = Stateful.defaultStepCount,
              invariants = []
            }
    report <- check def (Stateful.run machine)
    evidence <- expectCaptured report.result
    evidence.notes `shouldNotSatisfy` null

  it "machine with no rules is aborted, not reported as a counterexample" do
    let machine :: Stateful.Machine Counter IO
        machine =
          Stateful.Machine
            { initial = pure (Counter 0),
              rules = [],
              stepCount = Stateful.defaultStepCount,
              invariants = []
            }
    report <- check def (Stateful.run machine)
    case report.result of
      Aborted _ -> pure ()
      other -> expectationFailure ("expected Aborted, got: " <> show other)

  it "machine with a stepCount below 1 is aborted" do
    let machine :: Stateful.Machine Counter IO
        machine =
          Stateful.Machine
            { initial = pure (Counter 0),
              rules = [increment],
              stepCount = 0,
              invariants = []
            }
    report <- check def (Stateful.run machine)
    case report.result of
      Aborted _ -> pure ()
      other -> expectationFailure ("expected Aborted, got: " <> show other)

  it "a rule weight that is not finite and positive is a malformed test" do
    for_ [0, -1, 0 / 0, 1 / 0] \w -> do
      let machine :: Stateful.Machine Counter IO
          machine =
            Stateful.Machine
              { initial = pure (Counter 0),
                rules = [increment & Stateful.weighted w],
                stepCount = Stateful.defaultStepCount,
                invariants = []
              }
      report <- check def (Stateful.run machine)
      case report.result of
        Aborted (Errored e) | Just (MalformedTest d) <- fromException e -> do
          d.detail `shouldBe` "a Rule's weight must be finite and positive"
          lookup "weight" d.values `shouldBe` Just (T.pack (show w))
        other -> expectationFailure ("expected a malformed-test abort, got: " <> show other)

  it "a heavier rule is dispatched more often" do
    counts <- newIORef (0 :: Int, 0 :: Int)
    let heavy, light :: Stateful.Rule Counter IO
        heavy = Stateful.rule "heavy" \s -> liftIO (atomicModifyIORef' counts \(h, l) -> ((h + 1, l), ())) >> pure s
        light = Stateful.rule "light" \s -> liftIO (atomicModifyIORef' counts \(h, l) -> ((h, l + 1), ())) >> pure s
        machine =
          Stateful.Machine
            { initial = pure (Counter 0),
              rules = [heavy & Stateful.weighted 1000, light],
              stepCount = Stateful.defaultStepCount,
              invariants = []
            }
    report <- check def {testCases = Just 100, seed = Just (SeedFixed 7)} (Stateful.run machine)
    case report.result of
      Ok -> pure ()
      other -> expectationFailure ("expected Ok, got: " <> show other)
    (h, l) <- readIORef counts
    h `shouldSatisfy` (> 2 * l)

  it "an overrun inside a rule's draw is a health-check abort, not a fabricated counterexample" do
    let overrunning :: Stateful.Rule Counter IO
        overrunning =
          Stateful.rule "overrun" \s -> do
            _ <- forever (forAll intGen >> pure ())
            pure s
        machine =
          Stateful.Machine
            { initial = pure (Counter 0),
              rules = [overrunning],
              stepCount = Stateful.defaultStepCount,
              invariants = []
            }
    report <-
      check
        def {testCases = Just 5, suppressHealthCheck = Just [LargeInitialTestCase]}
        (Stateful.run machine)
    case report.result of
      Aborted (UnhealthyInput msg) -> T.unpack msg `shouldContain` "TestCasesTooLarge"
      other -> expectationFailure ("expected a TestCasesTooLarge abort, got: " <> show other)

  it "the default stepCount bounds steps, and most cases hit it exactly" do
    -- Analogue of the Rust reference's test_step_cap_is_50_most_of_the_time.
    counts <- stepRecorder False Stateful.defaultStepCount def {testCases = Just 30}
    counts `shouldSatisfy` all (\c -> c >= 1 && c <= 50)
    length (filter (== 50) counts) `shouldSatisfy` (> length counts `div` 2)

  it "an assume-rejecting rule's attempts are still bounded" do
    -- Analogue of the Rust reference's test_hopeless_machine_attempts_are_bounded.
    -- A rejected rule is reported to the engine and does not count toward
    -- the machine's stepCount, so a machine whose rule never gets
    -- past its precondition is bounded by the engine's separate 1000-attempt
    -- cap on a case with no successful rule, rather than the step cap.
    counts <- stepRecorder True Stateful.defaultStepCount def {testCases = Just 10}
    counts `shouldSatisfy` all (\c -> c >= 1 && c <= 1000)
    length (filter (== 1000) counts) `shouldSatisfy` (> length counts `div` 2)

  it "stepCount replaces the default cap" do
    -- Analogue of the Rust reference's test_stateful_step_count_setting_bounds_steps.
    let n = 7 :: Int
    counts <- stepRecorder False n def {testCases = Just 30}
    counts `shouldSatisfy` all (\c -> c >= 1 && c <= n)
    length (filter (== n) counts) `shouldSatisfy` (> length counts `div` 2)

-- ---------------------------------------------------------------------------
-- Pool + Machine integration

-- | Model carrying an engine pool plus a mirror of every value added to it, so
-- rules can assert that pool draws only ever return previously-registered
-- values.
data Model = Model
  { pool :: Pool Int,
    registered :: Set Int
  }

-- | Draw a value and add it to the pool, recording it in the mirror.
register :: Stateful.Rule Model IO
register =
  Stateful.rule "register" \m -> do
    n <- forAll intGen
    Pool.add m.pool n
    pure m {registered = Set.insert n m.registered}

-- | Draw a value from the pool without removing it; it must be one we added.
useReusable :: Stateful.Rule Model IO
useReusable =
  Stateful.rule "use_reusable" \m -> do
    v <- forAll (Pool.reuse m.pool)
    assert (Set.member v m.registered) "reusable draw was previously registered"
    pure m

-- | Consume a value from the pool; it must be one we added.
useConsumed :: Stateful.Rule Model IO
useConsumed =
  Stateful.rule "use_consumed" \m -> do
    v <- forAll (Pool.consume m.pool)
    assert (Set.member v m.registered) "consumed draw was previously registered"
    pure m

poolMachine :: Stateful.Machine Model IO
poolMachine =
  Stateful.Machine
    { initial = do
        p <- Pool.new
        pure (Model p Set.empty),
      rules = [register, useReusable, useConsumed],
      stepCount = Stateful.defaultStepCount,
      invariants = []
    }

poolMachineSpec :: Spec
poolMachineSpec = describe "Pool + Machine" do
  it "values added in one rule are drawn back in another" do
    report <- check def (Stateful.run poolMachine)
    report.result `shouldSatisfy` \case
      Ok -> True
      _ -> False

-- ---------------------------------------------------------------------------
-- Spec root

spec :: Spec
spec = do
  poolSpec
  statefulSpec
  invariantSamplingSpec
  poolMachineSpec

-- ---------------------------------------------------------------------------
-- Invariant sampling

-- | Per test case, how many steps ran and how many times each of an
-- always-run and a sampled invariant was checked, newest case first.
data Checks = Checks {steps :: !Int, always :: !Int, sampled :: !Int}
  deriving stock (Show)

invariantCadence :: IO [Checks]
invariantCadence = do
  perCase <- newIORef ([] :: [Checks])
  let bump f = atomicModifyIORef' perCase \case
        (top : rest) -> (f top : rest, ())
        [] -> ([], ())
      stepping :: Stateful.Rule Counter IO
      stepping =
        Stateful.rule "step" \(Counter n) -> do
          liftIO (bump \c -> c {steps = c.steps + 1})
          pure (Counter (n + 1))
      machine =
        Stateful.Machine
          { initial = do
              liftIO (atomicModifyIORef' perCase \cs -> (Checks 0 0 0 : cs, ()))
              pure (Counter 0),
            rules = [stepping],
            stepCount = 20,
            invariants =
              [ Stateful.alwaysInvariant "every_join_point" \_ -> liftIO (bump \c -> c {always = c.always + 1}),
                Stateful.invariant "sampled" \_ -> liftIO (bump \c -> c {sampled = c.sampled + 1})
              ]
          }
  report <- check def {testCases = Just 50} (Stateful.run machine)
  report.result `shouldSatisfy` \case
    Ok -> True
    _ -> False
  readIORef perCase

invariantSamplingSpec :: Spec
invariantSamplingSpec = describe "Invariant sampling" do
  it "a sampled invariant shrinks a persistent violation to the minimal counterexample" do
    let machine =
          Stateful.Machine
            { initial = pure (Counter 0),
              rules = [increment],
              stepCount = Stateful.defaultStepCount,
              invariants = [neverAboveFive]
            }
    report <- check def (Stateful.run machine)
    evidence <- expectCaptured report.result
    evidence.message `shouldBe` "counter does not exceed 5"
    length [n | n <- evidence.notes, StepHeader _ _ <- [n.kind]] `shouldBe` 6

  it "an always-run invariant is checked at every join point plus the initial and final states" do
    cases <- invariantCadence
    cases `shouldNotSatisfy` null
    -- A sequential machine runs one rule per round, so each step is followed
    -- by exactly one join point.
    cases `shouldSatisfy` all (\c -> c.always == c.steps + 2)

  it "a sampled invariant is checked on the initial and final states, and less often in between" do
    cases <- invariantCadence
    cases `shouldSatisfy` all (\c -> c.sampled >= 2 && c.sampled <= c.always)
    sum (map (.sampled) cases) `shouldSatisfy` (< sum (map (.always) cases))

  it "a drawing sampled invariant's counterexample reproduces on replay" do
    -- The invariant draws on every check, so its draws interleave with the
    -- engine's sampling decisions; replay only reproduces when both stay
    -- aligned.
    let drawingInvariant :: Stateful.Invariant Counter IO
        drawingInvariant =
          Stateful.invariant "drawing_never_above_three" \(Counter n) -> do
            _ <- forAll intGen
            assert (n <= 3) "counter does not exceed 3"
        machine =
          Stateful.Machine
            { initial = pure (Counter 0),
              rules = [increment],
              stepCount = Stateful.defaultStepCount,
              invariants = [drawingInvariant]
            }
    report <- check def (Stateful.run machine)
    evidence <- expectCaptured report.result
    token <- case allFailureOutcomes report.result of
      [outcome] -> expectToken outcome
      other -> fail ("expected one failure, got " <> show (length other))
    replayed <- replay def token (Stateful.run machine)
    actual <- expectCaptured replayed.result
    actual.message `shouldBe` evidence.message
    map (.failureReplayToken) (allFailureOutcomes replayed.result) `shouldBe` [Just token]
