-- | Unit tests for 'Hegel.Pool' and 'Hegel.Stateful'.
module Stateful (spec) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (def)
import Data.Function ((&))
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Hegel (Gen)
import Hegel.Gen qualified as Gen
import Hegel.Pool (Pool)
import Hegel.Pool qualified as Pool
import Hegel.Property (assert, assume, forAll, forAllSilent)
import Hegel.Report (Abort (..), Note (..), NoteKind (..), Report (..), Result (..), isFailureNote, renderReportRich)
import Hegel.Runner (check)
import Hegel.Settings (Settings (..))
import Hegel.Stateful qualified as Stateful
import Test.Hspec

-- ---------------------------------------------------------------------------
-- Helpers

intGen :: Gen Int
intGen = Gen.int & Gen.min 0 & Gen.max 100 & Gen.build

-- | A counter model used by several tests.
newtype Counter = Counter Int

increment :: Stateful.Rule Counter IO
increment =
  Stateful.Rule "increment" \(Counter n) ->
    pure (Counter (n + 1))

-- | Run a single-rule machine and return the number of steps each test case
-- took, mirroring the Rust reference's @run_step_recorder@.
--
-- With @failAssumption@ set the rule rejects via 'assume' on every step, so
-- the model never advances though every dispatch still counts.
stepRecorder :: Bool -> Settings -> IO [Int]
stepRecorder failAssumption settings = do
  perCase <- newIORef ([] :: [Int])
  let bump =
        atomicModifyIORef' perCase \case
          (top : rest) -> (top + 1 : rest, ())
          [] -> ([1], ())
      recording :: Stateful.Rule Counter IO
      recording =
        Stateful.Rule "step" \s -> do
          liftIO bump
          when failAssumption (assume False)
          pure s
      machine =
        Stateful.Machine
          { initial = do
              liftIO (atomicModifyIORef' perCase \cs -> (0 : cs, ()))
              pure (Counter 0),
            rules = [recording],
            invariants = []
          }
  _ <- check settings (Stateful.run machine)
  readIORef perCase

-- | A deliberately correct invariant.
alwaysNonNegative :: Stateful.Invariant Counter IO
alwaysNonNegative =
  Stateful.Invariant "always_non_negative" \(Counter n) ->
    assert (n >= 0) "counter is non-negative"

-- | A deliberately violated invariant: triggers once counter exceeds 5.
neverAboveFive :: Stateful.Invariant Counter IO
neverAboveFive =
  Stateful.Invariant "never_above_five" \(Counter n) ->
    assert (n <= 5) "counter does not exceed 5"

-- | A stack model whose rules draw values, so a counterexample only reproduces
-- when the replayed choice sequence stays aligned.
newtype Stack = Stack [Int]

pushValue :: Gen Int
pushValue = Gen.int & Gen.min (-100) & Gen.max 100 & Gen.build

push :: Stateful.Rule Stack IO
push =
  Stateful.Rule "push" \(Stack xs) -> do
    n <- forAll pushValue
    pure (Stack (n : xs))

-- | Draws a value and asserts it is zero — a bug that fails for any nonzero
-- draw. The counterexample therefore depends on a specific drawn value.
pushNonZeroBug :: Stateful.Rule Stack IO
pushNonZeroBug =
  Stateful.Rule "push_nonzero_bug" \(Stack xs) -> do
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
      Counterexample {} -> expectationFailure "expected GaveUp, got a counterexample"
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
              invariants = [neverAboveFive]
            }
    report <- check def (Stateful.run machine)
    case report.result of
      Counterexample {} -> pure ()
      other -> expectationFailure ("expected Counterexample, got: " <> show other)

  it "a counterexample past step 50 under a higher statefulStepCount fails to reconstruct" do
    -- This pins the replay caveat documented on
    -- 'Hegel.Settings.statefulStepCount'. A failure past step 50 under a
    -- higher count diverges on replay instead of reproducing.
    let neverAbove150 :: Stateful.Invariant Counter IO
        neverAbove150 =
          Stateful.Invariant "never_above_150" \(Counter n) ->
            assert (n <= 150) "counter does not exceed 150"
        machine =
          Stateful.Machine
            { initial = pure (Counter 0),
              rules = [increment],
              invariants = [neverAbove150]
            }
    report <- check def {statefulStepCount = 200} (Stateful.run machine)
    case report.result of
      Aborted (ReplayDiverged _) -> pure ()
      other -> expectationFailure ("expected Aborted (ReplayDiverged _), got: " <> show other)

  it "machinery annotations carry no source location" do
    -- The 'Step N: ...' / invariant-check annotations are emitted by
    -- 'Stateful.run' itself; a call-stack loc would point inside
    -- @library/Hegel/Stateful.hs@, which the rich renderer would then try
    -- to splice into the report as if it were the user's test source.
    let machine =
          Stateful.Machine
            { initial = pure (Counter 0),
              rules = [increment],
              invariants = [neverAboveFive]
            }
    report <- check def (Stateful.run machine)
    case report.result of
      Counterexample {notes} -> do
        let machinery = [n | n <- notes, isMachinery n.kind]
            isMachinery = \case
              Annotation -> True
              StepHeader _ _ -> True
              _ -> False
        machinery `shouldNotSatisfy` null
        [n | n <- machinery, StepHeader _ _ <- [n.kind]] `shouldNotSatisfy` null
        machinery `shouldSatisfy` all (isNothing . (.loc))
      other -> expectationFailure ("expected Counterexample, got: " <> show other)

  it "journals the failing assertion in-band as a nested Failure note" do
    -- End-to-end: the caught failure is journaled in-band and still re-thrown,
    -- so the runner reports a counterexample.
    let machine =
          Stateful.Machine
            { initial = pure (Counter 0),
              rules = [increment],
              invariants = [neverAboveFive]
            }
    report <- check def (Stateful.run machine)
    case report.result of
      Counterexample {notes} ->
        case filter isFailureNote notes of
          [f] -> do
            f.text `shouldBe` "counter does not exceed 5"
            f.depth `shouldBe` 1
            f.loc `shouldSatisfy` (not . isNothing)
          fs -> expectationFailure ("expected exactly one Failure note, got: " <> show (length fs))
      other -> expectationFailure ("expected Counterexample, got: " <> show other)

  it "rich report splices the failing invariant's source" do
    -- End-to-end through 'renderReportRich': the failing step's notes splice
    -- into this file's declarations (requires cwd = repo root, as under
    -- `just test`).
    let machine =
          Stateful.Machine
            { initial = pure (Counter 0),
              rules = [increment],
              invariants = [neverAboveFive]
            }
    report <- check def (Stateful.run machine)
    rich <- renderReportRich report
    -- The invariant's assert line (in 'neverAboveFive') is spliced.
    ("assert (n <= 5)" `T.isInfixOf` rich) `shouldBe` True
    ("┏━━ tests/unit/Stateful.hs" `T.isInfixOf` rich) `shouldBe` True

  it "value-drawing counterexample reproduces on replay" do
    -- Regression guard for choice-sequence alignment: with multiple rules that
    -- draw values, the counterexample only reproduces if replay stays byte-
    -- aligned with generation. A misalignment surfaces here as 'Aborted' (the
    -- failure did not recur on replay), not 'Counterexample'.
    let machine =
          Stateful.Machine
            { initial = pure (Stack []),
              rules = [push, pushNonZeroBug],
              invariants = []
            }
    report <- check def (Stateful.run machine)
    case report.result of
      Counterexample {} -> pure ()
      other -> expectationFailure ("expected Counterexample, got: " <> show other)

  it "machine with no rules is aborted, not reported as a counterexample" do
    let machine :: Stateful.Machine Counter IO
        machine =
          Stateful.Machine
            { initial = pure (Counter 0),
              rules = [],
              invariants = []
            }
    report <- check def (Stateful.run machine)
    case report.result of
      Aborted _ -> pure ()
      other -> expectationFailure ("expected Aborted, got: " <> show other)

  it "the default statefulStepCount bounds steps, and most cases hit it exactly" do
    -- Analogue of the Rust reference's test_step_cap_is_50_most_of_the_time.
    counts <- stepRecorder False def {testCases = 30}
    counts `shouldSatisfy` all (\c -> c >= 1 && c <= 50)
    length (filter (== 50) counts) `shouldSatisfy` (> length counts `div` 2)

  it "an assume-rejecting rule is still bounded by the step cap" do
    -- Analogue of the Rust reference's
    -- test_hopeless_machine_is_bounded_by_the_step_cap. The cap counts
    -- attempted rules rather than successful ones, so a rule that never
    -- gets past its precondition is bounded the same as one that always
    -- succeeds.
    counts <- stepRecorder True def {testCases = 30}
    counts `shouldSatisfy` all (\c -> c >= 1 && c <= 50)
    length (filter (== 50) counts) `shouldSatisfy` (> length counts `div` 2)

  it "statefulStepCount replaces the default cap" do
    -- Analogue of the Rust reference's test_stateful_step_count_setting_bounds_steps.
    let n = 7 :: Int
    counts <- stepRecorder False def {testCases = 30, statefulStepCount = n}
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
  Stateful.Rule "register" \m -> do
    n <- forAll intGen
    Pool.add m.pool n
    pure m {registered = Set.insert n m.registered}

-- | Draw a value from the pool without removing it; it must be one we added.
useReusable :: Stateful.Rule Model IO
useReusable =
  Stateful.Rule "use_reusable" \m -> do
    v <- forAll (Pool.reuse m.pool)
    assert (Set.member v m.registered) "reusable draw was previously registered"
    pure m

-- | Consume a value from the pool; it must be one we added.
useConsumed :: Stateful.Rule Model IO
useConsumed =
  Stateful.Rule "use_consumed" \m -> do
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
  poolMachineSpec
