-- | Unit tests for 'Hegel.Pool' and 'Hegel.Stateful'.
module Stateful (spec) where

import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Hegel (Gen)
import Hegel.Gen qualified as Gen
import Hegel.Pool (Pool)
import Hegel.Pool qualified as Pool
import Hegel.Property (assert, assume, forAll, forAllSilent)
import Hegel.Property.Internal (Env (..), askEnv)
import Hegel.Report (Note (..), NoteKind (..), Report (..), Result (..))
import Hegel.Runner (check)
import Hegel.Settings (defaultSettings)
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
    report <- check defaultSettings do
      env <- askEnv
      pool <- liftIO (Pool.new env.testCase)
      -- Immediately draw from an empty pool → AssumeRejected → Invalid.
      _ <- forAllSilent (Pool.valuesReusable pool)
      assert False "should not be reached"
    -- Every case is discarded, so we expect GaveUp (all Invalid), never a failure.
    case report.result of
      GaveUp _ -> pure ()
      Counterexample {} -> expectationFailure "expected GaveUp, got a counterexample"
      other -> expectationFailure ("expected GaveUp (all invalid), got: " <> show other)

  it "valuesReusable returns an added value without removing it" do
    report <- check defaultSettings do
      env <- askEnv
      pool <- liftIO (Pool.new env.testCase)
      n <- forAll intGen
      liftIO (Pool.add pool n)
      a <- forAll (Pool.valuesReusable pool)
      b <- forAll (Pool.valuesReusable pool)
      assert (a == n && b == n) "reusable draw returns the added value each time"
    report.result `shouldSatisfy` \case
      Ok -> True
      _ -> False

  it "valuesConsumed returns and removes the value" do
    report <- check defaultSettings do
      env <- askEnv
      pool <- liftIO (Pool.new env.testCase)
      n <- forAll intGen
      liftIO (Pool.add pool n)
      v <- forAll (Pool.valuesConsumed pool)
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
    report <- check defaultSettings (Stateful.run machine)
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
    report <- check defaultSettings (Stateful.run machine)
    case report.result of
      Counterexample {} -> pure ()
      other -> expectationFailure ("expected Counterexample, got: " <> show other)

  it "machinery annotations carry no source location" do
    -- The 'Step N: …' / invariant-check annotations are emitted by
    -- 'Stateful.run' itself; a call-stack loc would point inside
    -- @library/Hegel/Stateful.hs@, which the rich renderer would then try
    -- to splice into the report as if it were the user's test source.
    let machine =
          Stateful.Machine
            { initial = pure (Counter 0),
              rules = [increment],
              invariants = [neverAboveFive]
            }
    report <- check defaultSettings (Stateful.run machine)
    case report.result of
      Counterexample {notes} -> do
        let annotations = [n | n <- notes, n.kind == Annotation]
        annotations `shouldNotSatisfy` null
        annotations `shouldSatisfy` all (isNothing . (.loc))
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
    report <- check defaultSettings (Stateful.run machine)
    case report.result of
      Counterexample {notes} ->
        case [n | n <- notes, n.kind == Failure] of
          [f] -> do
            f.text `shouldBe` "counter does not exceed 5"
            f.depth `shouldBe` 1
            f.loc `shouldSatisfy` (not . isNothing)
          fs -> expectationFailure ("expected exactly one Failure note, got: " <> show (length fs))
      other -> expectationFailure ("expected Counterexample, got: " <> show other)

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
    report <- check defaultSettings (Stateful.run machine)
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
    report <- check defaultSettings (Stateful.run machine)
    case report.result of
      Aborted _ -> pure ()
      other -> expectationFailure ("expected Aborted, got: " <> show other)

  it "precondition (assume) in a rule skips the step without failing" do
    -- A rule that always rejects via assume; the machine should give up rather
    -- than fail, since every step is skipped.
    let alwaysRejects :: Stateful.Rule Counter IO
        alwaysRejects =
          Stateful.Rule "always_rejects" \s -> do
            assume False
            pure s
        machine =
          Stateful.Machine
            { initial = pure (Counter 0),
              rules = [alwaysRejects],
              invariants = []
            }
    report <- check defaultSettings (Stateful.run machine)
    case report.result of
      Ok -> pure () -- livelock guard may allow the machine to "succeed" with 0 steps
      GaveUp _ -> pure ()
      Counterexample {} -> expectationFailure "a rule with assume False should not produce a counterexample"
      other -> expectationFailure ("unexpected result: " <> show other)

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
    liftIO (Pool.add m.pool n)
    pure m {registered = Set.insert n m.registered}

-- | Draw a value from the pool without removing it; it must be one we added.
useReusable :: Stateful.Rule Model IO
useReusable =
  Stateful.Rule "use_reusable" \m -> do
    v <- forAll (Pool.valuesReusable m.pool)
    assert (Set.member v m.registered) "reusable draw was previously registered"
    pure m

-- | Consume a value from the pool; it must be one we added.
useConsumed :: Stateful.Rule Model IO
useConsumed =
  Stateful.Rule "use_consumed" \m -> do
    v <- forAll (Pool.valuesConsumed m.pool)
    assert (Set.member v m.registered) "consumed draw was previously registered"
    pure m

poolMachine :: Stateful.Machine Model IO
poolMachine =
  Stateful.Machine
    { initial = do
        env <- askEnv
        p <- liftIO (Pool.new env.testCase)
        pure (Model p Set.empty),
      rules = [register, useReusable, useConsumed],
      invariants = []
    }

poolMachineSpec :: Spec
poolMachineSpec = describe "Pool + Machine" do
  it "values added in one rule are drawn back in another" do
    report <- check defaultSettings (Stateful.run poolMachine)
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
