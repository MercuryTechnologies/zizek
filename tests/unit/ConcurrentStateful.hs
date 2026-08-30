-- | Unit tests for 'Hegel.Stateful.Concurrent'.
module ConcurrentStateful (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (fromException)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (def)
import Data.Function ((&))
import Data.Set qualified as Set
import Data.Text qualified as T
import Hegel (Gen)
import Hegel.Database (Database (..))
import Hegel.Gen qualified as Gen
import Hegel.Internal.Control (MalformedTest (..))
import Hegel.Property (assert, assume, forAll, resource, (===))
import Hegel.Report (Abort (..), Report (..), Result (..), Stats (..), renderReport)
import Hegel.Runner (check)
import Hegel.Settings (Settings (..))
import Hegel.Stateful.Concurrent qualified as Concurrent
import Test.Hspec
import UnliftIO.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import UnliftIO.Temporary (withSystemTempDirectory)

intGen :: Gen Int
intGen = Gen.int & Gen.min 0 & Gen.max 100 & Gen.build

-- | A model reused across tests where the machine's own state doesn't
-- matter, only that rules run.
newtype Counter = Counter Int

isOk :: Result -> Bool
isOk = \case
  Ok -> True
  _ -> False

isCounterexample :: Result -> Bool
isCounterexample = \case
  Counterexample {} -> True
  _ -> False

-- | Whether a report aborted with a 'MalformedTest' whose message contains
-- @needle@.
isMalformedTestAbort :: T.Text -> Result -> Bool
isMalformedTestAbort needle = \case
  Aborted (Errored e) -> case fromException e of
    Just (MalformedTest msg) -> needle `T.isInfixOf` msg
    Nothing -> False
  _ -> False

-- ---------------------------------------------------------------------------
-- internGroups

internGroupsSpec :: Spec
internGroupsSpec = describe "internGroups" do
  it "assigns dense ids by first appearance" do
    Concurrent.internGroups [Just "a", Just "b", Just "a"]
      `shouldBe` (["a", "b"], [0, 1, 0])

  it "normalizes Nothing to the anonymous group" do
    Concurrent.internGroups [Nothing, Just Concurrent.anonymousGroup]
      `shouldBe` ([Concurrent.anonymousGroup], [0, 0])

  it "keeps named and anonymous groups distinct" do
    Concurrent.internGroups [Just "w", Nothing, Just "w", Just "r"]
      `shouldBe` (["w", Concurrent.anonymousGroup, "r"], [0, 1, 0, 2])

-- ---------------------------------------------------------------------------
-- Machine validation

validationSpec :: Spec
validationSpec = describe "run (validation)" do
  it "a machine with no rules is a malformed test" do
    let machine :: Concurrent.Machine Counter IO
        machine = Concurrent.Machine {initial = pure (Counter 0), rules = [], invariants = []}
    report <- check def (Concurrent.run (Concurrent.fixed 1) machine)
    report.result `shouldSatisfy` isMalformedTestAbort "at least one rule"

  it "invalid concurrency bounds are a malformed test" do
    let noop :: Concurrent.Rule Counter IO
        noop = Concurrent.rule "noop" \_ -> pure ()
        machine = Concurrent.Machine {initial = pure (Counter 0), rules = [noop], invariants = []}
    forM_ [(0, 1), (2, 1)] \(lo, hi) -> do
      report <- check def (Concurrent.run (Concurrent.between lo hi) machine)
      report.result `shouldSatisfy` isMalformedTestAbort "1 <= min <= max"

  it "resource inside a concurrent rule's apply is a malformed test" do
    let badRule :: Concurrent.Rule Counter IO
        badRule =
          Concurrent.rule "bad" \_ -> do
            _ <- resource (pure ()) (const (pure ()))
            pure ()
        machine = Concurrent.Machine {initial = pure (Counter 0), rules = [badRule], invariants = []}
    report <- check def (Concurrent.run (Concurrent.fixed 1) machine)
    report.result `shouldSatisfy` isMalformedTestAbort "resource:"

-- ---------------------------------------------------------------------------
-- Engine behavior

behaviorSpec :: Spec
behaviorSpec = describe "run (behavior)" do
  it "discards the run's first max>1 machine creation as invalid rather than failing" do
    let noop :: Concurrent.Rule Counter IO
        noop = Concurrent.rule "noop" \_ -> pure ()
        machine = Concurrent.Machine {initial = pure (Counter 0), rules = [noop], invariants = []}
    report <- check def {testCases = 10} (Concurrent.run (Concurrent.upTo 2) machine)
    report.result `shouldSatisfy` isOk
    report.stats.invalid `shouldSatisfy` (>= 1)
    report.stats.valid `shouldSatisfy` (>= 1)

  it "the first max>1 machine creation costs exactly one discarded case" do
    -- 'testCases' targets valid examples, not raw attempts, so the engine
    -- transparently retries past the mandatory first discard rather than
    -- giving up: asking for exactly one valid case still costs one Invalid
    -- case first, deterministically, since only the run's first creation
    -- above max concurrency 1 is ever rejected.
    let noop :: Concurrent.Rule Counter IO
        noop = Concurrent.rule "noop" \_ -> pure ()
        machine = Concurrent.Machine {initial = pure (Counter 0), rules = [noop], invariants = []}
    report <- check def {testCases = 1} (Concurrent.run (Concurrent.upTo 2) machine)
    report.result `shouldSatisfy` isOk
    report.stats.invalid `shouldBe` 1
    report.stats.valid `shouldBe` 1

  it "fixed 1 keeps the run fully deterministic" do
    let failing :: Concurrent.Rule Counter IO
        failing =
          Concurrent.rule "fail_on_42" \_ -> do
            n <- forAll intGen
            assert (n /= 42) "n is not 42"
        machine = Concurrent.Machine {initial = pure (Counter 0), rules = [failing], invariants = []}
    report <- check def {testCases = 200} (Concurrent.run (Concurrent.fixed 1) machine)
    report.result `shouldSatisfy` isCounterexample
    report.stats.invalid `shouldBe` 0

  it "an invariant failure at a join point surfaces as a counterexample" do
    let bump :: Concurrent.Rule (IORef Int) IO
        bump = Concurrent.rule "bump" \ref -> liftIO (modifyIORef' ref (+ 1))
        neverAboveThree :: Concurrent.Invariant (IORef Int) IO
        neverAboveThree =
          Concurrent.Invariant "never_above_three" \ref -> do
            n <- liftIO (readIORef ref)
            assert (n <= 3) "counter stays small"
        machine =
          Concurrent.Machine
            { initial = liftIO (newIORef 0),
              rules = [bump],
              invariants = [neverAboveThree]
            }
    report <- check def {statefulStepCount = 10} (Concurrent.run (Concurrent.fixed 1) machine)
    report.result `shouldSatisfy` isCounterexample

  it "a rejected rule's assumption skips the step without discarding the case" do
    let sometimesRejects :: Concurrent.Rule (IORef Int) IO
        sometimesRejects =
          Concurrent.rule "maybe_bump" \ref -> do
            n <- forAll intGen
            assume (n `mod` 5 /= 0)
            liftIO (modifyIORef' ref (+ 1))
        machine = Concurrent.Machine {initial = liftIO (newIORef 0), rules = [sometimesRejects], invariants = []}
    report <- check def {testCases = 20} (Concurrent.run (Concurrent.fixed 1) machine)
    report.result `shouldSatisfy` isOk

  it "a round's dispatches never span two groups" do
    -- Deterministic, not timing-based: each rule records its own group into
    -- a shared set on dispatch, and the invariant, checked only once every
    -- worker has finished the round, atomically reads and clears that set.
    -- A round mixing groups would leave more than one label in it; same-group
    -- rules sharing a round (a1 and a2, both "g1") correctly leave just one.
    -- No delay is needed: the check is exact regardless of how the round's
    -- dispatches interleaved in wall-clock time, not a race to observe them
    -- overlapping.
    touchedThisRound <- newIORef Set.empty
    violated <- newIORef False
    let mkRule :: T.Text -> T.Text -> Concurrent.Rule () IO
        mkRule nm grp =
          Concurrent.grouped nm grp \_ ->
            liftIO (atomicModifyIORef' touchedThisRound \s -> (Set.insert grp s, ()))
        oneGroupPerRound :: Concurrent.Invariant () IO
        oneGroupPerRound =
          Concurrent.Invariant "one_group_per_round" \_ -> liftIO do
            groups <- atomicModifyIORef' touchedThisRound \s -> (Set.empty, s)
            when (Set.size groups > 1) (writeIORef violated True)
        machine =
          Concurrent.Machine
            { initial = pure (),
              rules = [mkRule "a1" "g1", mkRule "a2" "g1", mkRule "b1" "g2"],
              invariants = [oneGroupPerRound]
            }
    report <- check def {testCases = 5, statefulStepCount = 15} (Concurrent.run (Concurrent.upTo 4) machine)
    report.result `shouldSatisfy` isOk
    v <- readIORef violated
    v `shouldBe` False

  it "finds a lost-update race on a shared IORef" do
    let raceRule :: Concurrent.Rule RaceModel IO
        raceRule =
          Concurrent.rule "increment" \m -> liftIO do
            v <- readIORef m.counter
            threadDelay 200
            writeIORef m.counter (v + 1)
            modifyIORef' m.attempts (+ 1)
        noLostUpdates :: Concurrent.Invariant RaceModel IO
        noLostUpdates =
          Concurrent.Invariant "no_lost_updates" \m -> do
            c <- liftIO (readIORef m.counter)
            a <- liftIO (readIORef m.attempts)
            c === a
        machine =
          Concurrent.Machine
            { initial = liftIO (RaceModel <$> newIORef 0 <*> newIORef 0),
              rules = [raceRule],
              invariants = [noLostUpdates]
            }
    report <- check def {testCases = 20, statefulStepCount = 30} (Concurrent.run (Concurrent.fixed 4) machine)
    report.result `shouldSatisfy` isCounterexample

  it "a failure reports its origin and no reproducer" $
    withSystemTempDirectory "zizek-concurrent-stateful" \dbDir -> do
      let failing :: Concurrent.Rule Counter IO
          failing = Concurrent.rule "boom" \_ -> assert False "always fails"
          machine = Concurrent.Machine {initial = pure (Counter 0), rules = [failing], invariants = []}
          settings =
            def
              { testCases = 5,
                database = DatabaseDirectory dbDir,
                databaseKey = Just "concurrent-stateful-origin-spec"
              }
      report <- check settings (Concurrent.run (Concurrent.upTo 2) machine)
      report.result `shouldSatisfy` isCounterexample
      report.databaseKey `shouldBe` Nothing
      let rendered = renderReport report
      ("no stored example to replay" `T.isInfixOf` rendered) `shouldBe` True
      ("stored under" `T.isInfixOf` rendered) `shouldBe` False

-- | A model carrying both the racy shared counter and the ground-truth
-- attempt count, for 'behaviorSpec's lost-update race.
data RaceModel = RaceModel
  { counter :: IORef Int,
    attempts :: IORef Int
  }

spec :: Spec
spec = do
  internGroupsSpec
  validationSpec
  behaviorSpec
