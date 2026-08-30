-- | Unit tests for 'Hegel.Stateful.Concurrent'.
module ConcurrentStateful (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_, fromException)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (def)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Text qualified as T
import Hegel (Gen)
import Hegel.Database (Database (..))
import Hegel.Gen qualified as Gen
import Hegel.Internal.Control (MalformedTest (..))
import Hegel.Property (assert, assume, forAll, resource, (===))
import Hegel.Property.Fork qualified as Fork
import Hegel.Report (Abort (..), Note (..), NoteKind (Annotation, StepHeader), Report (..), Reproduction (..), Result (..), Stats (..), renderReport, renderReportRich)
import Hegel.Runner (check)
import Hegel.Settings (Settings (..))
import Hegel.Stateful.Concurrent qualified as Concurrent
import System.Timeout (timeout)
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
      `shouldBe` ([Just "a", Just "b"], [0, 1, 0])

  it "never merges an explicit group named after anonymousGroup's own display string" do
    -- Interning keys on the 'Maybe Text' label itself, never collapsing
    -- 'Nothing' to a display string, so 'Nothing' (truly ungrouped) and
    -- @Just anonymousGroup@ (a rule explicitly grouped under that exact
    -- name) get distinct ids and stay distinguishable in the labels
    -- returned here, not merely by id.
    Concurrent.internGroups [Nothing, Just Concurrent.anonymousGroup]
      `shouldBe` ([Nothing, Just Concurrent.anonymousGroup], [0, 1])

  it "keeps named and anonymous groups distinct" do
    Concurrent.internGroups [Just "w", Nothing, Just "w", Just "r"]
      `shouldBe` ([Just "w", Nothing, Just "r"], [0, 1, 0, 2])

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
    for_ [(0, 1), (2, 1)] \(lo, hi) -> do
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

  it "notes why a rejected step ended, once a later step fails" do
    -- Deterministic, no draws: the rule's own dispatch count (fresh per
    -- case, via the model) decides everything, so the very first case is
    -- already the failing one and nothing here depends on shrinking.
    let flaky :: Concurrent.Rule (IORef Int) IO
        flaky =
          Concurrent.rule "flaky" \ref -> do
            n <- liftIO (atomicModifyIORef' ref \a -> (a + 1, a + 1))
            assume (n /= 1)
            assert (n < 2) "fails on the second attempt"
        machine = Concurrent.Machine {initial = liftIO (newIORef 0), rules = [flaky], invariants = []}
    report <- check def {testCases = 1, statefulStepCount = 5} (Concurrent.run (Concurrent.fixed 1) machine)
    report.result `shouldSatisfy` isCounterexample
    case report.result of
      Counterexample {notes} -> do
        let rejectionNotes =
              [n | n <- notes, n.kind == Annotation, "Rule stopped early due to violated assumption." `T.isInfixOf` n.text]
        length rejectionNotes `shouldBe` 1
      _ -> expectationFailure "expected a Counterexample"

  it "settles a fork spawned by a rule that then rejects, rather than leaving it running" do
    -- The rule always rejects, immediately, always abandoning its fork
    -- without ever joining it. The fork's own body never finishes on its
    -- own within this test's lifetime, so the only way 'active' can drop
    -- back to 0 is genuine cancellation on the exception path.
    active <- newIORef (0 :: Int)
    let leaky :: Concurrent.Rule () IO
        leaky =
          Concurrent.rule "leaky" \_ -> do
            _worker <-
              Fork.spawn . liftIO $
                bracket_
                  (atomicModifyIORef' active \a -> (a + 1, ()))
                  (atomicModifyIORef' active \a -> (a - 1, ()))
                  (threadDelay maxBound)
            assume False
        machine = Concurrent.Machine {initial = pure (), rules = [leaky], invariants = []}
    -- A tight step budget matters here: every rejected dispatch spawns and
    -- abandons another fork, and the default budget (50) would let a single
    -- case churn through far more fork spawn\/cancel cycles than this test
    -- needs to exercise the fix.
    report <- check def {testCases = 10, statefulStepCount = 3} (Concurrent.run (Concurrent.fixed 3) machine)
    report.result `shouldSatisfy` isOk
    let waitForSettled = do
          a <- readIORef active
          when (a > 0) (threadDelay 1_000 >> waitForSettled)
    settled <- timeout 2_000_000 waitForSettled
    settled `shouldBe` Just ()

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

  it "a failure reports its own assertion message and no reproducer" $
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
      report.reproduction `shouldBe` Unreproducible
      case report.result of
        Counterexample {message} -> message `shouldBe` "always fails"
        _ -> expectationFailure "expected a Counterexample"
      let rendered = renderReport report
      ("no stored example to replay" `T.isInfixOf` rendered) `shouldBe` True
      ("stored under" `T.isInfixOf` rendered) `shouldBe` False
      richRendered <- renderReportRich report
      ("no stored example to replay" `T.isInfixOf` richRendered) `shouldBe` True
      ("stored under" `T.isInfixOf` richRendered) `shouldBe` False

  it "captures the failing rule's own step, live, in a nondeterministic report" do
    let failing :: Concurrent.Rule Counter IO
        failing = Concurrent.rule "boom" \_ -> assert False "always fails"
        machine = Concurrent.Machine {initial = pure (Counter 0), rules = [failing], invariants = []}
    report <- check def {testCases = 5} (Concurrent.run (Concurrent.fixed 2) machine)
    report.result `shouldSatisfy` isCounterexample
    case report.result of
      Counterexample {notes} -> do
        let isBoomStep :: Note -> Bool
            isBoomStep n = case n.kind of
              StepHeader _ label -> label == "boom"
              _ -> False
        notes `shouldSatisfy` any isBoomStep
        -- The round/worker identity folds in as a companion note right
        -- after each step header, not baked into the header's own text.
        let afterEachBoomStep =
              [rest | (n, rest) <- zip notes (drop 1 (List.tails notes)), isBoomStep n]
            roundWorkerNote :: [Note] -> Bool
            roundWorkerNote (r : _) = r.kind == Annotation && ("round" `T.isInfixOf` r.text) && ("worker" `T.isInfixOf` r.text)
            roundWorkerNote [] = False
        afterEachBoomStep `shouldSatisfy` all roundWorkerNote
      _ -> expectationFailure "expected a Counterexample"
    -- The richer, source-splicing renderer must not choke on a folded,
    -- multi-worker journal either, and must show the round/worker detail
    -- line as an ordinary annotation under the step, not only the plain
    -- renderer.
    rendered <- renderReportRich report
    ("boom" `T.isInfixOf` rendered) `shouldBe` True
    ("round" `T.isInfixOf` rendered) `shouldBe` True
    ("worker" `T.isInfixOf` rendered) `shouldBe` True

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
