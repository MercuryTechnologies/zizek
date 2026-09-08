-- | Unit tests for 'Hegel.Property.registerFinalizer' and the per-case drain.
module Finalizers (spec) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (displayException, fromException)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (def)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T
import Hegel (Gen)
import Hegel.Assertion (AssertionFailure (..))
import Hegel.Diff (renderDiff)
import Hegel.Gen qualified as Gen
import Hegel.Internal.Control (FinalizerFailed (..), MalformedTest (..))
import Hegel.Property
  ( Property,
    assert,
    check,
    discard,
    failure,
    forAll,
    registerFinalizer,
    (===),
  )
import Hegel.Property.Branch qualified as Branch
import Hegel.Property.Fork qualified as Fork
import Hegel.Property.Internal (Env (..), Journal (..), askEnv)
import Hegel.Report (Abort (..), CleanupDiagnostic (..), FailureEvidence (..), FailureEvidenceStatus (..), FailureOutcome (..), ReplayDivergence (..), ReplayReason (..), ReplayStats (..), Report (..), Result (..), SkipReason (..), Stats (..))
import Hegel.Runner (replay)
import Hegel.Settings (Settings (..), defaultSettings)
import Test.Hspec
import TestSupport (allFailureOutcomes, expectReconstructed, expectToken, hasFailures)
import UnliftIO.Async (AsyncCancelled (..), cancel, replicateConcurrently_, waitCatch, withAsync)
import UnliftIO.Exception (finally, throwIO)
import UnliftIO.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)

intR :: (Int, Int) -> Gen Int
intR (lo, hi) = Gen.integral & Gen.min lo & Gen.max hi & Gen.build

spec :: Spec
spec = describe "registerFinalizer" do
  it "runs once per case on success, with no cross-case bleed" do
    ref <- newIORef (0 :: Int)
    report <- check def do
      registerFinalizer (modifyIORef' ref (+ 1))
      pure ()
    count <- readIORef ref
    report.result `shouldSatisfy` \case
      Ok -> True
      _ -> False
    count `shouldSatisfy` (> 0)
    -- One drain per attempted case, valid or discarded.
    count `shouldBe` report.stats.valid + report.stats.invalid

  it "runs on a failing case" do
    ran <- newIORef False
    report <- check def do
      registerFinalizer (writeIORef ran True)
      _ <- forAll (intR (0, 100))
      failure "always fails"
    readIORef ran `shouldReturn` True
    report.result `shouldSatisfy` hasFailures

  it "runs on a discarded case" do
    ran <- newIORef False
    report <- check def do
      registerFinalizer (writeIORef ran True)
      discard
    readIORef ran `shouldReturn` True
    case report.result of
      GaveUp _ -> pure ()
      other -> expectationFailure ("expected GaveUp, got: " <> show other)

  it "runs finalizers LIFO (last registered, first run)" do
    order <- newIORef ([] :: [Text])
    _ <- check (defaultSettings {testCases = 1}) do
      registerFinalizer (modifyIORef' order (++ ["a"]))
      registerFinalizer (modifyIORef' order (++ ["b"]))
      pure ()
    readIORef order `shouldReturn` ["b", "a"]

  it "loses no registrations under concurrent registerFinalizer calls" do
    -- Each registration pushes onto one shared registry. Fired concurrently,
    -- every push must survive, so the drained count equals the number of
    -- registrations.
    counter <- newIORef (0 :: Int)
    let n = 2000 :: Int
    report <- check (defaultSettings {testCases = 1}) do
      replicateConcurrently_
        n
        (registerFinalizer (atomicModifyIORef' counter \x -> (x + 1, ())))
    report.result `shouldSatisfy` \case
      Ok -> True
      _ -> False
    readIORef counter `shouldReturn` n

  it "aborts the run as Errored when a finalizer throws" do
    report <- check def do
      registerFinalizer (throwIO (userError "teardown boom"))
      pure ()
    case report.result of
      Aborted (Errored _) -> pure ()
      other -> expectationFailure ("expected Aborted Errored, got: " <> show other)

  it "captures a control signal thrown by a finalizer as an abort (does not escape check)" do
    -- A finalizer runs after the case is markComplete'd, so a discard/stop it
    -- throws is misuse, not a live signal to honor. The drain must capture it
    -- (→ Errored), not let it escape check uncaught and crash the host.
    report <- check def do
      registerFinalizer (discard :: IO ())
      pure ()
    case report.result of
      Aborted (Errored _) -> pure ()
      other -> expectationFailure ("expected Aborted Errored, got: " <> show other)

  it "live cleanup retains the body diagnostic, location, and diff" do
    report <- check def do
      registerFinalizer (throwIO (userError "teardown boom"))
      _ <- forAll (intR (0, 100))
      (10 :: Int) === 7
    case report.result of
      Aborted (Errored e) -> do
        let msg = T.pack (displayException e)
        msg `shouldSatisfy` T.isInfixOf "teardown boom"
        case fromException e of
          Just (FinalizerFailed (Just body) cleanup) -> do
            length cleanup `shouldBe` 1
            case fromException body of
              Just AssertionFailure {message, diff = Just difference} -> do
                msg `shouldSatisfy` T.isInfixOf message
                msg `shouldSatisfy` T.isInfixOf "Finalizers.hs"
                msg `shouldSatisfy` T.isInfixOf (renderDiff difference)
              other -> expectationFailure (show other)
          other -> expectationFailure (show other)
      other -> expectationFailure ("expected Aborted Errored (Errored wins), got: " <> show other)

  it "retains the test run failure alongside cleanup diagnostics" do
    report <- check def do
      registerFinalizer (throwIO (userError "teardown boom"))
      throwIO (MalformedTest "malformed body")
    case report.result of
      Aborted (Errored e) -> do
        let msg = T.pack (displayException e)
        msg `shouldSatisfy` T.isInfixOf "malformed body"
        msg `shouldSatisfy` T.isInfixOf "teardown boom"
      other -> expectationFailure ("expected the MalformedTest to win, got: " <> show other)

  for_ allCleanupScopes \(scopeName, scope) -> do
    it ("aborts live cleanup failures in " <> scopeName) do
      report <- check def do
        scope do
          registerFinalizer (throwIO (userError "first cleanup"))
          registerFinalizer (throwIO (userError "second cleanup"))
        failure "body evidence"
      case report.result of
        Aborted (Errored e) -> do
          let msg = T.pack (displayException e)
          msg `shouldSatisfy` T.isInfixOf "first cleanup"
          msg `shouldSatisfy` T.isInfixOf "second cleanup"
          msg `shouldSatisfy` T.isInfixOf "body evidence"
        other -> expectationFailure (show other)

  for_ cleanupMatrix \(scopeName, scope, body) ->
    it (scopeName <> " cleanup preserves " <> show body <> " and skips later reconstructions") do
      let settings = defaultSettings {reportMultipleFailures = True, testCases = 300, derandomize = True, databaseKey = Just "cleanup-reconstruction"}
      baseline <- check settings (cleanupProperty (\_ -> scope (pure ()) >> pure ReproducedBody))
      case allFailureOutcomes baseline.result of
        [first, second, third] -> do
          firstEvidence <- expectReconstructed (Failures (first :| []))
          secondEvidence <- expectReconstructed (Failures (second :| []))
          secondToken <- expectToken second
          executed <- newIORef ([] :: [Text])
          drained <- newIORef ([] :: [Text])
          let hook label = do
                scope do
                  env <- askEnv
                  case env.journal of
                    Silent -> pure ()
                    Recording _ -> do
                      modifyIORef' executed (++ [label])
                      registerFinalizer (modifyIORef' drained (++ [label <> " oldest"]))
                      when (label == secondEvidence.message) do
                        registerFinalizer (modifyIORef' drained (++ [label <> " throwing"]) >> throwIO (userError "second replay cleanup"))
                        registerFinalizer (throwIO (userError "additional cleanup diagnostic"))
                      registerFinalizer (modifyIORef' drained (++ [label <> " newest"]))
                env <- askEnv
                pure case env.journal of
                  Recording _ | label == secondEvidence.message -> body
                  _ -> ReproducedBody
          report <- check settings (cleanupProperty hook)
          report.stats.valid `shouldBe` baseline.stats.valid
          report.stats.invalid `shouldBe` baseline.stats.invalid
          case allFailureOutcomes report.result of
            [actualFirst, middle, skipped] -> do
              actual <- expectReconstructed (Failures (actualFirst :| []))
              actual.message `shouldBe` firstEvidence.message
              actual.notes `shouldNotSatisfy` null
              actualFirst.cleanupDiagnostics `shouldBe` []
              assertBody body secondEvidence middle
              map (.failureOrigin) [actualFirst, middle, skipped] `shouldBe` map (.failureOrigin) [first, second, third]
              map (.failureReplayToken) [actualFirst, middle, skipped] `shouldBe` map (.failureReplayToken) [first, second, third]
              case skipped.failureEvidence of
                Skipped reason -> reason `shouldBe` SkippedAfterCleanupFailure
                other -> expectationFailure (show other)
              skipped.cleanupDiagnostics `shouldBe` []
              readIORef executed `shouldReturn` [firstEvidence.message, secondEvidence.message]
              readIORef drained
                `shouldReturn` [ firstEvidence.message <> " newest",
                                 firstEvidence.message <> " oldest",
                                 secondEvidence.message <> " newest",
                                 secondEvidence.message <> " throwing",
                                 secondEvidence.message <> " oldest"
                               ]
              replayed <- replay settings secondToken (cleanupProperty hook)
              case allFailureOutcomes replayed.result of
                [outcome] -> assertBody body secondEvidence outcome
                other -> expectationFailure (show other)
              replayed.stats.replayStats `shouldBe` Just (bodyAccounting body)
            other -> expectationFailure (show other)
        other -> expectationFailure (show other)

  for_ cleanupScopes \(name, scope) -> do
    for_ [("discard", discard, UnexpectedDiscard), ("exhaustion", void (forAll (intR (0, 100))), ExhaustedChoices)] \(label, stop, reason) ->
      it (name <> " drains cleanup when " <> label <> " occurs inside the scope") do
        baseline <- check def (scope (pure ()) >> failure "scope baseline")
        case allFailureOutcomes baseline.result of
          [outcome] -> do
            token <- expectToken outcome
            drained <- newIORef False
            report <-
              replay
                def
                token
                ( scope do
                    registerFinalizer (writeIORef drained True)
                    registerFinalizer (throwIO (userError "scope cleanup"))
                    stop
                )
            case allFailureOutcomes report.result of
              [actual] -> do
                actual.cleanupDiagnostics `shouldBe` [CleanupDiagnostic "user error (scope cleanup)"]
                case actual.failureEvidence of
                  Diverged divergence -> divergence.replayReason `shouldBe` reason
                  other -> expectationFailure (show other)
              other -> expectationFailure (show other)
            readIORef drained `shouldReturn` True
          other -> expectationFailure (show other)

    it (name <> " drains cleanup and propagates asynchronous replay cancellation") do
      baseline <- check def (scope (pure ()) >> failure "cancel baseline")
      case allFailureOutcomes baseline.result of
        [outcome] -> do
          token <- expectToken outcome
          ready <- newEmptyMVar
          blocked <- newEmptyMVar
          drained <- newIORef False
          let body = scope do
                registerFinalizer (writeIORef drained True)
                registerFinalizer (throwIO (userError "cancelled cleanup"))
                liftIO (putMVar ready ())
                liftIO (takeMVar blocked)
          withAsync (replay def token body) \worker -> do
            takeMVar ready
            cancel worker
            result <- waitCatch worker
            case result of
              Left exception -> fromException exception `shouldBe` Just AsyncCancelled
              Right report -> expectationFailure (show report)
          readIORef drained `shouldReturn` True
        other -> expectationFailure (show other)

  it "drains cancelled-fork cleanup and skips later reconstructions" do
    let settings = defaultSettings {reportMultipleFailures = True, derandomize = True}
    executions <- newIORef (0 :: Int)
    drains <- newIORef (0 :: Int)
    report <-
      check
        settings
        ( cleanupProperty \_ -> do
            cancelledFork do
              env <- askEnv
              case env.journal of
                Silent -> pure ()
                Recording _ -> do
                  modifyIORef' executions (+ 1)
                  registerFinalizer (modifyIORef' drains (+ 1))
                  registerFinalizer (throwIO (userError "cancel cleanup"))
            pure ReproducedBody
        )
    case allFailureOutcomes report.result of
      [first, second, third] -> do
        void (expectReconstructed (Failures (first :| [])))
        first.cleanupDiagnostics `shouldBe` [CleanupDiagnostic "user error (cancel cleanup)"]
        for_ [second, third] \outcome -> case outcome.failureEvidence of
          Skipped SkippedAfterCleanupFailure -> void (expectToken outcome)
          other -> expectationFailure (show other)
      other -> expectationFailure (show other)
    readIORef executions `shouldReturn` 1
    readIORef drains `shouldReturn` 1

data ReplayBody = ReproducedBody | ChangedBody | PassingBody | DiscardedBody | ExhaustedBody
  deriving stock (Eq, Show, Enum, Bounded)

bodyAccounting :: ReplayBody -> ReplayStats
bodyAccounting = \case
  ReproducedBody -> ReplayStats 1 0 0 0 1
  ChangedBody -> ReplayStats 1 0 0 0 0
  PassingBody -> ReplayStats 1 1 0 0 0
  DiscardedBody -> ReplayStats 1 0 1 0 0
  ExhaustedBody -> ReplayStats 1 0 0 1 0

assertBody :: ReplayBody -> FailureEvidence -> FailureOutcome -> Expectation
assertBody body expected outcome = do
  outcome.cleanupDiagnostics
    `shouldBe` [CleanupDiagnostic "user error (additional cleanup diagnostic)", CleanupDiagnostic "user error (second replay cleanup)"]
  case (body, outcome.failureEvidence) of
    (ReproducedBody, Reconstructed evidence) -> evidence.message `shouldBe` expected.message
    (ChangedBody, Diverged (ReplayDivergence (ChangedOrigin actual))) -> actual `shouldNotBe` outcome.failureOrigin
    (PassingBody, Diverged (ReplayDivergence UnexpectedSuccess)) -> pure ()
    (DiscardedBody, Diverged (ReplayDivergence UnexpectedDiscard)) -> pure ()
    (ExhaustedBody, Diverged (ReplayDivergence ExhaustedChoices)) -> pure ()
    other -> expectationFailure (show other)

cleanupProperty :: (Text -> Property ReplayBody) -> Property ()
cleanupProperty hook = do
  x <- forAll (intR (0, 2))
  body <- hook (case x of 0 -> "failure 0"; 1 -> "failure 1"; _ -> "failure 2")
  case body of
    ReproducedBody -> case x of
      0 -> assert False "failure 0"
      1 -> assert False "failure 1"
      _ -> assert False "failure 2"
    ChangedBody -> assert False "changed cleanup origin"
    PassingBody -> pure ()
    DiscardedBody -> discard
    ExhaustedBody -> void (forAll (Gen.list (intR (0, 100)) & Gen.minSize 10000 & Gen.maxSize 10000 & Gen.build))

cleanupScopes :: [(String, Property () -> Property ())]
cleanupScopes =
  [ ("root", id),
    ("branch", \body -> Branch.concurrently_ body (pure ())),
    ("fork", \body -> Fork.spawn body >>= Fork.join)
  ]

allCleanupScopes :: [(String, Property () -> Property ())]
allCleanupScopes = cleanupScopes <> [("cancelled fork", cancelledFork)]

cleanupMatrix :: [(String, Property () -> Property (), ReplayBody)]
cleanupMatrix =
  [("root", id, body) | body <- [minBound .. maxBound]]
    <> [("branch", \body -> Branch.concurrently_ body (pure ()), body) | body <- [ReproducedBody, ChangedBody]]
    <> [("fork", \body -> Fork.spawn body >>= Fork.join, body) | body <- [ReproducedBody, ChangedBody]]

cancelledFork :: Property () -> Property ()
cancelledFork body = do
  ready <- liftIO newEmptyMVar
  blocked <- liftIO newEmptyMVar
  worker <- Fork.spawn do
    body `finally` liftIO (putMVar ready ())
    liftIO (takeMVar blocked)
  liftIO (takeMVar ready)
  Fork.cancel worker
