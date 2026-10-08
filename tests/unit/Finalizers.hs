-- | Unit tests for 'Hegel.Property.registerFinalizer' and the per-case drain.
module Finalizers (spec) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (displayException, fromException)
import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (def)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as T
import Hegel (Gen)
import Hegel.Assertion (AssertionFailure (..))
import Hegel.Diff (renderDiff)
import Hegel.Gen qualified as Gen
import Hegel.Internal.Control (FinalizerFailed (..), malformedTest)
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
import Hegel.Report (Abort (..), Report (..), Result (..), Stats (..))
import Hegel.Runner (replay)
import Hegel.Settings (Settings (..), defaultSettings)
import Test.Hspec
import TestSupport (allFailureOutcomes, expectToken, hasFailures)
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
      throwIO (malformedTest "test" "malformed body" [])
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

  for_ allCleanupScopes \(scopeName, scope) ->
    it ("aborts when cleanup fails only in a stamped case in " <> scopeName) do
      report <- check def do
        x <- forAll (intR (0, 1000))
        scope do
          env <- askEnv
          case env.journal of
            Silent -> pure ()
            Recording _ -> registerFinalizer (throwIO (userError "stamped cleanup"))
        assert (x < 100) "stays small"
      case report.result of
        Aborted (Errored e) -> do
          T.pack (displayException e) `shouldSatisfy` T.isInfixOf "stamped cleanup"
          case fromException e of
            Just (FinalizerFailed (Just _) _) -> pure ()
            other -> expectationFailure (show other)
        other -> expectationFailure (show other)

  for_ cleanupScopes \(name, scope) ->
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

cleanupScopes :: [(String, Property () -> Property ())]
cleanupScopes =
  [ ("root", id),
    ("branch", \body -> Branch.concurrently_ body (pure ())),
    ("fork", \body -> Fork.spawn body >>= Fork.join)
  ]

allCleanupScopes :: [(String, Property () -> Property ())]
allCleanupScopes = cleanupScopes <> [("cancelled fork", cancelledFork)]

cancelledFork :: Property () -> Property ()
cancelledFork body = do
  ready <- liftIO newEmptyMVar
  blocked <- liftIO newEmptyMVar
  worker <- Fork.spawn do
    body `finally` liftIO (putMVar ready ())
    liftIO (takeMVar blocked)
  liftIO (takeMVar ready)
  Fork.cancel worker
