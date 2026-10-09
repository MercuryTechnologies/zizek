-- | Configuration resolution and runner execution contracts.
module RunnerConfiguration (spec) where

import Control.Concurrent.Async (AsyncCancelled)
import Control.Exception (SomeException, fromException)
import Control.Monad (void)
import Data.Either (isLeft)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Maybe (isJust)
import Data.Text qualified as T
import Data.Word (Word64)
import Hegel.Database (Database (..))
import Hegel.Exception (Diagnostic (..), SettingsError (..))
import Hegel.Gen qualified as Gen
import Hegel.Hspec qualified as Hspec
import Hegel.Internal.RunnerConfig qualified as Config
import Hegel.Phase (Phase (..))
import Hegel.Property (Property, assert, assume, forAll, registerFinalizer)
import Hegel.Replay (encodeReplayToken)
import Hegel.Report (Abort (..), FailureOutcome (..), Report (..), Result (..))
import Hegel.Runner qualified as Runner
import Hegel.Seed (Seed (..))
import Hegel.Settings (Settings (..), defaultSettings)
import Hegel.Tasty qualified as Native
import Test.Hspec
import Test.Hspec.Core.Spec qualified as Core
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Test.Tasty.Options (OptionSet, singleOption)
import Test.Tasty.Providers qualified as Provider
import Test.Tasty.Runners qualified as Tree
import TestSupport (allFailureOutcomes, expectToken)
import UnliftIO (liftIO)
import UnliftIO.Async (cancel, waitCatch, withAsync)
import UnliftIO.Exception (bracket_)
import UnliftIO.IORef
import UnliftIO.MVar (newEmptyMVar, putMVar, takeMVar)
import UnliftIO.Temporary (withSystemTempDirectory)

spec :: Spec
spec = do
  describe "settings profiles" do
    it "keeps every field the right-hand settings set" do
      let combined = defaultSettings {testCases = Just 1, seed = Just (SeedFixed 1)} <> defaultSettings {seed = Just SeedFresh}
      combined.testCases `shouldBe` Just 1
      combined.seed `shouldBe` Just SeedFresh

    it "starts from the named profile and lets an override win over it" do
      let rejecting = draw >> assume False
      developed <- Runner.check defaultSettings {profile = Just "development"} rejecting
      developed.result `shouldSatisfy` unhealthy
      -- The workload profile suppresses every health check.
      worked <- Runner.check defaultSettings {profile = Just "workload"} rejecting
      worked.result `shouldNotSatisfy` unhealthy
      overridden <- Runner.check defaultSettings {profile = Just "workload", suppressHealthCheck = Just []} rejecting
      overridden.result `shouldSatisfy` unhealthy

    it "aborts with a SettingsError naming an unknown profile" do
      report <- Runner.check defaultSettings {profile = Just "no-such-profile"} draw
      case report.result of
        Aborted (Errored e) | Just (SettingsError d) <- fromException e -> do
          d.context `shouldBe` "Hegel.Settings.profile"
          lookup "profile" d.values `shouldBe` Just "no-such-profile"
        other -> expectationFailure (show other)

    it "omits replay tokens when printBlob is off" do
      printed <- Runner.check defaultSettings failing
      fmap (isJust . (.failureReplayToken)) (allFailureOutcomes printed.result) `shouldBe` [True]
      hidden <- Runner.check defaultSettings {printBlob = Just False} failing
      fmap (isJust . (.failureReplayToken)) (allFailureOutcomes hidden.result) `shouldBe` [False]

  describe "runner configuration" do
    it "validates effective programmatic settings after overrides" do
      let invalid = defaultSettings {testCases = Just (-1)}
      Config.resolve invalid mempty `shouldSatisfy` isLeft
      override <- parsed [("test-cases", "1")]
      resolved <- either fail pure (Config.resolve invalid override)
      resolved.testCases `shouldBe` Just 1
      for_ [defaultSettings {maxCloneDepth = Just (-1)}] \settings ->
        Config.resolve settings mempty `shouldSatisfy` isLeft

    it "renders invalid programmatic settings consistently in Hspec and Tasty" do
      -- The engine rejects an unknown profile, so its error is raised inside
      -- the runner, after this library's own validation has passed.
      for_ [defaultSettings {maxCloneDepth = Just (-1)}, defaultSettings {profile = Just "no-such-profile"}] \settings -> do
        ran <- newIORef False
        let body = liftIO (writeIORef ran True)
        native <- runTree mempty (Native.testPropertyWith settings "bad settings" body)
        Tree.resultSuccessful native `shouldBe` False
        Tree.resultDescription native `shouldContain` "SettingsError"
        Tree.resultDescription native `shouldContain` "tests/unit/RunnerConfiguration.hs"
        hspecResult <- evalFixture () (Hspec.propForWith settings "bad settings" (const body))
        reason hspecResult `shouldContain` "SettingsError"
        reason hspecResult `shouldContain` "tests/unit/RunnerConfiguration.hs"
        readIORef ran `shouldReturn` False

    it "preserves absent values and overlays only supplied settings" do
      low <- parsed [("test-cases", "7"), ("seed", "11"), ("database", "disabled")]
      high <- parsed [("seed", "12")]
      resolved <- either fail pure (Config.resolve defaultSettings (low <> high))
      resolved.testCases `shouldBe` Just 7
      resolved.seed `shouldBe` Just (SeedFixed 12)
      resolved.database `shouldBe` Just DatabaseDisabled
      fresh <- parsed [("seed", "none"), ("database", "some/dir")]
      fresh.settings.seed `shouldBe` Just SeedFresh
      fresh.settings.database `shouldBe` Just (DatabaseDirectory "some/dir")
      unchanged <- either fail pure (Config.resolve resolved mempty)
      show unchanged `shouldBe` show resolved

    it "accepts numeric bounds and rejects malformed and overflowing inputs" do
      for_ [("test-cases", "0"), ("test-cases", show (maxBound :: Int)), ("seed", show (maxBound :: Word64))] \entry -> void (parsed [entry])
      for_ [("test-cases", "-1"), ("test-cases", "1.0"), ("test-cases", " 2"), ("test-cases", "+2"), ("test-cases", show (toInteger (maxBound :: Int) + 1)), ("seed", "18446744073709551616"), ("seed", ""), ("seed", "None"), ("database", "")] \entry@(name, _) ->
        case Config.parseOverrides (`lookup` [entry]) of
          Left message -> message `shouldContain` name
          Right value -> expectationFailure (show value)

    it "requires complete replay pairs within each source" do
      for_ [[("replay", "bad")], [("replay-key", "id")], [("replay", "bad"), ("replay-key", "id")], [("replay", "bad"), ("replay-key", "")]] \values ->
        Config.parseOverrides (`lookup` values) `shouldSatisfy` isLeft

    it "leaves the database to the settings profile unless a source overrides it" do
      resolved <- either fail pure (Config.resolve defaultSettings {databaseKey = Just "id"} mempty)
      resolved.database `shouldBe` Nothing

    it "applies native options before executing and rejects invalid configuration without a body" do
      count <- newIORef (0 :: Int)
      let body = draw >> liftIO (modifyIORef' count (+ 1))
      result <- runTree (singleOption (Native.HegelTestCases (Just "3"))) (Native.testProperty "cases" body)
      Tree.resultSuccessful result `shouldBe` True
      readIORef count `shouldReturn` 3
      writeIORef count 0
      bad <- runTree (singleOption (Native.HegelDatabase (Just ""))) (Native.testProperty "empty database" body)
      Tree.resultSuccessful bad `shouldBe` False
      Tree.resultDescription bad `shouldContain` "hegel-database"
      readIORef count `shouldReturn` 0
      invalid <- runTree (singleOption (Native.HegelTestCases (Just "no"))) (Native.testProperty "invalid" body)
      Tree.resultSuccessful invalid `shouldBe` False
      readIORef count `shouldReturn` 0

    it "replays a matching identity once with phases disabled and leaves other identities exploring" do
      baseline <- Runner.check defaultSettings failing
      token <- case allFailureOutcomes baseline.result of
        outcome : _ -> expectToken outcome
        _ -> fail "missing token"
      overrides <- parsed [("replay", T.unpack (encodeReplayToken token)), ("replay-key", "target")]
      count <- newIORef (0 :: Int)
      let body = liftIO (modifyIORef' count (+ 1)) >> failing
      report <- Config.execute (\_ -> pure ()) defaultSettings {databaseKey = Just "target", phases = Just []} overrides body
      allFailureOutcomes report.result `shouldSatisfy` (not . null)
      readIORef count `shouldReturn` 1
      writeIORef count 0
      ordinary <- Config.execute (\_ -> pure ()) defaultSettings {databaseKey = Just "other", testCases = Just 3} overrides (draw >> liftIO (modifyIORef' count (+ 1)))
      show ordinary.result `shouldBe` show Ok
      readIORef count `shouldReturn` 3
      divergent <- Config.execute (\_ -> pure ()) defaultSettings {databaseKey = Just "target"} overrides draw
      allFailureOutcomes divergent.result `shouldSatisfy` (not . null)

    it "reports completed cases after finalizers and includes shrink probes" do
      cleaned <- newIORef (0 :: Int)
      seen <- newIORef []
      let body = registerFinalizer (modifyIORef' cleaned (+ 1)) >> failing
          progress n = do
            readIORef cleaned `shouldReturn` n
            modifyIORef' seen (n :)
      report <- Runner.checkWithProgress progress defaultSettings body
      allFailureOutcomes report.result `shouldSatisfy` (not . null)
      counts <- reverse <$> readIORef seen
      counts `shouldBe` [1 .. length counts]
      length counts `shouldSatisfy` (> 1)

    it "native progress uses an unknown percentage after cleanup" do
      cleaned <- newIORef False
      seen <- newIORef []
      let body = registerFinalizer (writeIORef cleaned True) >> draw
          callback p = do
            readIORef cleaned `shouldReturn` True
            Provider.progressPercent p `shouldBe` 0
            modifyIORef' seen (Provider.progressText p :)
      case Native.testProperty "progress" body of
        Tree.SingleTest _ test -> void (Provider.run mempty test callback)
        _ -> fail "expected leaf"
      readIORef seen >>= (`shouldSatisfy` (not . null))

    it "fixture helpers persist under their describe path and honor explicit settings" $
      withSystemTempDirectory "hegel-fixture" \directory -> do
        let mk phases =
              describe "fixture group" $
                Hspec.propForModify (\s -> s {database = Just (DatabaseDirectory directory), phases = Just phases}) "fixture" (\() -> failing)
        first <- evalFixture () (mk [minBound .. maxBound])
        reason first `shouldContain` "RunnerConfiguration:fixture group/fixture"
        second <- evalFixture () (mk [Reuse])
        reason second `shouldContain` "runner failure"
        disabled <- evalFixture () (Hspec.propForWith defaultSettings {phases = Just [Reuse]} "fixture" (\() -> failing))
        reason disabled `shouldContain` "gave up"

    it "keeps one fixture around all cases and drains per-case finalizers" do
      acquired <- newIORef (0 :: Int)
      released <- newIORef (0 :: Int)
      cleaned <- newIORef (0 :: Int)
      let aroundAction action =
            bracket_
              (modifyIORef' acquired (+ 1))
              (modifyIORef' released (+ 1))
              (action ())
          body () = do
            registerFinalizer (modifyIORef' cleaned (+ 1))
            failing
      result <- evalAround aroundAction (Hspec.propForWith defaultSettings "fixture lifetime" body)
      reason result `shouldContain` "runner failure"
      readIORef acquired `shouldReturn` 1
      readIORef released `shouldReturn` 1
      readIORef cleaned >>= (`shouldSatisfy` (> 1))

    it "releases the fixture and per-case resources when an example is cancelled" do
      entered <- newEmptyMVar
      blocked <- newEmptyMVar
      cleaned <- newIORef False
      released <- newIORef False
      let aroundAction action = bracket_ (pure ()) (writeIORef released True) (action ())
          body () = do
            registerFinalizer (writeIORef cleaned True)
            liftIO (putMVar entered ())
            liftIO (takeMVar blocked)
      withAsync (evalAround aroundAction (Hspec.propForWith defaultSettings "cancel" body)) \worker -> do
        takeMVar entered
        cancel worker
        waitCatch worker >>= (`shouldSatisfy` surfacedAsCancellation)
      readIORef cleaned `shouldReturn` True
      readIORef released `shouldReturn` True

    it "same-named native leaves persist independently with explicit keys" $
      withSystemTempDirectory "hegel-native" \directory -> do
        let tree key phases = Native.testPropertyWith defaultSettings {database = Just (DatabaseDirectory directory), databaseKey = Just key, phases = Just phases} "same" failing
        first <- runTree mempty (tree "first" [minBound .. maxBound])
        Tree.resultDescription first `shouldContain` "runner failure"
        empty <- runTree mempty (tree "second" [Reuse])
        Tree.resultDescription empty `shouldContain` "gave up"
        void (runTree mempty (tree "second" [minBound .. maxBound]))
        for_ ["first", "second"] \key -> do
          stored <- runTree mempty (tree key [Reuse])
          Tree.resultDescription stored `shouldContain` "runner failure"

    it "preserves Hspec identities through actual tasty-hspec conversion" $
      withSystemTempDirectory "hegel-adapted" \directory -> do
        let make phases =
              testSpec "outer Tasty group" $
                describe "inner" $
                  Hspec.propModify (\s -> s {database = Just (DatabaseDirectory directory), phases = Just phases}) "adapted" failing
        generated <- make [minBound .. maxBound] >>= runTree mempty
        Tree.resultDescription generated `shouldContain` "RunnerConfiguration:inner/adapted"
        stored <- make [Reuse] >>= runTree mempty
        Tree.resultDescription stored `shouldContain` "runner failure"

parsed :: [(String, String)] -> IO Config.Overrides
parsed values = either fail pure (Config.parseOverrides (`lookup` values))

draw :: Property ()
draw = void (forAll (Gen.int & Gen.build))

failing :: Property ()
failing = draw >> assert False "runner failure"

runTree :: OptionSet -> TestTree -> IO Provider.Result
runTree opts = \case
  Tree.SingleTest _ test -> Provider.run opts test (\_ -> pure ())
  Tree.TestGroup _ [child] -> runTree opts child
  Tree.PlusTestOptions modify child -> runTree (modify opts) child
  _ -> fail "expected one leaf"

-- | Whether a cancelled example surfaced the cancellation itself, either rethrown
-- or reported as the example's error, rather than passing or failing as a
-- property.
--
-- hspec-core may do either: the runner's own thread wrapper rethrows it, while
-- 'Core.itemExample' called directly may catch it and report an error.
surfacedAsCancellation :: Either SomeException Core.Result -> Bool
surfacedAsCancellation = \case
  Left e -> isCancellation e
  Right (Core.Result _ (Core.Failure _ (Core.Error _ e))) -> isCancellation e
  Right _ -> False
  where
    isCancellation e = isJust (fromException @AsyncCancelled e)

evalFixture :: a -> SpecWith a -> IO Core.Result
evalFixture fixture = evalAround ($ fixture)

evalAround :: (Core.ActionWith a -> IO ()) -> SpecWith a -> IO Core.Result
evalAround aroundAction specification = do
  (_, trees) <- Core.runSpecM specification
  case concatMap leaves trees of
    [item] -> Core.itemExample item Core.defaultParams aroundAction (\_ -> pure ())
    _ -> fail "expected one example"
  where
    leaves :: Core.Tree c (Core.Item a) -> [Core.Item a]
    leaves = \case
      Core.Leaf item -> [item]
      Core.Node _ children -> concatMap leaves children
      Core.NodeWithCleanup _ _ children -> concatMap leaves children

reason :: Core.Result -> String
reason (Core.Result _ (Core.Failure _ (Core.Reason message))) = message
reason other = show other

-- | Whether a run aborted on a failed health check.
unhealthy :: Result -> Bool
unhealthy = \case
  Aborted (UnhealthyInput _) -> True
  _ -> False
