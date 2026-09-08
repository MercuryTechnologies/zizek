-- | Smoke tests for the hspec and tasty integrations.
module Integrations (spec, tastyTree) where

import Control.Exception (displayException, try)
import Control.Monad (when)
import Data.Default.Class (def)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust, listToMaybe)
import Data.Text qualified as T
import GHC.Stack (SrcLoc (..))
import Hegel qualified
import Hegel.Gen qualified as Gen
import Hegel.Hspec (propWith)
import Hegel.Pool qualified as Pool
import Hegel.Property (Property, assert, check_, footnote, forAll, forEach, (===))
import Hegel.Property.Internal (Env (..), Journal (..), askEnv)
import Hegel.Replay (decodeReplayToken, encodeReplayToken)
import Hegel.Report
import Hegel.Runner (check, replay)
import Hegel.Settings (Settings (..))
import Hegel.Stateful qualified as Stateful
import Hegel.Tasty qualified
import Test.Hspec
import Test.Hspec.Core.Spec qualified as HspecCore
import Test.Tasty (TestTree)
import Test.Tasty.Providers qualified as Tasty
import Test.Tasty.Runners qualified as TastyTree
import TestSupport (allFailureOutcomes, expectReconstructed, expectToken, failureRecordOf)
import TraceFixtures (eventfulMachine)

spec :: Spec
spec = do
  propWith def "runs a property block as an hspec Example" do
    x <- forAll (Gen.int & Gen.min 0 & Gen.max 10 & Gen.build)
    assert (x >= 0 && x <= 10) "stays in range"

  it "maps counterexamples to hspec failures with a location" $ do
    -- Evaluate a property directly to inspect its hspec Result. A property
    -- written as @() -> PropertyT IO ()@ is an Example via the @arg ->@
    -- (fixture) instance; @($ ())@ supplies the trivial unit fixture.
    HspecCore.Result _ status <-
      HspecCore.evaluateExample
        ( \() -> do
            x <- forAll (Gen.int & Gen.min 0 & Gen.max 10 & Gen.build)
            x === x + 1
        )
        HspecCore.defaultParams
        ($ ())
        (\_ -> pure ())
    case status of
      HspecCore.Failure loc (HspecCore.Reason reason) -> do
        loc `shouldSatisfy` isJust
        reason `shouldContain` "=== failed"
      other -> expectationFailure ("expected a failure with a reason, got: " <> show other)

  it "preserves the singleton prop exception report and its displayed replay token" $
    checkExceptionReplay (Hegel.prop (pure ()) singletonBody) (forEach (pure ()) singletonBody) False

  it "preserves pool events and a replayable token in check_ exceptions" $
    checkExceptionReplay (check_ def (Stateful.run eventfulMachine)) (Stateful.run eventfulMachine) True

  it "renders every failure through Hspec and uses the first reconstructed location" $ do
    expected <- check multipleSettings multipleProperty >>= expectMultiple
    HspecCore.Result _ status <- runHspec (propWith multipleSettings "multiple" multipleProperty)
    case status of
      HspecCore.Failure location (HspecCore.Reason reason) -> do
        checkMultipleText expected reason
        case (location, ((.loc) =<< (failureRecordOf =<< listToMaybe expected))) of
          (Just actual, Just wanted) -> do
            HspecCore.locationFile actual `shouldBe` srcLocFile wanted
            HspecCore.locationLine actual `shouldBe` srcLocStartLine wanted
          _ -> expectationFailure "missing first failure location"
      other -> expectationFailure ("expected hspec failure: " <> show other)

  it "Hspec navigates to a later location after an initial divergence" do
    baseline <- check multipleSettings (navigationProperty Nothing)
    case allFailureOutcomes baseline.result of
      [first, second] -> do
        firstEvidence <- expectReconstructed (Failures (first :| []))
        secondEvidence <- expectReconstructed (Failures (second :| []))
        let property = navigationProperty (Just firstEvidence.message)
        HspecCore.Result _ status <- runHspec (propWith multipleSettings "later location" property)
        case status of
          HspecCore.Failure (Just actual) (HspecCore.Reason text) -> do
            case secondEvidence.loc of
              Just expected -> do
                HspecCore.locationFile actual `shouldBe` srcLocFile expected
                HspecCore.locationLine actual `shouldBe` srcLocStartLine expected
              Nothing -> expectationFailure "missing baseline location"
            text `shouldContain` "failure 1 (replay diverged)"
            text `shouldContain` T.unpack secondEvidence.message
          other -> expectationFailure (show other)
      other -> expectationFailure (show other)

  it "renders every failure through Tasty in engine order" $ do
    expected <- check multipleSettings multipleProperty >>= expectMultiple
    result <- runTasty (Hegel.Tasty.testPropertyWith multipleSettings "multiple" multipleProperty)
    TastyTree.resultSuccessful result `shouldBe` False
    checkMultipleText expected (TastyTree.resultDescription result)

tastyTree :: TestTree
tastyTree =
  Hegel.Tasty.testProperty "tasty integration: runs a property via IsTest" do
    x <- forAll (Gen.int & Gen.min 0 & Gen.max 10 & Gen.build)
    assert (x <= 10) "upper bound holds"

multipleSettings :: Settings
multipleSettings = def {reportMultipleFailures = True, testCases = 200, derandomize = True, databaseKey = Just "integration-multiple"}

multipleProperty :: Property ()
multipleProperty = do
  branch <- forAll (Gen.int & Gen.min 0 & Gen.max 1 & Gen.build)
  Stateful.run
    Stateful.Machine
      { initial = do
          pool <- Pool.named "widgets"
          Pool.add pool (7 :: Int)
          pure pool,
        rules =
          [ Stateful.Rule "inspect" \pool -> do
              value <- forAll (Pool.reuse pool)
              footnote (if branch == 0 then "first diagnostic" else "second diagnostic")
              if branch == 0 then firstDiff value else secondDiff value
              pure pool
          ],
        invariants = []
      }

firstDiff, secondDiff :: Int -> Property ()
firstDiff value = value === 8
secondDiff value = value === 9

expectMultiple :: Report -> IO [FailureOutcome]
expectMultiple Report {result} = case allFailureOutcomes result of
  [ FailureOutcome {failureEvidence = Reconstructed a},
    FailureOutcome {failureEvidence = Reconstructed b}
    ] -> do
      for_ [a, b] \record -> do
        record.events `shouldNotSatisfy` null
        record.notes `shouldNotSatisfy` null
        record.loc `shouldSatisfy` isJust
        record.diff `shouldSatisfy` isJust
      pure (allFailureOutcomes result)
  _ -> expectationFailure "expected two reconstructed failures" >> pure []

checkMultipleText :: [FailureOutcome] -> String -> Expectation
checkMultipleText records rendered = do
  let text = T.pack rendered
  rendered `shouldContain` "failure 1"
  rendered `shouldContain` "failure 2"
  rendered `shouldContain` "first diagnostic"
  rendered `shouldContain` "second diagnostic"
  text `shouldSatisfy` (\value -> "widgets₁" `T.isInfixOf` value || "widgets1" `T.isInfixOf` value)
  rendered `shouldContain` "inspect"
  rendered `shouldContain` "=== failed"
  rendered `shouldContain` "Integrations.hs"
  rendered `shouldContain` "7"
  rendered `shouldContain` "8"
  rendered `shouldContain` "9"
  tokens <- traverse expectToken records
  let positions = fmap (T.length . fst . (`T.breakOn` text) . encodeReplayToken) tokens
  positions `shouldSatisfy` \case [a, b] -> a < b && b < T.length text; _ -> False
  for_ tokens \token -> text `shouldSatisfy` T.isInfixOf (encodeReplayToken token)

runHspec :: Spec -> IO HspecCore.Result
runHspec specification = do
  (_, trees) <- HspecCore.runSpecM specification
  case concatMap leaves trees of
    [item] -> HspecCore.itemExample item HspecCore.defaultParams ($ ()) (\_ -> pure ())
    _ -> fail "expected one hspec item"
  where
    leaves :: HspecCore.Tree c (HspecCore.Item a) -> [HspecCore.Item a]
    leaves = \case
      HspecCore.Leaf item -> [item]
      HspecCore.Node _ children -> concatMap leaves children
      HspecCore.NodeWithCleanup _ _ children -> concatMap leaves children

runTasty :: TestTree -> IO Tasty.Result
runTasty (TastyTree.SingleTest _ test) = Tasty.run mempty test (\_ -> pure ())
runTasty _ = fail "expected one tasty leaf"

checkExceptionReplay :: IO () -> Property () -> Bool -> Expectation
checkExceptionReplay action property expectEvents = do
  caught <- try @PropertyFailed action
  case caught of
    Right () -> expectationFailure "expected PropertyFailed"
    Left exception -> case allFailureOutcomes exception.report.result of
      [outcome@FailureOutcome {failureEvidence = Reconstructed FailureEvidence {message, events}}] -> do
        token <- expectToken outcome
        when expectEvents (events `shouldNotSatisfy` null)
        let displayed = T.pack (displayException exception)
            tokenLines = [T.drop (T.length "replay token: ") line | line <- T.lines displayed, "replay token: " `T.isPrefixOf` line]
        tokenLines `shouldBe` [encodeReplayToken token]
        encoded <- case tokenLines of
          [value] -> pure value
          _ -> fail "expected one displayed token"
        decoded <- either (fail . show) pure (decodeReplayToken encoded)
        decoded `shouldBe` token
        replayed <- replay def decoded property
        case allFailureOutcomes replayed.result of
          [FailureOutcome {failureReplayToken = actualToken, failureEvidence = Reconstructed FailureEvidence {message = actual, events = actualEvents}}] -> do
            actual `shouldBe` message
            actualToken `shouldBe` Just token
            when expectEvents (actualEvents `shouldNotSatisfy` null)
          other -> expectationFailure ("expected reconstructed replay: " <> show other)
      other -> expectationFailure ("expected singleton report with token: " <> show other)

singletonBody :: () -> IO ()
singletonBody _ = assert False "singleton exception"

navigationProperty :: Maybe T.Text -> Property ()
navigationProperty changed = do
  n <- forAll (Gen.int & Gen.min 0 & Gen.max 1 & Gen.build)
  let message = if n == 0 then "navigation zero" else "navigation one"
  env <- askEnv
  case env.journal of
    Recording _ | changed == Just message -> pure ()
    _ ->
      if n == 0
        then assert False "navigation zero"
        else assert False "navigation one"
