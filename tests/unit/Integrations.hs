-- | Smoke tests for the hspec and tasty integrations (native backend).
module Integrations (spec, tastyTree) where

import Data.Function ((&))
import Data.Maybe (isJust)
import Hegel.Gen qualified as Gen
import Hegel.Hspec (hegel)
import Hegel.Property (assert, forAll, (===))
import Hegel.Tasty qualified
import Test.Hspec
import Test.Hspec.Core.Spec qualified as HspecCore
import Test.Tasty (TestTree)

spec :: Spec
spec = do
  it "runs a bare property block as an hspec Example" $ hegel do
    x <- forAll (Gen.int & Gen.min 0 & Gen.max 10 & Gen.build)
    assert (x >= 0 && x <= 10) "stays in range"

  it "maps counterexamples to hspec failures with a location" $ do
    let prop = hegel do
          x <- forAll (Gen.int & Gen.min 0 & Gen.max 10 & Gen.build)
          x === x + 1
    HspecCore.Result _ status <-
      HspecCore.evaluateExample prop HspecCore.defaultParams ($ ()) (\_ -> pure ())
    case status of
      HspecCore.Failure loc (HspecCore.Reason reason) -> do
        loc `shouldSatisfy` isJust
        reason `shouldContain` "=== failed"
      other -> expectationFailure ("expected a failure with a reason, got: " <> show other)

tastyTree :: TestTree
tastyTree =
  Hegel.Tasty.testProperty "tasty integration: runs a property via IsTest" do
    x <- forAll (Gen.int & Gen.min 0 & Gen.max 10 & Gen.build)
    assert (x <= 10) "upper bound holds"
