module BasicProperties (spec) where

import Data.Function ((&))
import Hegel (Gen, Phase (..), runProperty, runProperty_)
import Hegel.Gen qualified as Gen
import Hegel.Outcome (Outcome (..))
import Hegel.Runner (Settings (..), defaultSettings)
import Test.Hspec

intR :: (Int, Int) -> Gen Int
intR (lo, hi) = Gen.integral & Gen.min lo & Gen.max hi & Gen.build

spec :: Spec
spec = do
  it "all integers in [0,100] are in [0,100]" $
    runProperty_ defaultSettings (intR (0, 100)) $ \n ->
      n `shouldSatisfy` (\x -> x >= 0 && x <= 100)

  it "shrinks to 42 when forbidding 42" $ do
    outcome <- runProperty defaultSettings (intR (0, 100)) $ \n ->
      n `shouldNotBe` 42
    outcome `shouldSatisfy` \case
      Failed {} -> True
      _ -> False

  it "honours phases = [Generate]" $
    runProperty_ (defaultSettings {phases = [Generate]}) (intR (0, 100)) $ \n ->
      n `shouldSatisfy` (\x -> x >= 0 && x <= 100)
