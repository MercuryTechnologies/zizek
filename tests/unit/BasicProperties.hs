module BasicProperties (spec) where

import Data.Function ((&))
import Hegel (Gen)
import Hegel.Gen qualified as Gen
import Hegel.Phase (Phase (..))
import Hegel.Report (Report (..), Result (..))
import Hegel.Settings (Settings (..), defaultSettings)
import Test.Hspec
import TestRunner (Runner, runWith, runWith_)

intR :: (Int, Int) -> Gen Int
intR (lo, hi) = Gen.integral & Gen.min lo & Gen.max hi & Gen.build

spec :: Runner -> Spec
spec runner = do
  it "all integers in [0,100] are in [0,100]" $ do
    runWith_ runner defaultSettings (intR (0, 100)) $ \n ->
      n `shouldSatisfy` (\x -> x >= 0 && x <= 100)

  it "shrinks to 42 when forbidding 42" $ do
    report <- runWith runner defaultSettings (intR (0, 100)) $ \n ->
      n `shouldNotBe` (42 :: Int)
    report.result `shouldSatisfy` \case
      Counterexample {} -> True
      _ -> False

  it "honours phases = [Generate]" $ do
    runWith_ runner (defaultSettings {phases = [Generate]}) (intR (0, 100)) $ \n ->
      n `shouldSatisfy` (\x -> x >= 0 && x <= 100)
