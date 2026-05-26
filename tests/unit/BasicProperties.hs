module BasicProperties (spec) where

import Data.Function ((&))
import Hegel (Phase (..), runProperty, runProperty_)
import Hegel.Generators (Generator)
import Hegel.Generators.Integer qualified as Integer
import Hegel.Outcome (Outcome (..))
import Hegel.Runner (Settings (..), defaultSettings)
import Test.Hspec

intR :: (Int, Int) -> Generator Int
intR r = Integer.gen $ Integer.integers @Int & Integer.withRange r

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
