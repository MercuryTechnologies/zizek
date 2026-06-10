module BasicProperties (spec) where

import Data.Function ((&))
import Hegel (Gen, prop)
import Hegel.Gen qualified as Gen
import Hegel.Phase (Phase (..))
import Hegel.Property (check_, forEach)
import Hegel.Report (Note (..), NoteKind (..), Report (..), Result (..))
import Hegel.Runner (check)
import Hegel.Settings (Settings (..), defaultSettings)
import Test.Hspec

intR :: (Int, Int) -> Gen Int
intR (lo, hi) = Gen.integral & Gen.min lo & Gen.max hi & Gen.build

spec :: Spec
spec = do
  it "all integers in [0,100] are in [0,100]" $ do
    prop (intR (0, 100)) $ \n ->
      n `shouldSatisfy` (\x -> x >= 0 && x <= 100)

  it "shrinks to the smallest forbidden value" $ do
    report <- check defaultSettings $ forEach (intR (0, 100)) $ \n ->
      n `shouldSatisfy` (< 42)
    case report.result of
      Counterexample {notes} ->
        fmap (.text) (filter (\n -> n.kind == Drawn) notes) `shouldBe` ["42"]
      other -> expectationFailure ("expected a counterexample, got: " <> show other)

  it "honours phases = [Generate]" $ do
    check_ (defaultSettings {phases = [Generate]}) $ forEach (intR (0, 100)) $ \n ->
      n `shouldSatisfy` (\x -> x >= 0 && x <= 100)
