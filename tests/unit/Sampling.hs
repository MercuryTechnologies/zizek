-- | Coverage for 'Hegel.sample'\/'Hegel.samples': drawing generated values
-- outside a property run.
module Sampling (spec) where

import Data.Default.Class (def)
import Data.Function ((&))
import Data.List (nub)
import Hegel (Gen, defaultSettings, sample, samples)
import Hegel.Gen qualified as Gen
import Hegel.Settings (Settings (..))
import Test.Hspec

intR :: (Int, Int) -> Gen Int
intR (lo, hi) = Gen.integral & Gen.min lo & Gen.max hi & Gen.build

spec :: Spec
spec = do
  describe "sample" do
    it "returns a value from the generator" do
      n <- sample def (intR (0, 100))
      n `shouldSatisfy` \x -> x >= 0 && x <= 100

    it "reproduces the same value under the same seed" do
      let settings = defaultSettings {seed = Just 42}
          gen = intR (0, 1000000)
      a <- sample settings gen
      b <- sample settings gen
      a `shouldBe` b

    it "generally differs under a different seed" do
      let gen = intR (0, 1000000)
      a <- sample defaultSettings {seed = Just 1} gen
      b <- sample defaultSettings {seed = Just 2} gen
      a `shouldNotBe` b

    it "throws on a generator that always discards" do
      sample def Gen.discard `shouldThrow` anyIOException

  describe "samples" do
    it "returns at most n values" do
      xs <- samples def 50 (intR (0, 1000000))
      length xs `shouldSatisfy` (<= 50)

    it "returns more than one distinct value" do
      xs <- samples def 50 (intR (0, 1000000))
      length (nub xs) `shouldSatisfy` (> 1)

    it "throws when the generator's valid rate is too low to pass FilterTooMuch" do
      samples def 100 (Gen.filtered (const False) (Gen.int & Gen.build))
        `shouldThrow` anyIOException
