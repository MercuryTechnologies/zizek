module SequenceGenerators (spec) where

import Data.Default.Class (def)
import Data.Function ((&))
import Data.List (isSubsequenceOf, nub, sort)
import Data.Text (Text)
import Data.Text qualified as T
import Hegel (Gen, prop)
import Hegel.Gen qualified as Gen
import Hegel.Property (assert, forEach)
import Hegel.Report (Report (..), isDrawn)
import Hegel.Runner (check)
import Test.Hspec
import TestSupport (failureNotes, noteKind, noteText)

-- | The drawn value a failing property shrinks to, rendered without spaces.
minimal :: (Show a) => Gen a -> (a -> Bool) -> IO [Text]
minimal g interesting = do
  report <- check def $ forEach g \x -> assert (not (interesting x)) "interesting"
  pure (T.filter (/= ' ') . noteText <$> filter (isDrawn . noteKind) (failureNotes report.result))

spec :: Spec
spec = do
  describe "Gen.permutation" $ do
    it "reorders its input without adding or losing values" $
      prop (Gen.permutation [1 .. 10 :: Int]) \xs ->
        sort xs `shouldBe` [1 .. 10]

    it "keeps duplicate values" $
      prop (Gen.permutation "aabbc") \xs ->
        sort xs `shouldBe` "aabbc"

    it "produces an empty list from an empty input" $
      prop (Gen.permutation ([] :: [Int])) (`shouldBe` [])

    it "shrinks to the original order" $
      minimal (Gen.permutation [3, 1, 2 :: Int]) (const True) `shouldReturn` ["[3,1,2]"]

    it "moves a value to the front without disturbing the rest" $
      minimal (Gen.permutation [1 .. 5 :: Int]) (\xs -> take 1 xs == [5]) `shouldReturn` ["[5,1,2,3,4]"]

  describe "Gen.subsequence" $ do
    it "keeps the original order" $
      prop (Gen.subsequence [1 .. 10 :: Int] & Gen.build) \xs ->
        xs `shouldSatisfy` (`isSubsequenceOf` [1 .. 10])

    it "stays within its size bounds" $
      prop (Gen.subsequence [1 .. 10 :: Int] & Gen.minSize 2 & Gen.maxSize 4 & Gen.build) \xs ->
        length xs `shouldSatisfy` (\n -> n >= 2 && n <= 4)

    it "accepts a maxSize larger than its input" $
      prop (Gen.subsequence [1, 2, 3 :: Int] & Gen.maxSize 10 & Gen.build) \xs ->
        length xs `shouldSatisfy` (<= 3)

    it "produces an empty list from an empty input" $
      prop (Gen.subsequence ([] :: [Int]) & Gen.build) (`shouldBe` [])

    it "shrinks to the empty list" $
      minimal (Gen.subsequence [1 .. 5 :: Int] & Gen.build) (const True) `shouldReturn` ["[]"]

    it "shrinks to a prefix of minSize values" $
      minimal (Gen.subsequence [1 .. 5 :: Int] & Gen.minSize 3 & Gen.build) (const True) `shouldReturn` ["[1,2,3]"]

  describe "Gen.sample" $ do
    it "draws only values from its input" $
      prop (Gen.sample "abc" & Gen.maxSize 20 & Gen.build) \xs ->
        xs `shouldSatisfy` all (`elem` ("abc" :: String))

    it "can repeat a value by default" $
      minimal (Gen.sample [1, 2 :: Int] & Gen.build) (\xs -> nub xs /= xs) `shouldReturn` ["[1,1]"]

    it "takes each position at most once without replacement" $
      prop (Gen.sample [1 .. 10 :: Int] & Gen.withoutReplacement & Gen.build) \xs ->
        nub xs `shouldBe` xs

    it "stays within its size bounds without replacement" $
      prop (Gen.sample [1 .. 10 :: Int] & Gen.withoutReplacement & Gen.minSize 2 & Gen.maxSize 4 & Gen.build) \xs ->
        length xs `shouldSatisfy` (\n -> n >= 2 && n <= 4)

    it "produces an empty list from an empty input" $ do
      prop (Gen.sample ([] :: [Int]) & Gen.build) (`shouldBe` [])
      prop (Gen.sample ([] :: [Int]) & Gen.withoutReplacement & Gen.build) (`shouldBe` [])

    it "shrinks to repeats of the first value" $
      minimal (Gen.sample [1 .. 5 :: Int] & Gen.minSize 3 & Gen.build) (const True) `shouldReturn` ["[1,1,1]"]

    it "shrinks to a prefix of minSize values without replacement" $
      minimal (Gen.sample [1 .. 5 :: Int] & Gen.withoutReplacement & Gen.minSize 3 & Gen.build) (const True)
        `shouldReturn` ["[1,2,3]"]
