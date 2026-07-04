-- | Coverage for 'Hegel.Alphabet': every curated preset's draws land
-- inside its documented character set, and every finite preset's draws
-- span the whole set given enough test cases. Doubles as a regression guard
-- for the @categories [] & includeCharacters@ encoding the escape hatches
-- ('Hegel.Alphabet.only', 'Hegel.Alphabet.ranges') lean on.
module Alphabet (spec) where

import Data.Char (GeneralCategory (..), generalCategory, ord)
import Data.Function ((&))
import Data.Set qualified as Set
import Hegel (defaultSettings, prop, samples)
import Hegel.Alphabet qualified as Alphabet
import Hegel.Gen qualified as Gen
import Test.Hspec

-- | The distinct characters a builder draws over @n@ test cases.
sample :: Int -> Gen.CharBuilder -> IO (Set.Set Char)
sample n b = Set.fromList <$> samples defaultSettings n (b & Gen.build)

-- | A codepoint-range membership predicate, inclusive on both ends.
inRange :: Int -> Int -> Char -> Bool
inRange lo hi c = let cp = ord c in cp >= lo && cp <= hi

spec :: Spec
spec = describe "Hegel.Alphabet" do
  describe "contiguous ranges only draw characters in their documented range" do
    mapM_
      (\(name, builder, member) -> it name (prop (builder & Gen.build) (`shouldSatisfy` member)))
      [ ("ascii", Alphabet.ascii, inRange 0x00 0x7F),
        ("asciiPrintable", Alphabet.asciiPrintable, inRange 0x20 0x7E),
        ("lower", Alphabet.lower, inRange 0x61 0x7A),
        ("upper", Alphabet.upper, inRange 0x41 0x5A),
        ("digit", Alphabet.digit, inRange 0x30 0x39),
        ("binit", Alphabet.binit, inRange 0x30 0x31),
        ("octit", Alphabet.octit, inRange 0x30 0x37),
        ("latin1", Alphabet.latin1, inRange 0x00 0xFF)
      ]

  describe "category-based alphabets" do
    it "alpha only draws ASCII letters" $
      prop (Alphabet.alpha & Gen.build) $ \c ->
        c `shouldSatisfy` (\x -> (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z'))

    it "alphaNum only draws ASCII letters or digits" $
      prop (Alphabet.alphaNum & Gen.build) $ \c ->
        c `shouldSatisfy` (\x -> (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z') || (x >= '0' && x <= '9'))

    it "unicode excludes surrogate codepoints" $
      prop (Alphabet.unicode & Gen.build) $ \c ->
        generalCategory c `shouldNotBe` Surrogate

    it "whitespace only draws separator or control-whitespace characters" $
      prop (Alphabet.whitespace & Gen.build) $ \c ->
        c `shouldSatisfy` \x ->
          generalCategory x `elem` [Space, LineSeparator, ParagraphSeparator]
            || x `elem` ("\t\n\v\f\r\x85" :: String)

    it "whitespace spans both a Unicode space and an ASCII control whitespace" $ do
      seen <- sample 4000 Alphabet.whitespace
      Set.member ' ' seen `shouldBe` True
      Set.member '\t' seen `shouldBe` True

    it "combiningMarks only draws combining-mark characters" $
      prop (Alphabet.combiningMarks & Gen.build) $ \c ->
        generalCategory c `shouldSatisfy` (`elem` [NonSpacingMark, SpacingCombiningMark, EnclosingMark])

  describe "finite alphabets span their whole documented set" do
    mapM_
      ( \(name, builder, expected) -> it name do
          seen <- sample 4000 builder
          seen `shouldBe` Set.fromList expected
      )
      [ ("hexit", Alphabet.hexit, "0123456789abcdefABCDEF"),
        ("asciiPunctuation", Alphabet.asciiPunctuation, "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"),
        ("base64", Alphabet.base64, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"),
        ("base64Url", Alphabet.base64Url, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"),
        ("uriUnreserved", Alphabet.uriUnreserved, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~"),
        ("zeroWidth", Alphabet.zeroWidth, "\x200B\x200C\x200D\x2060\xFEFF"),
        ("bidiControls", Alphabet.bidiControls, "\x200E\x200F\x202A\x202B\x202C\x202D\x202E\x2066\x2067\x2068\x2069")
      ]

  describe "Alphabet.only" do
    it "draws only the given characters" $
      prop (Alphabet.only "xyz" & Gen.build) $
        \c -> c `shouldSatisfy` (`elem` ("xyz" :: String))

    it "spans every given character" $ do
      seen <- sample 500 (Alphabet.only "xyz")
      seen `shouldBe` Set.fromList "xyz"

  describe "Alphabet.ranges" do
    it "draws only characters within the given ranges" $
      prop (Alphabet.ranges [(0x41, 0x42), (0x61, 0x62)] & Gen.build) $ \c ->
        c `shouldSatisfy` (`elem` ("ABab" :: String))

    it "spans every given range" $ do
      seen <- sample 500 (Alphabet.ranges [(0x41, 0x42), (0x61, 0x62)])
      seen `shouldBe` Set.fromList "ABab"
