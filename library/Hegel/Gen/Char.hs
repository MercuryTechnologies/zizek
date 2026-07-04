-- | 'Char' generator.
--
-- Generates a single Unicode character (surrogates excluded):
--
-- > Gen.char & Gen.build
--
-- To restrict the character set, for example when providing an alphabet
-- to 'Hegel.Gen.Regex.alphabet', use the modifier functions:
--
-- > Gen.char & Gen.minCodepoint 97 & Gen.maxCodepoint 122 & Gen.build
module Hegel.Gen.Char
  ( -- * Builder
    CharBuilder,
    char,

    -- * Codec
    Codec (..),

    -- * Modifiers
    codec,
    minCodepoint,
    maxCodepoint,
    categories,
    excludeCategories,
    includeCharacters,
    excludeCharacters,
    HasAlphabet (..),

    -- * Internal
    buildCharTextGen,
  )
where

import Control.Exception (throwIO)
import Data.Char (GeneralCategory (..))
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32, Word64)
import Foreign.ForeignPtr (ForeignPtr)
import Hegel.Gen.Builder (Build (..), checkNonNegative, checkOrderedMaybe)
import Hegel.Gen.Internal.String (stringDraw)
import Hegel.Internal.DataSource (HegelStringGenerator, InvariantViolation (..), TextSpec (..), buildTextGen)
import Hegel.Internal.TestCase (TestCase)

-- | Which base range a text\/char\/regex-alphabet draw's alphabet starts
-- from, before codepoint bounds and category filters narrow it further.
-- 'Utf8' (the default) imposes no restriction — all of Unicode.
data Codec = Ascii | Latin1 | Utf8
  deriving stock (Show, Eq)

-- | The @codec@ string @hegel_string_generator_text@ expects, or 'Nothing'
-- for 'Utf8' (equivalent to passing @NULL@\/@"utf-8"@).
codecArg :: Codec -> Maybe Text
codecArg Ascii = Just "ascii"
codecArg Latin1 = Just "latin-1"
codecArg Utf8 = Nothing

-- | Builder for a single Unicode character. Character constraints are
-- optional; absent fields impose no restriction beyond surrogate exclusion.
data CharBuilder = CharBuilder
  { bCodec :: !Codec,
    bMinCodepoint :: !(Maybe Int),
    bMaxCodepoint :: !(Maybe Int),
    bCategories :: !(Maybe [GeneralCategory]),
    bExcludeCategories :: !(Maybe [GeneralCategory]),
    bIncludeCharacters :: !(Maybe Text),
    bExcludeCharacters :: !(Maybe Text),
    -- Track whether 'categories' was set; when True, Cs auto-injection is
    -- suppressed (categories and excludeCategories are mutually exclusive).
    bCategoriesExplicit :: !Bool
  }

-- | Generate a random Unicode character.
char :: CharBuilder
char =
  CharBuilder
    { bCodec = Utf8,
      bMinCodepoint = Nothing,
      bMaxCodepoint = Nothing,
      bCategories = Nothing,
      bExcludeCategories = Nothing,
      bIncludeCharacters = Nothing,
      bExcludeCharacters = Nothing,
      bCategoriesExplicit = False
    }

-- | Restrict to characters encodable in the given codec.
codec :: Codec -> CharBuilder -> CharBuilder
codec c b = b {bCodec = c}

-- | Set the minimum Unicode codepoint (inclusive).
minCodepoint :: Int -> CharBuilder -> CharBuilder
minCodepoint n b = b {bMinCodepoint = Just n}

-- | Set the maximum Unicode codepoint (inclusive).
maxCodepoint :: Int -> CharBuilder -> CharBuilder
maxCodepoint n b = b {bMaxCodepoint = Just n}

-- | Restrict to characters from these Unicode general categories
-- (e.g. @[LowercaseLetter, UppercaseLetter]@). Mutually exclusive with
-- 'excludeCategories'.
categories :: [GeneralCategory] -> CharBuilder -> CharBuilder
categories cs b = b {bCategories = Just cs, bCategoriesExplicit = True}

-- | Exclude characters from these Unicode general categories.
-- Mutually exclusive with 'categories'.
excludeCategories :: [GeneralCategory] -> CharBuilder -> CharBuilder
excludeCategories cs b = b {bExcludeCategories = Just cs}

-- | Always include these characters even if excluded by other filters.
includeCharacters :: Text -> CharBuilder -> CharBuilder
includeCharacters t b = b {bIncludeCharacters = Just t}

-- | Always exclude these characters.
excludeCharacters :: Text -> CharBuilder -> CharBuilder
excludeCharacters t b = b {bExcludeCharacters = Just t}

-- | Builders that accept a character-set restriction.
class HasAlphabet b where
  alphabet :: CharBuilder -> b -> b

-- | The two-letter Unicode general-category abbreviation
-- @hegel_string_generator_text@'s @categories@\/@exclude_categories@ expect.
categoryCode :: GeneralCategory -> Text
categoryCode UppercaseLetter = "Lu"
categoryCode LowercaseLetter = "Ll"
categoryCode TitlecaseLetter = "Lt"
categoryCode ModifierLetter = "Lm"
categoryCode OtherLetter = "Lo"
categoryCode NonSpacingMark = "Mn"
categoryCode SpacingCombiningMark = "Mc"
categoryCode EnclosingMark = "Me"
categoryCode DecimalNumber = "Nd"
categoryCode LetterNumber = "Nl"
categoryCode OtherNumber = "No"
categoryCode ConnectorPunctuation = "Pc"
categoryCode DashPunctuation = "Pd"
categoryCode OpenPunctuation = "Ps"
categoryCode ClosePunctuation = "Pe"
categoryCode InitialQuote = "Pi"
categoryCode FinalQuote = "Pf"
categoryCode OtherPunctuation = "Po"
categoryCode MathSymbol = "Sm"
categoryCode CurrencySymbol = "Sc"
categoryCode ModifierSymbol = "Sk"
categoryCode OtherSymbol = "So"
categoryCode Space = "Zs"
categoryCode LineSeparator = "Zl"
categoryCode ParagraphSeparator = "Zp"
categoryCode Control = "Cc"
categoryCode Format = "Cf"
categoryCode Surrogate = "Cs"
categoryCode PrivateUse = "Co"
categoryCode NotAssigned = "Cn"

-- | Build the @hegel_string_generator_text@ handle for a 'CharBuilder' at
-- the given size bounds. 'Hegel.Gen.Char' materializes single characters
-- with @minSize = maxSize = 1@; 'Hegel.Gen.Regex's @alphabet@ modifier
-- reuses this (at the same bounds — only the character set matters for an
-- alphabet) to build the alphabet's text generator.
--
-- Validates 'bMinCodepoint'\/'bMaxCodepoint' here, the single point every
-- caller routes through, before marshalling.
buildCharTextGen :: Word64 -> Word64 -> CharBuilder -> IO (ForeignPtr HegelStringGenerator)
buildCharTextGen minSz maxSz b = do
  checkOrderedMaybe "Hegel.Gen.Char" b.bMinCodepoint b.bMaxCodepoint
  mapM_ (checkNonNegative "Hegel.Gen.Char") b.bMinCodepoint
  mapM_ (checkNonNegative "Hegel.Gen.Char") b.bMaxCodepoint
  buildTextGen
    TextSpec
      { minSize = minSz,
        maxSize = maxSz,
        codec = codecArg b.bCodec,
        minCodepoint = maybe 0 fromIntegral b.bMinCodepoint,
        maxCodepoint = maybe maxBound fromIntegral b.bMaxCodepoint :: Word32,
        categories = fmap (fmap categoryCode) b.bCategories,
        excludeCategories = exclCats,
        includeCharacters = b.bIncludeCharacters,
        excludeCharacters = b.bExcludeCharacters
      }
  where
    -- Haskell 'Text' cannot represent lone surrogates, so exclude them by
    -- default unless the caller explicitly took over category filtering
    -- with 'categories'.
    exclCats
      | b.bCategoriesExplicit = fmap (fmap categoryCode) b.bExcludeCategories
      | otherwise = Just (fmap categoryCode (nub (Surrogate : fromMaybe [] b.bExcludeCategories)))

instance Build CharBuilder Char where
  build b = stringDraw (buildCharTextGen 1 1 b) postProcess
    where
      postProcess :: TestCase -> Text -> IO Char
      postProcess _tc t = case T.uncons t of
        Just (c, _) -> pure c
        Nothing ->
          throwIO
            InvariantViolation
              { detail = "libhegel: a minSize=maxSize=1 text draw returned an empty string"
              }
