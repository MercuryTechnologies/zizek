-- | 'Char' generator.
--
-- Generates a single Unicode character (surrogates excluded):
--
-- > Gen.char & Gen.build
--
-- To restrict the character set — for example when providing an alphabet
-- to 'Hegel.Gen.Regex.alphabet' — use the modifier functions:
--
-- > Gen.char & Gen.minCodepoint 97 & Gen.maxCodepoint 122 & Gen.build
module Hegel.Gen.Char
  ( -- * Builder
    CharBuilder,
    char,

    -- * Modifiers
    codec,
    minCodepoint,
    maxCodepoint,
    categories,
    excludeCategories,
    includeCharacters,
    excludeCharacters,

    -- * Internal
    toCharacterFields,
  )
where

import CBOR.Value (Value (..))
import Data.List (nub)
import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Gen.Builder (Build (..))
import Hegel.Gen.Internal (basic)
import Hegel.Protocol.Cbor (ParseError (..), hegelText)
import Hegel.Schema (CharacterFields (..), TextSchema (..), defaultCharacterFields)

-- | Builder for a single Unicode character. Character constraints are
-- optional; absent fields impose no restriction beyond surrogate exclusion.
data CharBuilder = CharBuilder
  { bCodec :: !(Maybe Text),
    bMinCodepoint :: !(Maybe Int),
    bMaxCodepoint :: !(Maybe Int),
    bCategories :: !(Maybe [Text]),
    bExcludeCategories :: !(Maybe [Text]),
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
    { bCodec = Nothing,
      bMinCodepoint = Nothing,
      bMaxCodepoint = Nothing,
      bCategories = Nothing,
      bExcludeCategories = Nothing,
      bIncludeCharacters = Nothing,
      bExcludeCharacters = Nothing,
      bCategoriesExplicit = False
    }

-- | Restrict to characters encodable in the given codec (e.g. @"ascii"@).
codec :: Text -> CharBuilder -> CharBuilder
codec c b = b {bCodec = Just c}

-- | Set the minimum Unicode codepoint (inclusive).
minCodepoint :: Int -> CharBuilder -> CharBuilder
minCodepoint n b = b {bMinCodepoint = Just n}

-- | Set the maximum Unicode codepoint (inclusive).
maxCodepoint :: Int -> CharBuilder -> CharBuilder
maxCodepoint n b = b {bMaxCodepoint = Just n}

-- | Restrict to characters from these Unicode general categories
-- (e.g. @["Ll", "Lu"]@). Mutually exclusive with 'excludeCategories'.
categories :: [Text] -> CharBuilder -> CharBuilder
categories cs b = b {bCategories = Just cs, bCategoriesExplicit = True}

-- | Exclude characters from these Unicode general categories.
-- Mutually exclusive with 'categories'.
excludeCategories :: [Text] -> CharBuilder -> CharBuilder
excludeCategories cs b = b {bExcludeCategories = Just cs}

-- | Always include these characters even if excluded by other filters.
includeCharacters :: Text -> CharBuilder -> CharBuilder
includeCharacters t b = b {bIncludeCharacters = Just t}

-- | Always exclude these characters.
excludeCharacters :: Text -> CharBuilder -> CharBuilder
excludeCharacters t b = b {bExcludeCharacters = Just t}

-- | Convert a 'CharBuilder' to 'CharacterFields', injecting @\"Cs\"@ into
-- 'excludeCategories' unless 'categories' was explicitly set.
toCharacterFields :: CharBuilder -> CharacterFields
toCharacterFields b =
  injectCs
    b.bCategoriesExplicit
    defaultCharacterFields
      { codec = b.bCodec,
        minCodepoint = b.bMinCodepoint,
        maxCodepoint = b.bMaxCodepoint,
        categories = b.bCategories,
        excludeCategories = b.bExcludeCategories,
        includeCharacters = b.bIncludeCharacters,
        excludeCharacters = b.bExcludeCharacters
      }

-- Inject "Cs" into excludeCategories when categories is not explicitly set.
injectCs :: Bool -> CharacterFields -> CharacterFields
injectCs True cf = cf
injectCs False cf =
  cf
    { excludeCategories =
        Just $ nub $ "Cs" : maybe [] id cf.excludeCategories
    }

instance Build CharBuilder Char where
  build b =
    basic
      TextSchema
        { minSize = 1,
          maxSize = Just 1,
          charFields = toCharacterFields b
        }
      parseChar

parseChar :: Value -> Either ParseError Char
parseChar v = case hegelText v of
  Left err -> Left err
  Right t
    | T.null t -> Left ParseError {expected = "non-empty string", got = v}
    | otherwise -> Right (T.head t)
