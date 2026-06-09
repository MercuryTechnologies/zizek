-- | 'Text' generator from a regular expression pattern.
--
-- > Gen.regex "[a-z]+" & Gen.build
--
-- Enable full-match mode and restrict the character set with modifiers:
--
-- > Gen.regex "^[a-z]+$" & Gen.fullMatch & Gen.build
-- > Gen.regex "[a-z]+"   & Gen.alphabet (Gen.char & Gen.minCodepoint 97 & Gen.maxCodepoint 122) & Gen.build
module Hegel.Gen.Regex
  ( -- * Builder
    RegexBuilder,
    regex,

    -- * Modifiers
    fullMatch,
    alphabet,
  )
where

import CBOR.Value (Value (..))
import Data.Text (Text)
import Hegel.Gen.Builder (Build (..))
import Hegel.Gen.Char (CharBuilder, toCharacterFields)
import Hegel.Gen.Internal (basic)
import Hegel.Protocol.Cbor (ParseError (..), hegelText)
import Hegel.Schema qualified as Schema

-- | Builder for a regex-constrained 'Text' generator.
data RegexBuilder = RegexBuilder
  { bPattern :: !Text,
    bFullMatch :: !Bool,
    bAlphabet :: !(Maybe CharBuilder)
  }

-- | Generate a random 'Text' matching the given regular expression.
regex :: Text -> RegexBuilder
regex p = RegexBuilder {bPattern = p, bFullMatch = False, bAlphabet = Nothing}

-- | Require the entire generated string to match the pattern.
fullMatch :: RegexBuilder -> RegexBuilder
fullMatch b = b {bFullMatch = True}

-- | Restrict the generated characters to those described by the given
-- 'CharBuilder'. Equivalent to @hegel@'s @alphabet@ parameter.
alphabet :: CharBuilder -> RegexBuilder -> RegexBuilder
alphabet cb b = b {bAlphabet = Just cb}

instance Build RegexBuilder Text where
  build b =
    basic
      (Schema.regex b.bPattern)
        { Schema.fullmatch = b.bFullMatch,
          Schema.alphabet = toCharacterFields <$> b.bAlphabet
        }
      parseText

parseText :: Value -> Either ParseError Text
parseText = hegelText
