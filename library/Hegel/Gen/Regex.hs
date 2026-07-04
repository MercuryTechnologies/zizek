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
  )
where

import Data.Text (Text)
import Hegel.Gen.Builder (Build (..))
import Hegel.Gen.Char (CharBuilder, HasAlphabet (..), buildCharTextGen)
import Hegel.Gen.Internal.String (stringGen)
import Hegel.Internal.DataSource (buildRegexGen)

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
instance HasAlphabet RegexBuilder where
  alphabet cb b = b {bAlphabet = Just cb}

instance Build RegexBuilder Text where
  build b = stringGen gen
    where
      -- The alphabet, if any, uses the same single-character bounds
      -- 'Hegel.Gen.Char' uses; see 'buildCharTextGen' for why.
      --
      -- The engine clones the alphabet's intervals rather than retaining the
      -- handle, so composing its construction into this one action, with
      -- nothing else holding a reference afterward, is safe.
      gen = traverse (buildCharTextGen 1 1) b.bAlphabet >>= buildRegexGen b.bPattern b.bFullMatch
