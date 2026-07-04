-- | 'Text' generator.
--
-- Bounded length via 'Hegel.Gen.Builder.minSize' and 'Hegel.Gen.Builder.maxSize':
--
-- > Gen.text & Gen.minSize 1 & Gen.maxSize 64 & Gen.build
--
-- Surrogates are excluded by default, since 'Data.Text.Text' cannot
-- represent them. Restrict the character set with an alphabet:
--
-- > Gen.text & Gen.alphabet (Gen.char & Gen.categories [LowercaseLetter]) & Gen.build
module Hegel.Gen.Text
  ( TextBuilder,
    text,
  )
where

import Data.Text (Text)
import Hegel.Gen.Builder (Build (..), HasSize (..), checkSizeBounds)
import Hegel.Gen.Char (CharBuilder, HasAlphabet (..), buildCharTextGen)
import Hegel.Gen.Internal (Gen (..), draw)
import Hegel.Gen.Internal.String (stringGen)
import Hegel.Internal.DataSource (TextSpec (..), buildTextGen)

data TextBuilder = TextBuilder
  { bMinSize :: !Int,
    bMaxSize :: !(Maybe Int),
    bAlphabet :: !(Maybe CharBuilder)
  }

-- | Generate a random 'Text' value.
text :: TextBuilder
text = TextBuilder {bMinSize = 0, bMaxSize = Nothing, bAlphabet = Nothing}

instance HasSize TextBuilder where
  minSize n b = b {bMinSize = n}
  maxSize n b = b {bMaxSize = Just n}

-- | Restrict the generated characters to those described by the given
-- 'CharBuilder', at this builder's own size bounds.
instance HasAlphabet TextBuilder where
  alphabet cb b = b {bAlphabet = Just cb}

instance Build TextBuilder Text where
  build b = Draw \tc -> do
    checkSizeBounds "Hegel.Gen.Text" b.bMinSize b.bMaxSize
    draw tc textGen
    where
      -- 'textGen' must stay bound here, outside the 'Draw' lambda above, so
      -- 'stringGen' builds its handle once and shares it across every draw
      -- of this 'Gen' value.
      textGen = stringGen gen
      wireLo = fromIntegral b.bMinSize
      wireHi = maybe maxBound fromIntegral b.bMaxSize
      gen = case b.bAlphabet of
        -- No codec\/codepoint\/category restriction beyond excluding
        -- surrogates, which 'Data.Text.Text' cannot represent.
        Nothing ->
          buildTextGen
            TextSpec
              { minSize = wireLo,
                maxSize = wireHi,
                codec = Nothing,
                minCodepoint = 0,
                maxCodepoint = maxBound,
                categories = Nothing,
                excludeCategories = Just ["Cs"],
                includeCharacters = Nothing,
                excludeCharacters = Nothing
              }
        Just cb -> buildCharTextGen wireLo wireHi cb
