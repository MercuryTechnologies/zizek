-- | 'Text' generator.
--
-- Bounded length via 'Hegel.Gen.Builder.minSize' and 'Hegel.Gen.Builder.maxSize':
--
-- > Gen.text & Gen.minSize 1 & Gen.maxSize 64 & Gen.build
--
-- Surrogates are excluded by default (the server returns UTF-8; lone
-- surrogates are not valid UTF-8 and cannot be stored in 'Data.Text.Text').
module Hegel.Gen.Text
  ( TextBuilder,
    text,
  )
where

import CBOR.Value (Value (..))
import Data.Text (Text)
import Hegel.Gen.Builder (Build (..), HasSize (..))
import Hegel.Gen.Internal (basic)
import Hegel.Protocol.Cbor (ParseError (..), hegelText)
import Hegel.Schema (TextSchema (maxSize))
import Hegel.Schema qualified as Schema

data TextBuilder = TextBuilder
  { bMinSize :: !Int,
    bMaxSize :: !(Maybe Int)
  }

-- | Generate a random 'Text' value.
text :: TextBuilder
text = TextBuilder {bMinSize = 0, bMaxSize = Nothing}

instance HasSize TextBuilder where
  minSize n b = b {bMinSize = n}
  maxSize n b = b {bMaxSize = Just n}

instance Build TextBuilder Text where
  build b =
    basic
      ((Schema.text b.bMinSize) {maxSize = b.bMaxSize})
      parseText

parseText :: Value -> Either ParseError Text
parseText = hegelText
