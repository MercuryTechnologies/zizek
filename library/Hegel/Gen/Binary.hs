-- | 'ByteString' generator.
--
-- Bounded length via 'Hegel.Gen.Builder.minSize' and 'Hegel.Gen.Builder.maxSize':
--
-- > Gen.binary & Gen.minSize 4 & Gen.maxSize 64 & Gen.build
module Hegel.Gen.Binary
  ( BinaryBuilder,
    binary,
  )
where

import CBOR.Value (Value (..))
import Data.ByteString (ByteString)
import Hegel.Gen.Builder (Build (..), HasSize (..))
import Hegel.Gen.Internal (basic)
import Hegel.Protocol.Cbor (ParseError (..))
import Hegel.Schema qualified as Schema

data BinaryBuilder = BinaryBuilder
  { bMinSize :: !Int,
    bMaxSize :: !(Maybe Int)
  }

-- | Generate a random 'ByteString'.
binary :: BinaryBuilder
binary = BinaryBuilder {bMinSize = 0, bMaxSize = Nothing}

instance HasSize BinaryBuilder where
  minSize n b = b {bMinSize = n}
  maxSize n b = b {bMaxSize = Just n}

instance Build BinaryBuilder ByteString where
  build b = basic (Schema.binary b.bMinSize b.bMaxSize) parseBinary

parseBinary :: Value -> Either ParseError ByteString
parseBinary (ByteString bs) = Right bs
parseBinary v = Left ParseError {expected = "bytes", got = v}
