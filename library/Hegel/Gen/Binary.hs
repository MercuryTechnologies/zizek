module Hegel.Gen.Binary
  ( BinaryBuilder,
    binary,
  )
where

import CBOR.Value (Value (..))
import Data.ByteString (ByteString)
import Hegel.Gen.Builder (Build (..), HasSize (..))
import Hegel.Gen.Internal (pattern Schema)
import Hegel.Protocol.Cbor (ParseError (..), buildMap, intVal, textVal)

data BinaryBuilder = BinaryBuilder
  { bMinSize :: !Int,
    bMaxSize :: !(Maybe Int)
  }

binary :: BinaryBuilder
binary = BinaryBuilder {bMinSize = 0, bMaxSize = Nothing}

instance HasSize BinaryBuilder where
  minSize n b = b {bMinSize = n}
  maxSize n b = b {bMaxSize = Just n}

instance Build BinaryBuilder ByteString where
  build b =
    let pairs =
          [ ("type", textVal "binary"),
            ("min_size", intVal b.bMinSize)
          ]
            ++ foldMap (\hi -> [("max_size", intVal hi)]) b.bMaxSize
     in Schema (buildMap pairs) parseBinary

parseBinary :: Value -> Either ParseError ByteString
parseBinary (ByteString bs) = Right bs
parseBinary v = Left ParseError {expected = "bytes", got = v}
