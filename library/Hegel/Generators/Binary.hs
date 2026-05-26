module Hegel.Generators.Binary
  ( BinaryGenerator (..),
    gen,
    binary,
  )
where

import CBOR.Value (Value (..))
import Data.ByteString (ByteString)
import Hegel.Generators (Generator, pattern Schema)
import Hegel.Protocol.Cbor (ParseError (..), buildMap, intVal, textVal)

data BinaryGenerator = BinaryGenerator
  { minSize :: !Int,
    maxSize :: !(Maybe Int)
  }

binary :: BinaryGenerator
binary = BinaryGenerator {minSize = 0, maxSize = Nothing}

gen :: BinaryGenerator -> Generator ByteString
gen cfg =
  let pairs =
        [ ("type", textVal "binary"),
          ("min_size", intVal cfg.minSize)
        ]
          ++ foldMap (\hi -> [("max_size", intVal hi)]) cfg.maxSize
   in Schema (buildMap pairs) parseBinary

parseBinary :: Value -> Either ParseError ByteString
parseBinary (ByteString bs) = Right bs
parseBinary v = Left ParseError {expected = "bytes", got = v}
