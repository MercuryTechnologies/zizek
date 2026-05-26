module Hegel.Gen.Binary
  ( BinaryOptions (..),
    defaultBinaryOptions,
    binary,
    binaryWith,
  )
where

import CBOR.Value (Value (..))
import Data.ByteString (ByteString)
import Hegel.Gen.Internal (Generator, pattern Schema)
import Hegel.Protocol.Cbor (ParseError (..), buildMap, intVal, textVal)
import Hegel.Range (Range (..))

data BinaryOptions = BinaryOptions
  { minSize :: !Int,
    maxSize :: !(Maybe Int)
  }

defaultBinaryOptions :: BinaryOptions
defaultBinaryOptions = BinaryOptions {minSize = 0, maxSize = Nothing}

-- | Generate a 'ByteString' whose length falls within the given range.
binary :: Range Int -> Generator ByteString
binary (Range lo hi) = binaryWith BinaryOptions {minSize = lo, maxSize = Just hi}

-- | Generate a 'ByteString' with the given options.
binaryWith :: BinaryOptions -> Generator ByteString
binaryWith opts =
  let pairs =
        [ ("type", textVal "binary"),
          ("min_size", intVal opts.minSize)
        ]
          ++ foldMap (\hi -> [("max_size", intVal hi)]) opts.maxSize
   in Schema (buildMap pairs) parseBinary

parseBinary :: Value -> Either ParseError ByteString
parseBinary (ByteString bs) = Right bs
parseBinary v = Left ParseError {expected = "bytes", got = v}
