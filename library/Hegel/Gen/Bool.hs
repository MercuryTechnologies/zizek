module Hegel.Gen.Bool
  ( bool,
  )
where

import CBOR.Value (Value (..))
import Hegel.Gen.Internal (Generator, pattern Schema)
import Hegel.Protocol.Cbor (ParseError (..), buildMap, textVal)

bool :: Generator Bool
bool = Schema (buildMap [("type", textVal "boolean")]) parseBool

parseBool :: Value -> Either ParseError Bool
parseBool (Bool b) = Right b
parseBool v = Left ParseError {expected = "boolean", got = v}
