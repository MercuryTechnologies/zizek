module Hegel.Generators.Bool
  ( gen,
  )
where

import CBOR.Value (Value (..))
import Hegel.Generators (Generator, pattern Schema)
import Hegel.Protocol.Cbor (ParseError (..), buildMap, textVal)

gen :: Generator Bool
gen = Schema (buildMap [("type", textVal "boolean")]) parseBool

parseBool :: Value -> Either ParseError Bool
parseBool (Bool b) = Right b
parseBool v = Left ParseError {expected = "boolean", got = v}
