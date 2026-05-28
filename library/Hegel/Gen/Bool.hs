-- | Boolean generator.
module Hegel.Gen.Bool
  ( BoolBuilder,
    bool,
  )
where

import CBOR.Value (Value (..))
import Hegel.Gen.Builder (Build (..))
import Hegel.Gen.Internal (pattern Schema)
import Hegel.Protocol.Cbor (ParseError (..), buildMap, textVal)

data BoolBuilder = BoolBuilder

-- | Generate a random boolean.
bool :: BoolBuilder
bool = BoolBuilder

instance Build BoolBuilder Bool where
  build _ = Schema (buildMap [("type", textVal "boolean")]) parseBool

parseBool :: Value -> Either ParseError Bool
parseBool (Bool b) = Right b
parseBool v = Left ParseError {expected = "boolean", got = v}
