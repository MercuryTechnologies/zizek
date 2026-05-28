-- | Boolean generator.
module Hegel.Gen.Bool
  ( BoolBuilder,
    bool,
  )
where

import CBOR.Value (Value (..))
import Hegel.Gen.Builder (Build (..))
import Hegel.Gen.Internal (basic)
import Hegel.Protocol.Cbor (ParseError (..))
import Hegel.Schema qualified as Schema

data BoolBuilder = BoolBuilder

-- | Generate a random boolean.
bool :: BoolBuilder
bool = BoolBuilder

instance Build BoolBuilder Bool where
  build _ = basic Schema.bool parseBool

parseBool :: Value -> Either ParseError Bool
parseBool (Bool b) = Right b
parseBool v = Left ParseError {expected = "boolean", got = v}
