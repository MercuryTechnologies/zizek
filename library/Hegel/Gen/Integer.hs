module Hegel.Gen.Integer
  ( IntegerOptions (..),
    defaultIntegerOptions,
    integer,
    boundedIntegers,
    integerWith,
  )
where

import CBOR.Value (Value (..))
import Data.Maybe (fromMaybe)
import Hegel.Gen.Internal (Generator, pattern Schema)
import Hegel.Protocol.Cbor (ParseError (..), buildMap, intVal, textVal)
import Hegel.Range (Range (..))

data IntegerOptions a = IntegerOptions
  { minValue :: Maybe a,
    maxValue :: Maybe a
  }

defaultIntegerOptions :: IntegerOptions a
defaultIntegerOptions = IntegerOptions {minValue = Nothing, maxValue = Nothing}

-- | Generate an integer in the given range (inclusive on both ends).
integer :: forall a. (Bounded a, Integral a) => Range a -> Generator a
integer (Range lo hi) =
  Schema
    ( buildMap
        [ ("type", textVal "integer"),
          ("min_value", intVal lo),
          ("max_value", intVal hi)
        ]
    )
    parseInteger

-- | Generate any integer in the type's full 'Bounded' range.
boundedIntegers :: forall a. (Bounded a, Integral a) => Generator a
boundedIntegers = integer (Range minBound maxBound)

-- | Generate an integer with optional bounds; unset bounds fall back to
-- 'minBound' / 'maxBound' for the type.
integerWith :: forall a. (Bounded a, Integral a) => IntegerOptions a -> Generator a
integerWith opts =
  let lo = fromMaybe minBound opts.minValue
      hi = fromMaybe maxBound opts.maxValue
   in Schema
        ( buildMap
            [ ("type", textVal "integer"),
              ("min_value", intVal lo),
              ("max_value", intVal hi)
            ]
        )
        parseInteger

parseInteger :: (Integral a) => Value -> Either ParseError a
parseInteger (UInt n) = Right (fromIntegral n)
parseInteger (NInt n) = Right (fromIntegral (negate (fromIntegral n :: Integer) - 1))
parseInteger v = Left ParseError {expected = "integer", got = v}
