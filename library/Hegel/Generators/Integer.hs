module Hegel.Generators.Integer
  ( IntegerGenerator,
    gen,
    integers,
    withRange,
    minValue,
    maxValue,
  )
where

import CBOR.Value (Value (..))
import Data.Maybe (fromMaybe)
import Hegel.Generators (Generator, pattern Schema)
import Hegel.Protocol.Cbor (ParseError (..), buildMap, intVal, textVal)

data IntegerGenerator a = IntegerGenerator
  { minVal :: Maybe a,
    maxVal :: Maybe a
  }

integers :: IntegerGenerator a
integers = IntegerGenerator Nothing Nothing

withRange :: (a, a) -> IntegerGenerator a -> IntegerGenerator a
withRange (lo, hi) c = c {minVal = Just lo, maxVal = Just hi}

minValue :: a -> IntegerGenerator a -> IntegerGenerator a
minValue v c = c {minVal = Just v}

maxValue :: a -> IntegerGenerator a -> IntegerGenerator a
maxValue v c = c {maxVal = Just v}

gen :: forall a. (Bounded a, Integral a) => IntegerGenerator a -> Generator a
gen cfg =
  let lo = fromMaybe minBound cfg.minVal
      hi = fromMaybe maxBound cfg.maxVal
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
