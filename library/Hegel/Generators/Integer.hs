module Hegel.Generators.Integer
  ( IntegerGenerator
  , integers
  , minValue
  , maxValue
  , withRange
  ) where

import CBOR.Value (Value (..))
import Hegel.Generators (BasicGenerator (..), Generator (..))
import Hegel.Protocol.Cbor (ParseError (..), buildMap, intVal, textVal)

data IntegerGenerator a = IntegerGenerator
  { lower :: !(Maybe a)
  , upper :: !(Maybe a)
  }

integers :: IntegerGenerator a
integers = IntegerGenerator {lower = Nothing, upper = Nothing}

minValue :: a -> IntegerGenerator a -> IntegerGenerator a
minValue v g = g {lower = Just v}

maxValue :: a -> IntegerGenerator a -> IntegerGenerator a
maxValue v g = g {upper = Just v}

withRange :: IntegerGenerator a -> (a, a) -> IntegerGenerator a
withRange g (lo, hi) = minValue lo (maxValue hi g)

instance (Bounded a, Integral a) => Generator (IntegerGenerator a) where
  type Output (IntegerGenerator a) = a

  asBasic g =
    Just BasicGenerator
      { schema =
          buildMap
            [ ("type", textVal "integer")
            , ("min_value", intVal lo)
            , ("max_value", intVal hi)
            ]
      , parse = parseInteger
      }
    where
      lo = maybe minBound id g.lower
      hi = maybe maxBound id g.upper

parseInteger :: (Integral a) => Value -> Either ParseError a
parseInteger (UInt n) = Right (fromIntegral n)
parseInteger (NInt n) = Right (fromIntegral (negate (fromIntegral n :: Integer) - 1))
parseInteger v        = Left ParseError {expected = "integer", got = v}
