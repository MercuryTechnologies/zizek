module Hegel.Gen.Integer
  ( IntegerBuilder,
    integer,
  )
where

import CBOR.Value (Value (..))
import Data.Maybe (fromMaybe)
import Hegel.Gen.Builder (Build (..), HasMax (..), HasMin (..))
import Hegel.Gen.Internal (pattern Schema)
import Hegel.Protocol.Cbor (ParseError (..), buildMap, intVal, textVal)

data IntegerBuilder a = IntegerBuilder
  { bMin :: Maybe a,
    bMax :: Maybe a
  }

integer :: IntegerBuilder a
integer = IntegerBuilder {bMin = Nothing, bMax = Nothing}

instance HasMin (IntegerBuilder a) a where
  min lo b = b {bMin = Just lo}

instance HasMax (IntegerBuilder a) a where
  max hi b = b {bMax = Just hi}

instance (Bounded a, Integral a) => Build (IntegerBuilder a) a where
  build b =
    let lo = fromMaybe minBound b.bMin
        hi = fromMaybe maxBound b.bMax
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
