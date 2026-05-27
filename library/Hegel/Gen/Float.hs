module Hegel.Gen.Float
  ( FloatBuilder,
    float,
    double,
    exclusiveMin,
    exclusiveMax,
    disallowNan,
    disallowInfinity,
  )
where

import CBOR.Value (Value (..))
import Data.Maybe (isJust)
import GHC.Float (double2Float, float2Double)
import Hegel.Gen.Builder (Build (..), HasMax (..), HasMin (..))
import Hegel.Gen.Internal (pattern Schema)
import Hegel.Protocol.Cbor
  ( ParseError (..),
    boolVal,
    buildMap,
    doubleVal,
    floatVal,
    intVal,
    textVal,
  )

data FloatBuilder a = FloatBuilder
  { bMin :: Maybe a,
    bMax :: Maybe a,
    bExclMin :: Bool,
    bExclMax :: Bool,
    bAllowNan :: Bool,
    bAllowInf :: Bool
  }

defaultFloatBuilder :: FloatBuilder a
defaultFloatBuilder =
  FloatBuilder
    { bMin = Nothing,
      bMax = Nothing,
      bExclMin = False,
      bExclMax = False,
      bAllowNan = True,
      bAllowInf = True
    }

float :: FloatBuilder Float
float = defaultFloatBuilder

double :: FloatBuilder Double
double = defaultFloatBuilder

exclusiveMin :: FloatBuilder a -> FloatBuilder a
exclusiveMin b = b {bExclMin = True}

exclusiveMax :: FloatBuilder a -> FloatBuilder a
exclusiveMax b = b {bExclMax = True}

disallowNan :: FloatBuilder a -> FloatBuilder a
disallowNan b = b {bAllowNan = False}

disallowInfinity :: FloatBuilder a -> FloatBuilder a
disallowInfinity b = b {bAllowInf = False}

instance HasMin (FloatBuilder a) a where
  min lo b = b {bMin = Just lo}

instance HasMax (FloatBuilder a) a where
  max hi b = b {bMax = Just hi}

-- Use concrete GHC.Float primitives (float2Double / double2Float) rather than
-- realToFrac here. realToFrac routes through Rational (toRational then
-- fromRational), which calls decodeFloat on the bit pattern and treats NaN/Inf
-- as finite values — NaN becomes 1.5×2^128 and ±Inf becomes ±2^128. cbor2
-- always encodes Python float('nan') and float('inf') as CBOR Float16, so
-- even a Double generator receives a Float16 (NaN :: Float) off the wire.
instance Build (FloatBuilder Float) Float where
  build b = Schema (buildSchema floatVal 32 b) parse
    where
      parse (Float16 f) = Right f
      parse (Float32 f) = Right f
      parse (Float64 d) = Right (double2Float d)
      parse v = Left ParseError {expected = "float", got = v}

instance Build (FloatBuilder Double) Double where
  build b = Schema (buildSchema doubleVal 64 b) parse
    where
      parse (Float16 f) = Right (float2Double f)
      parse (Float32 f) = Right (float2Double f)
      parse (Float64 d) = Right d
      parse v = Left ParseError {expected = "float", got = v}

maxFiniteVal :: forall a. (RealFloat a) => a
maxFiniteVal =
  let b = floatRadix (0 :: a)
      p = floatDigits (0 :: a)
      (_, eMax) = floatRange (0 :: a)
   in encodeFloat (b ^ p - 1) (eMax - p)

buildSchema :: forall a. (RealFloat a) => (a -> Value) -> Int -> FloatBuilder a -> Value
buildSchema toVal width b =
  buildMap $
    [ ("type", textVal "float"),
      ("width", intVal width),
      ("exclude_min", boolVal b.bExclMin),
      ("exclude_max", boolVal b.bExclMax),
      ("allow_nan", boolVal effectiveAllowNan),
      ("allow_infinity", boolVal effectiveAllowInf)
    ]
      ++ minPairs
      ++ maxPairs
  where
    effectiveAllowNan = b.bAllowNan && not (isJust b.bMin) && not (isJust b.bMax)
    effectiveAllowInf = b.bAllowInf && not (isJust b.bMin && isJust b.bMax)

    -- Pin to the type's range when generating only finite values. Without
    -- explicit bounds a Float64 value outside Float32's range would silently
    -- become ±∞ after the double2Float conversion in the Float instance.
    needsBounds = not effectiveAllowNan && not effectiveAllowInf

    minPairs = case b.bMin of
      Just lo -> [("min_value", toVal lo)]
      Nothing | needsBounds -> [("min_value", toVal (negate (maxFiniteVal @a)))]
      Nothing -> []

    maxPairs = case b.bMax of
      Just hi -> [("max_value", toVal hi)]
      Nothing | needsBounds -> [("max_value", toVal (maxFiniteVal @a))]
      Nothing -> []
