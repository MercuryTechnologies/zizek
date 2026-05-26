module Hegel.Gen.Float
  ( FloatOptions (..),
    defaultFloatOptions,
    float,
    double,
    floatWith,
    doubleWith,
  )
where

import CBOR.Value (Value (..))
import Data.Maybe (isJust)
import GHC.Float (double2Float, float2Double)
import Hegel.Gen.Internal (Generator, pattern Schema)
import Hegel.Protocol.Cbor
  ( ParseError (..),
    boolVal,
    buildMap,
    doubleVal,
    floatVal,
    intVal,
    textVal,
  )
import Hegel.Range (Range (..))

data FloatOptions a = FloatOptions
  { minValue :: Maybe a,
    maxValue :: Maybe a,
    excludeMin :: Bool,
    excludeMax :: Bool,
    -- | Allow NaN values. Silently ignored when either bound is set, since a
    -- bounded range cannot contain NaN.
    allowNan :: Bool,
    -- | Allow infinite values. Silently ignored when both bounds are set.
    allowInfinity :: Bool
  }

defaultFloatOptions :: FloatOptions a
defaultFloatOptions =
  FloatOptions
    { minValue = Nothing,
      maxValue = Nothing,
      excludeMin = False,
      excludeMax = False,
      allowNan = True,
      allowInfinity = True
    }

-- | Generate a 'Float' in the given range.
float :: Range Float -> Generator Float
float (Range lo hi) = floatWith defaultFloatOptions {minValue = Just lo, maxValue = Just hi}

-- | Generate a 'Double' in the given range.
double :: Range Double -> Generator Double
double (Range lo hi) = doubleWith defaultFloatOptions {minValue = Just lo, maxValue = Just hi}

-- Use concrete GHC.Float primitives (float2Double / double2Float) rather than
-- realToFrac here. realToFrac routes through Rational (toRational then
-- fromRational), which calls decodeFloat on the bit pattern and treats NaN/Inf
-- as finite values — NaN becomes 1.5×2^128 and ±Inf becomes ±2^128. cbor2
-- always encodes Python float('nan') and float('inf') as CBOR Float16, so
-- even a Double generator receives a Float16 (NaN :: Float) off the wire.
floatWith :: FloatOptions Float -> Generator Float
floatWith opts = Schema (buildSchema floatVal 32 opts) parse
  where
    parse (Float16 f) = Right f
    parse (Float32 f) = Right f
    parse (Float64 d) = Right (double2Float d)
    parse v = Left ParseError {expected = "float", got = v}

doubleWith :: FloatOptions Double -> Generator Double
doubleWith opts = Schema (buildSchema doubleVal 64 opts) parse
  where
    parse (Float16 f) = Right (float2Double f)
    parse (Float32 f) = Right (float2Double f)
    parse (Float64 d) = Right d
    parse v = Left ParseError {expected = "float", got = v}

-- | Maximum finite value of a 'RealFloat' type.
maxFiniteVal :: forall a. (RealFloat a) => a
maxFiniteVal =
  let b = floatRadix (0 :: a)
      p = floatDigits (0 :: a)
      (_, eMax) = floatRange (0 :: a)
   in encodeFloat (b ^ p - 1) (eMax - p)

buildSchema :: forall a. (RealFloat a) => (a -> Value) -> Int -> FloatOptions a -> Value
buildSchema toVal width opts =
  buildMap $
    [ ("type", textVal "float"),
      ("width", intVal width),
      ("exclude_min", boolVal opts.excludeMin),
      ("exclude_max", boolVal opts.excludeMax),
      ("allow_nan", boolVal effectiveAllowNan),
      ("allow_infinity", boolVal effectiveAllowInfinity)
    ]
      ++ minPairs
      ++ maxPairs
  where
    effectiveAllowNan = opts.allowNan && not (isJust opts.minValue) && not (isJust opts.maxValue)
    effectiveAllowInfinity = opts.allowInfinity && not (isJust opts.minValue && isJust opts.maxValue)

    -- Pin to the type's range when generating only finite values. Without
    -- explicit bounds a Float64 value outside Float32's range would silently
    -- become ±∞ after the double2Float conversion in floatWith's parser.
    needsBounds = not effectiveAllowNan && not effectiveAllowInfinity

    minPairs = case opts.minValue of
      Just lo -> [("min_value", toVal lo)]
      Nothing | needsBounds -> [("min_value", toVal (negate (maxFiniteVal @a)))]
      Nothing -> []

    maxPairs = case opts.maxValue of
      Just hi -> [("max_value", toVal hi)]
      Nothing | needsBounds -> [("max_value", toVal (maxFiniteVal @a))]
      Nothing -> []
