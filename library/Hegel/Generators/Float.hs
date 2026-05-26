module Hegel.Generators.Float
  ( FloatGenerator,
    genFloat,
    genDouble,
    floats,
    withMinValue,
    withMaxValue,
    withExcludeMin,
    withExcludeMax,
    withAllowNan,
    withAllowInfinity,
  )
where

import CBOR.Value (Value (..))
import Data.Maybe (isJust, isNothing)
import GHC.Float (double2Float, float2Double)
import Hegel.Generators (Generator, pattern Schema)
import Hegel.Protocol.Cbor
  ( ParseError (..),
    boolVal,
    buildMap,
    doubleVal,
    floatVal,
    intVal,
    textVal,
  )

data FloatGenerator a = FloatGenerator
  { minVal :: Maybe a,
    maxVal :: Maybe a,
    excludeMin :: Bool,
    excludeMax :: Bool,
    allowNan :: Bool,
    allowInfinity :: Bool
  }

floats :: FloatGenerator a
floats =
  FloatGenerator
    { minVal = Nothing,
      maxVal = Nothing,
      excludeMin = False,
      excludeMax = False,
      allowNan = True,
      allowInfinity = True
    }

withMinValue :: (Ord a) => a -> FloatGenerator a -> FloatGenerator a
withMinValue lo cfg =
  cfg
    { minVal = Just lo,
      maxVal = fmap (max lo) cfg.maxVal,
      allowNan = False,
      allowInfinity = if isJust cfg.maxVal then False else cfg.allowInfinity
    }

withMaxValue :: (Ord a) => a -> FloatGenerator a -> FloatGenerator a
withMaxValue hi cfg =
  cfg
    { maxVal = Just hi,
      minVal = fmap (min hi) cfg.minVal,
      allowNan = False,
      allowInfinity = if isJust cfg.minVal then False else cfg.allowInfinity
    }

withExcludeMin :: Bool -> FloatGenerator a -> FloatGenerator a
withExcludeMin b cfg = cfg {excludeMin = b}

withExcludeMax :: Bool -> FloatGenerator a -> FloatGenerator a
withExcludeMax b cfg = cfg {excludeMax = b}

withAllowNan :: Bool -> FloatGenerator a -> FloatGenerator a
withAllowNan nan cfg =
  cfg {allowNan = nan && isNothing cfg.minVal && isNothing cfg.maxVal}

withAllowInfinity :: Bool -> FloatGenerator a -> FloatGenerator a
withAllowInfinity inf cfg =
  cfg {allowInfinity = inf && not (isJust cfg.minVal && isJust cfg.maxVal)}

-- Use concrete GHC.Float primitives (float2Double / double2Float) rather than
-- realToFrac here. realToFrac routes through Rational (toRational then
-- fromRational), which calls decodeFloat on the bit pattern and treats NaN/Inf
-- as finite values — NaN becomes 1.5×2^128 and ±Inf becomes ±2^128. cbor2
-- always encodes Python float('nan') and float('inf') as CBOR Float16, so
-- even a Double generator receives a Float16 (NaN :: Float) off the wire.
genFloat :: FloatGenerator Float -> Generator Float
genFloat cfg = Schema (buildSchema floatVal 32 cfg) parse
  where
    parse (Float16 f) = Right f
    parse (Float32 f) = Right f
    parse (Float64 d) = Right (double2Float d)
    parse v = Left ParseError {expected = "float", got = v}

genDouble :: FloatGenerator Double -> Generator Double
genDouble cfg = Schema (buildSchema doubleVal 64 cfg) parse
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

buildSchema :: forall a. (RealFloat a) => (a -> Value) -> Int -> FloatGenerator a -> Value
buildSchema toVal width cfg =
  buildMap $
    [ ("type", textVal "float"),
      ("width", intVal width),
      ("exclude_min", boolVal cfg.excludeMin),
      ("exclude_max", boolVal cfg.excludeMax),
      ("allow_nan", boolVal cfg.allowNan),
      ("allow_infinity", boolVal cfg.allowInfinity)
    ]
      ++ minPairs
      ++ maxPairs
  where
    -- Pin to the type's range when generating only finite values. Without explicit
    -- bounds a Float64 value outside Float32's range would silently become ±∞ after
    -- the double2Float conversion in genFloat's parser.
    needsBounds :: Bool
    needsBounds = not cfg.allowNan && not cfg.allowInfinity

    minPairs = case cfg.minVal of
      Just lo -> [("min_value", toVal lo)]
      Nothing | needsBounds -> [("min_value", toVal (negate (maxFiniteVal @a)))]
      Nothing -> []

    maxPairs = case cfg.maxVal of
      Just hi -> [("max_value", toVal hi)]
      Nothing | needsBounds -> [("max_value", toVal (maxFiniteVal @a))]
      Nothing -> []
