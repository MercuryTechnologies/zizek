-- | Floating-point generators.
--
-- By default 'float' and 'double' produce NaN and ±Infinity.
--
-- Setting a bound via 'Hegel.Gen.Builder.min' or 'Hegel.Gen.Builder.max'
-- implicitly excludes NaN; setting both implicitly excludes Infinity.
--
--  Use 'disallowNan' and 'disallowInfinity' to exclude them unconditionally.
module Hegel.Gen.Float
  ( -- * Builders
    FloatBuilder,
    float,
    double,

    -- * Modifiers
    exclusiveMin,
    exclusiveMax,
    disallowNan,
    disallowInfinity,
  )
where

import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Float (double2Float, float2Double)
import Hegel.Gen.Builder (Build (..), GenValidationError (..), HasMax (..), HasMin (..), checkOrdered)
import Hegel.Gen.Internal (Gen (..))
import Hegel.Internal.DataSource (FloatSpec (..), drawFloat)

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

-- | Generate a random 32-bit floating-point number.
float :: FloatBuilder Float
float = defaultFloatBuilder

-- | Generate a random 64-bit floating-point number.
double :: FloatBuilder Double
double = defaultFloatBuilder

-- | Treat the lower bound as exclusive.
exclusiveMin :: FloatBuilder a -> FloatBuilder a
exclusiveMin b = b {bExclMin = True}

-- | Treat the upper bound as exclusive.
exclusiveMax :: FloatBuilder a -> FloatBuilder a
exclusiveMax b = b {bExclMax = True}

-- | Exclude NaN. (Already implicit once any bound is set.)
disallowNan :: FloatBuilder a -> FloatBuilder a
disallowNan b = b {bAllowNan = False}

-- | Exclude ±Infinity. (Already implicit once both bounds are set.)
disallowInfinity :: FloatBuilder a -> FloatBuilder a
disallowInfinity b = b {bAllowInf = False}

instance HasMin (FloatBuilder a) a where
  min lo b = b {bMin = Just lo}

instance HasMax (FloatBuilder a) a where
  max hi b = b {bMax = Just hi}

-- | Effective allow-NaN\/allow-Infinity flags. Kept verbatim from the
-- CBOR-era implementation: setting any bound implies NaN is excluded;
-- setting both implies Infinity is excluded. This is what already prevents
-- the engine-rejected NaN+bound \/ infinity+both-finite combinations.
effectiveFlags :: FloatBuilder a -> (Bool, Bool)
effectiveFlags b = (allowNan, allowInf)
  where
    allowNan = b.bAllowNan && not (isJust b.bMin) && not (isJust b.bMax)
    allowInf = b.bAllowInf && not (isJust b.bMin && isJust b.bMax)

-- | Smallest positive subnormal 'Double' (@5e-324@) — libhegel's documented
-- \"no restriction\" sentinel for @smallest_nonzero_magnitude@ at width 64.
smallestSubnormalDouble :: Double
smallestSubnormalDouble = 5e-324

-- | Smallest positive subnormal 'Float', as a 'Double' — the sentinel for
-- width 32.
smallestSubnormalFloat :: Double
smallestSubnormalFloat = float2Double (encodeFloat 1 (fst (floatRange (0 :: Float)) - floatDigits (0 :: Float)))

-- | Validate a 'FloatBuilder's bounds: neither may be NaN, and the lower must
-- not exceed the upper.
--
-- With an exclusive bound the two must be strictly ordered, since equal
-- endpoints would leave the range empty.
checkFloatBounds :: (RealFloat a, Show a) => Text -> FloatBuilder a -> IO ()
checkFloatBounds what b = do
  checkNotNaN b.bMin
  checkNotNaN b.bMax
  case (b.bMin, b.bMax) of
    (Just lo, Just hi)
      | b.bExclMin || b.bExclMax ->
          when (lo >= hi) $
            throwIO
              GenValidationError
                { context = what,
                  detail = "empty exclusive range: min (" <> T.pack (show lo) <> ") >= max (" <> T.pack (show hi) <> ")"
                }
      | otherwise -> checkOrdered what lo hi
    _ -> pure ()
  where
    checkNotNaN Nothing = pure ()
    checkNotNaN (Just x)
      | isNaN x = throwIO GenValidationError {context = what, detail = "bound is NaN"}
      | otherwise = pure ()

instance Build (FloatBuilder Float) Float where
  build b = Draw \tc -> do
    checkFloatBounds "Hegel.Gen.Float" b
    double2Float <$> drawFloat tc 32 spec
    where
      (allowNan, allowInf) = effectiveFlags b
      spec =
        FloatSpec
          { minValue = maybe (-1 / 0) float2Double b.bMin,
            maxValue = maybe (1 / 0) float2Double b.bMax,
            allowNan,
            allowInfinity = allowInf,
            excludeMin = b.bExclMin,
            excludeMax = b.bExclMax,
            smallestNonzeroMagnitude = smallestSubnormalFloat
          }

instance Build (FloatBuilder Double) Double where
  build b = Draw \tc -> do
    checkFloatBounds "Hegel.Gen.Float" b
    drawFloat tc 64 spec
    where
      (allowNan, allowInf) = effectiveFlags b
      spec =
        FloatSpec
          { minValue = fromMaybe (-1 / 0) b.bMin,
            maxValue = fromMaybe (1 / 0) b.bMax,
            allowNan,
            allowInfinity = allowInf,
            excludeMin = b.bExclMin,
            excludeMax = b.bExclMax,
            smallestNonzeroMagnitude = smallestSubnormalDouble
          }
