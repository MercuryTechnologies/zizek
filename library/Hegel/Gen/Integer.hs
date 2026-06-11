-- | Integral generators.
--
-- Unbounded by default; narrow with 'Hegel.Gen.Builder.min' and
-- 'Hegel.Gen.Builder.max':
--
-- > Gen.int & Gen.min 0 & Gen.max 100 & Gen.build
module Hegel.Gen.Integer
  ( -- * Builder
    IntegralBuilder,
    integral,

    -- * Type-pinned aliases
    -- $aliases
    int,
    int8,
    int16,
    int32,
    int64,
    word,
    word8,
    word16,
    word32,
    word64,

    -- * Enumerations
    enum,
    enumBounded,
  )
where

import CBOR.Class (ToCBOR (..))
import CBOR.Value (Value (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Maybe (fromMaybe)
import Data.Word (Word16, Word32, Word64, Word8)
import Hegel.Cbor (ParseError (..))
import Hegel.Gen.Builder (Build (..), HasMax (..), HasMin (..))
import Hegel.Gen.Internal (Gen, basic)
import Hegel.Schema qualified as Schema

data IntegralBuilder a = IntegralBuilder
  { bMin :: Maybe a,
    bMax :: Maybe a
  }

-- | Generate a random integral number in the given inclusive range.
--
-- Use a type application or a type-pinned alias to fix the element type:
--
-- > Gen.integral @Int & Gen.build
-- > Gen.int           & Gen.build
integral :: IntegralBuilder a
integral = IntegralBuilder {bMin = Nothing, bMax = Nothing}

-- $aliases
-- Type-pinned specializations of 'integral'.

-- | Generate a random machine integer.
--
-- /This is a specialization of 'integral', offered for convenience./
int :: IntegralBuilder Int
int = integral

-- | Generate a random 8-bit integer.
--
-- /This is a specialization of 'integral', offered for convenience./
int8 :: IntegralBuilder Int8
int8 = integral

-- | Generate a random 16-bit integer.
--
-- /This is a specialization of 'integral', offered for convenience./
int16 :: IntegralBuilder Int16
int16 = integral

-- | Generate a random 32-bit integer.
--
-- /This is a specialization of 'integral', offered for convenience./
int32 :: IntegralBuilder Int32
int32 = integral

-- | Generate a random 64-bit integer.
--
-- /This is a specialization of 'integral', offered for convenience./
int64 :: IntegralBuilder Int64
int64 = integral

-- | Generate a random machine word.
--
-- /This is a specialization of 'integral', offered for convenience./
word :: IntegralBuilder Word
word = integral

-- | Generate a random 8-bit word.
--
-- /This is a specialization of 'integral', offered for convenience./
word8 :: IntegralBuilder Word8
word8 = integral

-- | Generate a random 16-bit word.
--
-- /This is a specialization of 'integral', offered for convenience./
word16 :: IntegralBuilder Word16
word16 = integral

-- | Generate a random 32-bit word.
--
-- /This is a specialization of 'integral', offered for convenience./
word32 :: IntegralBuilder Word32
word32 = integral

-- | Generate a random 64-bit word.
--
-- /This is a specialization of 'integral', offered for convenience./
word64 :: IntegralBuilder Word64
word64 = integral

instance HasMin (IntegralBuilder a) a where
  min lo b = b {bMin = Just lo}

instance HasMax (IntegralBuilder a) a where
  max hi b = b {bMax = Just hi}

instance (Bounded a, Integral a, ToCBOR a) => Build (IntegralBuilder a) a where
  build b =
    let lo = fromMaybe minBound b.bMin
        hi = fromMaybe maxBound b.bMax
     in basic (Schema.integer lo hi) parseInteger

parseInteger :: (Integral a) => Value -> Either ParseError a
parseInteger (UInt n) = Right (fromIntegral n)
parseInteger (NInt n) = Right (fromIntegral (negate (fromIntegral n :: Integer) - 1))
parseInteger v = Left ParseError {expected = "integer", got = v}

-- | Generate an enumeration, drawing from 'minBound' to 'maxBound'.
enumBounded :: forall a. (Bounded a, Enum a) => Gen a
enumBounded =
  toEnum
    <$> build
      IntegralBuilder
        { bMin = Just (fromEnum (minBound :: a)),
          bMax = Just (fromEnum (maxBound :: a))
        }

-- | Generate a value of an enumeration within the given inclusive range.
enum :: (Enum a) => a -> a -> Gen a
enum lo hi =
  toEnum
    <$> build
      IntegralBuilder
        { bMin = Just (fromEnum lo),
          bMax = Just (fromEnum hi)
        }
