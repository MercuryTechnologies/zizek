module Hegel.Gen.Integer
  ( IntegerBuilder,
    integer,
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
    enum,
    enumBounded,
  )
where

import CBOR.Value (Value (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Maybe (fromMaybe)
import Data.Word (Word16, Word32, Word64, Word8)
import Hegel.Gen.Builder (Build (..), HasMax (..), HasMin (..))
import Hegel.Gen.Internal (Generator, pattern Schema)
import Hegel.Protocol.Cbor (ParseError (..), buildMap, intVal, textVal)

data IntegerBuilder a = IntegerBuilder
  { bMin :: Maybe a,
    bMax :: Maybe a
  }

integer :: IntegerBuilder a
integer = IntegerBuilder {bMin = Nothing, bMax = Nothing}

int :: IntegerBuilder Int
int = integer

int8 :: IntegerBuilder Int8
int8 = integer

int16 :: IntegerBuilder Int16
int16 = integer

int32 :: IntegerBuilder Int32
int32 = integer

int64 :: IntegerBuilder Int64
int64 = integer

word :: IntegerBuilder Word
word = integer

word8 :: IntegerBuilder Word8
word8 = integer

word16 :: IntegerBuilder Word16
word16 = integer

word32 :: IntegerBuilder Word32
word32 = integer

word64 :: IntegerBuilder Word64
word64 = integer

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

-- | Generate a value of a bounded enumeration, drawing from the full range
-- of the type.
enumBounded :: forall a. (Bounded a, Enum a) => Generator a
enumBounded =
  toEnum <$> build IntegerBuilder
    { bMin = Just (fromEnum (minBound :: a))
    , bMax = Just (fromEnum (maxBound :: a))
    }

-- | Generate a value of an enumeration within the given inclusive range.
enum :: (Enum a) => a -> a -> Generator a
enum lo hi =
  toEnum <$> build IntegerBuilder
    { bMin = Just (fromEnum lo)
    , bMax = Just (fromEnum hi)
    }
