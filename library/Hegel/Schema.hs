-- | Wire-schema vocabulary for the @hegel-core@ protocol.
--
-- All schema records live here so that @Hegel.Gen.*@ modules consume
-- typed smart constructors instead of building CBOR maps by hand.
-- 'CBOR.Class.toCBOR' converts any record to the wire 'CBOR.Value'.
module Hegel.Schema
  ( -- * Leaf schemas (produced by generator modules)
    BoolSchema,
    bool,
    IntegerSchema (..),
    integer,
    BinarySchema (..),
    binary,
    FloatSchema (..),

    -- * Composite \/ wire-only schemas (used by @Hegel.Gen.Internal@)
    UnitSchema,
    unit,
    TupleSchema (..),
    tuple,
    OneOfSchema (..),
    oneOf,
  )
where

import CBOR.Class (ToCBOR (..))
import CBOR.Value (Value (..))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Hegel.Protocol.Cbor (buildMap, (.=), (.=?))

-- | Boolean schema. Server emits 'True' or 'False' uniformly.
data BoolSchema = BoolSchema

instance ToCBOR BoolSchema where
  toCBOR _ = buildMap ["type" .= ("boolean" :: Text)]

-- | Generate a random boolean.
bool :: BoolSchema
bool = BoolSchema

-- | Integer schema with inclusive lower and upper bounds.
data IntegerSchema a = IntegerSchema
  { -- | Inclusive lower bound.
    minValue :: !a,
    -- | Inclusive upper bound.
    maxValue :: !a
  }

instance (ToCBOR a) => ToCBOR (IntegerSchema a) where
  toCBOR s =
    buildMap
      [ "type" .= ("integer" :: Text),
        "min_value" .= s.minValue,
        "max_value" .= s.maxValue
      ]

-- | Generate an integer in the inclusive range @[lo, hi]@.
integer :: a -> a -> IntegerSchema a
integer = IntegerSchema

-- | Binary schema with minimum size and optional maximum size.
data BinarySchema = BinarySchema
  { -- | Minimum number of bytes (inclusive).
    minSize :: !Int,
    -- | Maximum number of bytes (inclusive), or unbounded.
    maxSize :: !(Maybe Int)
  }

instance ToCBOR BinarySchema where
  toCBOR s =
    buildMap $
      [ "type" .= ("binary" :: Text),
        "min_size" .= s.minSize
      ]
        <> catMaybes ["max_size" .=? s.maxSize]

-- | Generate a 'Data.ByteString.ByteString' whose length lies in
-- @[minSize, maxSize]@.
binary :: Int -> Maybe Int -> BinarySchema
binary = BinarySchema

-- | Float \/ double schema. The 'width' field selects single- or
-- double-precision; the bounds and toggles correspond directly to the
-- @hegel-core@ float vocabulary.
data FloatSchema a = FloatSchema
  { -- | Floating-point width in bits (32 or 64).
    width :: !Int,
    -- | If 'True', the lower bound is exclusive.
    excludeMin :: !Bool,
    -- | If 'True', the upper bound is exclusive.
    excludeMax :: !Bool,
    -- | Whether NaN is a permitted draw.
    allowNan :: !Bool,
    -- | Whether ±∞ are permitted draws.
    allowInfinity :: !Bool,
    -- | Inclusive lower bound, if any.
    minValue :: !(Maybe a),
    -- | Inclusive upper bound, if any.
    maxValue :: !(Maybe a)
  }

instance (ToCBOR a) => ToCBOR (FloatSchema a) where
  toCBOR s =
    buildMap $
      [ "type" .= ("float" :: Text),
        "width" .= s.width,
        "exclude_min" .= s.excludeMin,
        "exclude_max" .= s.excludeMax,
        "allow_nan" .= s.allowNan,
        "allow_infinity" .= s.allowInfinity
      ]
        <> catMaybes
          [ "min_value" .=? s.minValue,
            "max_value" .=? s.maxValue
          ]

-- | Constant schema: always returns @null@. Used as a sentinel in
-- @Hegel.Gen.Internal@ for 'Pure' branches of 'oneOf'.
data UnitSchema = UnitSchema

instance ToCBOR UnitSchema where
  toCBOR _ =
    buildMap
      [ "type" .= ("constant" :: Text),
        "value" .= Null
      ]

-- | The unit-schema sentinel.
unit :: UnitSchema
unit = UnitSchema

-- | Tuple schema: a fixed-arity sequence of independent draws.
data TupleSchema = TupleSchema
  { -- | Element schemas in positional order.
    elements :: ![Value]
  }

instance ToCBOR TupleSchema where
  toCBOR s =
    buildMap
      [ "type" .= ("tuple" :: Text),
        "elements" .= s.elements
      ]

-- | Combine a list of schemas into a single tuple draw.
tuple :: [Value] -> TupleSchema
tuple = TupleSchema

-- | OneOf schema: a tagged-union draw.
data OneOfSchema = OneOfSchema
  { -- | Branch schemas; the server picks one and returns @[index, value]@.
    generators :: ![Value]
  }

instance ToCBOR OneOfSchema where
  toCBOR s =
    buildMap
      [ "type" .= ("one_of" :: Text),
        "generators" .= s.generators
      ]

-- | Pick one of the branch schemas uniformly at random.
oneOf :: [Value] -> OneOfSchema
oneOf = OneOfSchema
