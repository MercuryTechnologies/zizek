-- | Wire-schema vocabulary for the Hegel protocol.
--
-- __Internal module.__ Implementation substrate of @zizek@ itself, exposed so
-- you can reach past the public API when you must; it is not part of the
-- stable public interface and may change without notice.
--
-- All schema records live here so that @Hegel.Gen.*@ modules consume
-- typed smart constructors instead of building CBOR maps by hand.
-- 'CBOR.Class.toCBOR' converts any record to the wire 'CBOR.Value'.
--
-- Records are accessed via 'OverloadedRecordDot'; field selectors are
-- suppressed ('NoFieldSelectors') so that field names don't escape into
-- call sites and conflict with identically-named builder modifier functions.
module Hegel.Internal.Schema
  ( -- * Leaf schemas (produced by generator modules)
    BoolSchema,
    bool,
    IntegerSchema (..),
    integer,
    BinarySchema (..),
    binary,
    FloatSchema (..),
    CharacterFields (..),
    defaultCharacterFields,
    TextSchema (..),
    text,
    UuidSchema (..),
    uuid,
    UrlSchema,
    url,
    DomainSchema (..),
    domain,
    RegexSchema (..),
    regex,

    -- * Collection schemas
    ListSchema (..),
    list,
    MapSchema (..),
    map,

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
import Data.Default.Class (Default (..))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Hegel.Internal.CBOR (buildMap, (.=), (.=?))
import Prelude hiding (map)

-- | Boolean schema.
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
-- @hegel@ float vocabulary.
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
    -- | Lower bound, if any; exclusivity governed by 'excludeMin'.
    minValue :: !(Maybe a),
    -- | Upper bound, if any; exclusivity governed by 'excludeMax'.
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

-- | Character filtering options, shared by 'TextSchema' and 'RegexSchema'.
data CharacterFields = CharacterFields
  { -- | Restrict to characters encodable in this codec (e.g. @"ascii"@,
    -- @"latin-1"@).
    codec :: !(Maybe Text),
    -- | Minimum Unicode codepoint (inclusive).
    minCodepoint :: !(Maybe Int),
    -- | Maximum Unicode codepoint (inclusive).
    maxCodepoint :: !(Maybe Int),
    -- | Include only characters from these Unicode general categories
    -- (e.g. @["L", "Nd"]@). Mutually exclusive with 'excludeCategories'.
    categories :: !(Maybe [Text]),
    -- | Exclude characters from these Unicode general categories.
    -- Mutually exclusive with 'categories'.
    excludeCategories :: !(Maybe [Text]),
    -- | Always include these characters, even if excluded by other filters.
    includeCharacters :: !(Maybe Text),
    -- | Always exclude these characters.
    excludeCharacters :: !(Maybe Text)
  }

instance ToCBOR CharacterFields where
  toCBOR = buildMap . charFieldEntries

-- | All fields absent: no character restrictions.
defaultCharacterFields :: CharacterFields
defaultCharacterFields =
  CharacterFields
    { codec = Nothing,
      minCodepoint = Nothing,
      maxCodepoint = Nothing,
      categories = Nothing,
      excludeCategories = Nothing,
      includeCharacters = Nothing,
      excludeCharacters = Nothing
    }

-- | Alias for 'defaultCharacterFields'.
instance Default CharacterFields where
  def = defaultCharacterFields

-- Shared helper: emit the character-filtering entries for a CBOR map.
charFieldEntries :: CharacterFields -> [(Text, Value)]
charFieldEntries cf =
  catMaybes
    [ "codec" .=? cf.codec,
      "min_codepoint" .=? cf.minCodepoint,
      "max_codepoint" .=? cf.maxCodepoint,
      "categories" .=? cf.categories,
      "exclude_categories" .=? cf.excludeCategories,
      "include_characters" .=? cf.includeCharacters,
      "exclude_characters" .=? cf.excludeCharacters
    ]

-- | Text schema. Wire type is @\"string\"@ (per the @hegel@ spec).
--
-- Haskell @Text@ cannot represent lone surrogates; the 'text' smart
-- constructor therefore defaults 'excludeCategories' to @[\"Cs\"]@. Override
-- by constructing 'TextSchema' directly when you need full control (e.g. in
-- test harnesses).
data TextSchema = TextSchema
  { -- | Minimum codepoint length (inclusive).
    minSize :: !Int,
    -- | Maximum codepoint length (inclusive), or unbounded.
    maxSize :: !(Maybe Int),
    -- | Character filtering constraints.
    charFields :: !CharacterFields
  }

instance ToCBOR TextSchema where
  toCBOR s =
    buildMap $
      [ "type" .= ("string" :: Text),
        "min_size" .= s.minSize
      ]
        <> catMaybes ["max_size" .=? s.maxSize]
        <> charFieldEntries s.charFields

-- | Default text schema: unbounded length, surrogates excluded.
text :: Int -> TextSchema
text minSize =
  TextSchema
    { minSize,
      maxSize = Nothing,
      charFields = defaultCharacterFields {excludeCategories = Just ["Cs"]}
    }

-- | UUID schema. Wire type is @\"uuid\"@.
data UuidSchema = UuidSchema
  { -- | RFC 4122 UUID version (1–5), or 'Nothing' for any version.
    version :: !(Maybe Int)
  }

instance ToCBOR UuidSchema where
  toCBOR s =
    buildMap $
      ["type" .= ("uuid" :: Text)]
        <> catMaybes ["version" .=? s.version]

-- | Default UUID schema: any version.
uuid :: UuidSchema
uuid = UuidSchema {version = Nothing}

-- | URL schema. Wire type is @\"url\"@. Generates RFC 3986 HTTP\/HTTPS URLs.
data UrlSchema = UrlSchema

instance ToCBOR UrlSchema where
  toCBOR _ = buildMap ["type" .= ("url" :: Text)]

-- | URL schema sentinel.
url :: UrlSchema
url = UrlSchema

-- | Domain schema. Wire type is @\"domain\"@. Generates RFC 1035 FQDNs.
data DomainSchema = DomainSchema
  { -- | Maximum total length of the generated domain name, inclusive.
    maxLength :: !(Maybe Int)
  }

instance ToCBOR DomainSchema where
  toCBOR s =
    buildMap $
      ["type" .= ("domain" :: Text)]
        <> catMaybes ["max_length" .=? s.maxLength]

-- | Default domain schema: no length limit (engine default is 255).
domain :: DomainSchema
domain = DomainSchema {maxLength = Nothing}

-- | Regex schema.
data RegexSchema = RegexSchema
  { -- | The regular expression pattern.
    regexPattern :: !Text,
    -- | When 'True', the entire generated string must match the pattern.
    -- Default: 'False'.
    fullmatch :: !Bool,
    -- | Optional character set restriction for generated strings.
    alphabet :: !(Maybe CharacterFields)
  }

instance ToCBOR RegexSchema where
  toCBOR s =
    buildMap $
      [ "type" .= ("regex" :: Text),
        "pattern" .= s.regexPattern,
        "fullmatch" .= s.fullmatch
      ]
        <> catMaybes ["alphabet" .=? s.alphabet]

-- | Default regex schema: partial match, no alphabet restriction.
regex :: Text -> RegexSchema
regex p = RegexSchema {regexPattern = p, fullmatch = False, alphabet = Nothing}

-- | List schema: a variable-length sequence of homogeneous draws.
data ListSchema = ListSchema
  { -- | Schema for each element.
    element :: !Value,
    -- | Minimum number of elements (inclusive).
    minSize :: !Int,
    -- | Maximum number of elements (inclusive), or unbounded.
    maxSize :: !(Maybe Int),
    -- | When 'True', the engine rejects duplicate elements.
    unique :: !Bool
  }

instance ToCBOR ListSchema where
  toCBOR s =
    buildMap $
      [ "type" .= ("list" :: Text),
        "elements" .= s.element,
        "min_size" .= s.minSize,
        "unique" .= s.unique
      ]
        <> catMaybes ["max_size" .=? s.maxSize]

-- | Build a list schema from element schema, size bounds, and uniqueness flag.
list :: Value -> Int -> Maybe Int -> Bool -> ListSchema
list = ListSchema

-- | Map (dictionary) schema: variable-length key-value entries.
data MapSchema = MapSchema
  { -- | Schema for keys.
    keys :: !Value,
    -- | Schema for values.
    values :: !Value,
    -- | Minimum number of entries (inclusive).
    minSize :: !Int,
    -- | Maximum number of entries (inclusive), or unbounded.
    maxSize :: !(Maybe Int)
  }

instance ToCBOR MapSchema where
  toCBOR s =
    buildMap $
      [ "type" .= ("dict" :: Text),
        "keys" .= s.keys,
        "values" .= s.values,
        "min_size" .= s.minSize
      ]
        <> catMaybes ["max_size" .=? s.maxSize]

-- | Build a map schema from key schema, value schema, and size bounds.
map :: Value -> Value -> Int -> Maybe Int -> MapSchema
map = MapSchema

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
  { -- | Branch schemas; the engine picks one and returns @[index, value]@.
    generators :: ![Value]
  }

instance ToCBOR OneOfSchema where
  toCBOR s =
    buildMap
      [ "type" .= ("one_of" :: Text),
        "generators" .= s.generators
      ]

-- | Have the engine pick one of the branch schemas.
oneOf :: [Value] -> OneOfSchema
oneOf = OneOfSchema
