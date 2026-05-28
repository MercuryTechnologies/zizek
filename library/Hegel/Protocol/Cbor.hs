-- | Constructors and accessors for working with CBOR 'Value's at the wire
-- layer.
module Hegel.Protocol.Cbor
  ( -- * Errors
    ParseError (..),

    -- * Maps
    lookupKey,
    buildMap,
    (.=),
    (.=?),

    -- * Constructors
    textVal,
    intVal,
    floatVal,
    doubleVal,
    boolVal,
    nullVal,

    -- * Accessors
    hegelText,
    asText,
    asInt,
    asBool,
    asWord32,
    asWord64,
  )
where

import CBOR.Class (ToCBOR (..))
import CBOR.Value (Value (..))
import Control.Exception (Exception)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Data.Vector qualified as V
import Data.Word (Word32, Word64)

-- | A CBOR 'Value' didn't match an expected shape.
data ParseError = ParseError
  { -- | Human-readable description of the expected shape.
    expected :: !Text,
    -- | The value that was actually received.
    got :: !Value
  }
  deriving stock (Eq, Show)

instance Exception ParseError

-- | Look up a 'TextString' key in a CBOR 'Map'.
lookupKey :: Text -> Value -> Maybe Value
lookupKey key (Map entries) =
  V.foldr step Nothing entries
  where
    step (TextString k, v) acc = if k == key then Just v else acc
    step _ acc = acc
lookupKey _ _ = Nothing

-- | Build a CBOR 'Map' from key/value pairs.
buildMap :: [(Text, Value)] -> Value
buildMap pairs = Map (V.fromList [(TextString k, v) | (k, v) <- pairs])

-- | Pair a wire key with a value via 'toCBOR'. Mirrors aeson's
-- @('Data.Aeson..=')@. Use with 'buildMap':
--
-- > buildMap
-- >   [ "type"      .= ("integer" :: Text)
-- >   , "min_value" .= (0 :: Int)
-- >   , "max_value" .= (100 :: Int)
-- >   ]
(.=) :: (ToCBOR a) => Text -> a -> (Text, Value)
k .= v = (k, toCBOR v)

infixr 8 .=

-- | Like @('.=')@, but yields 'Nothing' when the value is absent so the
-- key can be omitted from the map entirely. Group optional fields with
-- 'Data.Maybe.catMaybes' and splice into 'buildMap' with @(<>)@:
--
-- > buildMap $
-- >   [ "min_size" .= s.minSize ]
-- >   <> catMaybes ["max_size" .=? s.maxSize]
(.=?) :: (ToCBOR a) => Text -> Maybe a -> Maybe (Text, Value)
k .=? mv = fmap (k .=) mv

infixr 8 .=?

textVal :: Text -> Value
textVal = TextString

intVal :: (Integral a) => a -> Value
intVal n
  | n >= 0 = UInt (fromIntegral n)
  | otherwise = NInt (fromIntegral (negate n - 1))

floatVal :: Float -> Value
floatVal = Float32

doubleVal :: Double -> Value
doubleVal = Float64

boolVal :: Bool -> Value
boolVal = Bool

nullVal :: Value
nullVal = Null

-- | Decode a generated string value from the @hegel@ server.
--
-- The server encodes all string values as @CBOR tag 91@ wrapping a WTF-8
-- byte string (identical to UTF-8 for non-surrogate code points). Returns
-- 'Left' when the value is not a tag-91 byte string or the bytes are not
-- valid UTF-8.
hegelText :: Value -> Either ParseError Text
hegelText (Tag 91 (ByteString bs)) =
  case decodeUtf8' bs of
    Right t -> Right t
    Left _ -> Left ParseError {expected = "valid UTF-8", got = ByteString bs}
hegelText v = Left ParseError {expected = "string (tag 91)", got = v}

asText :: Value -> Maybe Text
asText (TextString t) = Just t
asText _ = Nothing

asInt :: Value -> Maybe Int
asInt (UInt n) = Just (fromIntegral n)
asInt (NInt n) = Just (negate (fromIntegral n) - 1)
asInt _ = Nothing

asBool :: Value -> Maybe Bool
asBool (Bool b) = Just b
asBool _ = Nothing

asWord32 :: Value -> Maybe Word32
asWord32 (UInt n) = Just (fromIntegral n)
asWord32 _ = Nothing

asWord64 :: Value -> Maybe Word64
asWord64 (UInt n) = Just n
asWord64 _ = Nothing
