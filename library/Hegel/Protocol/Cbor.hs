module Hegel.Protocol.Cbor
  ( ParseError (..)
  , lookupKey
  , buildMap
  , textVal
  , intVal
  , boolVal
  , nullVal
  , asText
  , asInt
  , asBool
  , asWord32
  , asWord64
  ) where

import CBOR.Value (Value (..))
import Control.Exception (Exception)
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Word (Word32, Word64)

data ParseError = ParseError
  { expected :: !Text
  , got      :: !Value
  }
  deriving stock (Show)

instance Exception ParseError

lookupKey :: Text -> Value -> Maybe Value
lookupKey key (Map entries) =
  V.foldr step Nothing entries
  where
    step (TextString k, v) acc = if k == key then Just v else acc
    step _ acc = acc
lookupKey _ _ = Nothing

buildMap :: [(Text, Value)] -> Value
buildMap pairs = Map (V.fromList [(TextString k, v) | (k, v) <- pairs])

textVal :: Text -> Value
textVal = TextString

intVal :: (Integral a) => a -> Value
intVal n
  | n >= 0    = UInt (fromIntegral n)
  | otherwise = NInt (fromIntegral (negate n - 1))

boolVal :: Bool -> Value
boolVal = Bool

nullVal :: Value
nullVal = Null

asText :: Value -> Maybe Text
asText (TextString t) = Just t
asText _              = Nothing

asInt :: Value -> Maybe Int
asInt (UInt n)  = Just (fromIntegral n)
asInt (NInt n)  = Just (negate (fromIntegral n) - 1)
asInt _         = Nothing

asBool :: Value -> Maybe Bool
asBool (Bool b) = Just b
asBool _        = Nothing

asWord32 :: Value -> Maybe Word32
asWord32 (UInt n) = Just (fromIntegral n)
asWord32 _        = Nothing

asWord64 :: Value -> Maybe Word64
asWord64 (UInt n) = Just n
asWord64 _        = Nothing
