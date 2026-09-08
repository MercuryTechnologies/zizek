module Hegel.Replay
  ( ReplayToken,
    ReplayError (..),
    encodeReplayToken,
    decodeReplayToken,
    replayTokenOrigin,
    replayTokenVersion,
  )
where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.Char (digitToInt, intToDigit, isHexDigit)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Hegel.Internal.Replay (ReplayToken, makeReplayToken, tokenBlobOf, tokenOriginOf, tokenVersionOf)

-- | Why a serialized replay token could not be decoded.
data ReplayError
  = ReplayTokenMalformedEnvelope
  | ReplayTokenUnsupportedFormat !Text
  | ReplayTokenEmptyField
  | ReplayTokenInvalidOriginHex
  | ReplayTokenOriginNotUtf8
  deriving stock (Eq, Show)

-- | The engine version that produced a replay token.
replayTokenVersion :: ReplayToken -> Text
replayTokenVersion = tokenVersionOf

-- | The failure origin expected when a replay token is used.
replayTokenOrigin :: ReplayToken -> Text
replayTokenOrigin = tokenOriginOf

-- | Encode a token as copyable text containing engine metadata.
encodeReplayToken :: ReplayToken -> Text
encodeReplayToken token =
  T.intercalate ":" ["hegel-replay", tokenVersionOf token, encodeHex (TE.encodeUtf8 (tokenOriginOf token)), TE.decodeUtf8 (tokenBlobOf token)]

encodeHex :: ByteString -> Text
encodeHex = T.pack . concatMap byteHex . BSC.unpack
  where
    byteHex :: Char -> String
    byteHex c = [intToDigit (fromEnum c `shiftR` 4), intToDigit (fromEnum c .&. 15)]

-- | Decode a token produced by 'encodeReplayToken'.
decodeReplayToken :: Text -> Either ReplayError ReplayToken
decodeReplayToken input = case T.splitOn ":" input of
  ["hegel-replay", version, origin, blob]
    | T.null version || T.null origin || T.null blob -> Left ReplayTokenEmptyField
    | otherwise -> do
        decodedOrigin <- decodeHex origin
        case TE.decodeUtf8' decodedOrigin of
          Left _ -> Left ReplayTokenOriginNotUtf8
          Right decodedOriginText -> Right (makeReplayToken version decodedOriginText (TE.encodeUtf8 blob))
  format : _ | format /= "hegel-replay" -> Left (ReplayTokenUnsupportedFormat format)
  _ -> Left ReplayTokenMalformedEnvelope
  where
    decodeHex text
      | T.length text `mod` 2 /= 0 = Left ReplayTokenInvalidOriginHex
      | T.any (not . isHexDigit) text = Left ReplayTokenInvalidOriginHex
      | otherwise = Right (BSC.pack (pairs (T.unpack text)))

    pairs :: [Char] -> [Char]
    pairs [] = []
    pairs (a : b : rest) = toEnum (digitToInt a * 16 + digitToInt b) : pairs rest
    pairs _ = []
