module Hegel.Replay
  ( ReplayToken,
    ReplayError (..),
    encodeReplayToken,
    decodeReplayToken,
    replayTokenOrigin,
    replayTokenVersion,
  )
where

import Control.Monad (when)
import Data.Bifunctor (first)
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
--
-- Every token 'encodeReplayToken' produces decodes back to an equal token,
-- including one whose version contains @:@ or whose origin is empty.
decodeReplayToken :: Text -> Either ReplayError ReplayToken
decodeReplayToken input = do
  (version, origin, blob) <- envelope (T.splitOn ":" input)
  when (T.null version || T.null blob) (Left ReplayTokenEmptyField)
  originBytes <- decodeHex origin
  originText <- first (const ReplayTokenOriginNotUtf8) (TE.decodeUtf8' originBytes)
  pure (makeReplayToken version originText (TE.encodeUtf8 blob))
  where
    -- The origin is hex and the blob is base64, so neither holds a colon, and
    -- whatever lies between the format and the origin is the version.
    envelope :: [Text] -> Either ReplayError (Text, Text, Text)
    envelope = \case
      "hegel-replay" : fields
        | blob : origin : version@(_ : _) <- reverse fields ->
            Right (T.intercalate ":" (reverse version), origin, blob)
      format : _ | format /= "hegel-replay" -> Left (ReplayTokenUnsupportedFormat format)
      _ -> Left ReplayTokenMalformedEnvelope

    decodeHex :: Text -> Either ReplayError ByteString
    decodeHex text
      | even (T.length text) && T.all isHexDigit text = Right (BSC.pack (hexPairs (T.unpack text)))
      | otherwise = Left ReplayTokenInvalidOriginHex

    hexPairs :: [Char] -> [Char]
    hexPairs (hi : lo : rest) = toEnum (digitToInt hi * 16 + digitToInt lo) : hexPairs rest
    hexPairs _ = []
