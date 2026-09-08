module Hegel.Internal.Replay
  ( ReplayToken,
    makeReplayToken,
    tokenVersionOf,
    tokenOriginOf,
    tokenBlobOf,
  )
where

import Data.ByteString (ByteString)
import Data.Text (Text)

data ReplayToken = ReplayToken
  { tokenVersion :: !Text,
    tokenOrigin :: !Text,
    tokenBlob :: !ByteString
  }
  deriving stock (Eq, Show)

makeReplayToken :: Text -> Text -> ByteString -> ReplayToken
makeReplayToken = ReplayToken

tokenVersionOf :: ReplayToken -> Text
tokenVersionOf ReplayToken {tokenVersion} = tokenVersion

tokenOriginOf :: ReplayToken -> Text
tokenOriginOf ReplayToken {tokenOrigin} = tokenOrigin

tokenBlobOf :: ReplayToken -> ByteString
tokenBlobOf ReplayToken {tokenBlob} = tokenBlob
