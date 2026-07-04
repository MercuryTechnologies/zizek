-- | 'UUID' generator.
--
-- Generate a random UUID (any version by default):
--
-- > Gen.uuid & Gen.build
--
-- Pin to a specific RFC 4122 version:
--
-- > Gen.uuid & Gen.version 4 & Gen.build
module Hegel.Gen.Uuid
  ( -- * Builder
    UuidBuilder,
    uuid,

    -- * Modifiers
    version,
  )
where

import Control.Exception (throwIO)
import Data.ByteString.Lazy qualified as BSL
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.Word (Word8)
import Hegel.Gen.Builder (Build (..))
import Hegel.Gen.Internal (Gen (..))
import Hegel.Internal.DataSource (InvariantViolation (..), drawUuid)

newtype UuidBuilder = UuidBuilder
  { bVersion :: Maybe Word8
  }

-- | Generate a random UUID.
uuid :: UuidBuilder
uuid = UuidBuilder {bVersion = Nothing}

-- | Restrict generation to UUIDs of the given RFC 4122 version (1–5).
version :: Word8 -> UuidBuilder -> UuidBuilder
version n b = b {bVersion = Just n}

instance Build UuidBuilder UUID where
  build b = Draw \tc -> do
    bytes <- drawUuid tc b.bVersion
    case UUID.fromByteString (BSL.fromStrict bytes) of
      Just u -> pure u
      Nothing ->
        throwIO
          InvariantViolation {detail = "libhegel: uuid draw did not return 16 bytes"}
