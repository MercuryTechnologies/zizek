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
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.Word (Word8)
import Hegel.Gen.Builder (Build (..), GenValidationError (..))
import Hegel.Gen.Internal (Gen (..))
import Hegel.Internal.DataSource (InvariantViolation (..), drawUuid)

newtype UuidBuilder = UuidBuilder
  { bVersion :: Maybe Word8
  }

-- | Generate a random UUID.
uuid :: UuidBuilder
uuid = UuidBuilder {bVersion = Nothing}

-- | Restrict generation to UUIDs of the given version, which must fit in a
-- nibble.
--
-- RFC 9562 defines versions 1 through 8.
version :: Word8 -> UuidBuilder -> UuidBuilder
version n b = b {bVersion = Just n}

instance Build UuidBuilder UUID where
  build b = Draw \tc -> do
    checkVersion b.bVersion
    bytes <- drawUuid tc b.bVersion
    case UUID.fromByteString (BSL.fromStrict bytes) of
      Just u -> pure u
      Nothing ->
        throwIO
          InvariantViolation {detail = "libhegel: uuid draw did not return 16 bytes"}
    where
      checkVersion :: Maybe Word8 -> IO ()
      checkVersion (Just v)
        | v > 15 =
            throwIO
              GenValidationError
                { context = "Hegel.Gen.Uuid",
                  detail = "version (" <> T.pack (show v) <> ") > 15 (RFC 4122 nibble range)"
                }
      checkVersion _ = pure ()
