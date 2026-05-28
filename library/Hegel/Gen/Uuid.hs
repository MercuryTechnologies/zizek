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

import CBOR.Value (Value)
import Data.UUID (UUID, fromText)
import Hegel.Gen.Builder (Build (..))
import Hegel.Gen.Internal (basic)
import Hegel.Protocol.Cbor (ParseError (..), hegelText)
import Hegel.Schema (UuidSchema (version))
import Hegel.Schema qualified as Schema

data UuidBuilder = UuidBuilder
  { bVersion :: !(Maybe Int)
  }

-- | Generate a random UUID.
uuid :: UuidBuilder
uuid = UuidBuilder {bVersion = Nothing}

-- | Restrict generation to UUIDs of the given RFC 4122 version (1–5).
version :: Int -> UuidBuilder -> UuidBuilder
version n b = b {bVersion = Just n}

instance Build UuidBuilder UUID where
  build b = basic (Schema.uuid {version = b.bVersion}) parseUuid

parseUuid :: Value -> Either ParseError UUID
parseUuid v = case hegelText v of
  Left err -> Left err
  Right t -> case fromText t of
    Just u -> Right u
    Nothing -> Left ParseError {expected = "UUID string", got = v}
