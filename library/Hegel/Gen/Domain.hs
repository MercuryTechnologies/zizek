-- | Domain name generator.
--
-- Generate a random RFC 1035 fully qualified domain name:
--
-- > Gen.domain & Gen.build
--
-- Bound the total length:
--
-- > Gen.domain & Gen.maxLength 50 & Gen.build
module Hegel.Gen.Domain
  ( -- * Builder
    DomainBuilder,
    domain,

    -- * Modifiers
    maxLength,
  )
where

import Data.Text (Text)
import Hegel.Gen.Builder (Build (..))
import Hegel.Gen.Internal (basic)
import Hegel.Internal.CBOR (hegelText)
import Hegel.Internal.Schema (DomainSchema (maxLength))
import Hegel.Internal.Schema qualified as Schema

data DomainBuilder = DomainBuilder
  { bMaxLength :: !(Maybe Int)
  }

-- | Generate a random fully qualified domain name.
domain :: DomainBuilder
domain = DomainBuilder {bMaxLength = Nothing}

-- | Set the maximum total length of the generated domain name.
maxLength :: Int -> DomainBuilder -> DomainBuilder
maxLength n b = b {bMaxLength = Just n}

instance Build DomainBuilder Text where
  build b = basic (Schema.domain {maxLength = b.bMaxLength}) hegelText
