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
import Data.Word (Word64)
import Hegel.Gen.Builder (Build (..))
import Hegel.Gen.Internal (Gen (..))
import Hegel.Internal.DataSource (buildDomainGen, drawString)
import System.IO.Unsafe (unsafePerformIO)

newtype DomainBuilder = DomainBuilder
  { bMaxLength :: Word64
  }

-- | Generate a random fully qualified domain name. Default max length is
-- 255, RFC 1035's maximum.
domain :: DomainBuilder
domain = DomainBuilder {bMaxLength = 255}

-- | Set the maximum total length of the generated domain name (4..=255).
maxLength :: Word64 -> DomainBuilder -> DomainBuilder
maxLength n b = b {bMaxLength = n}

instance Build DomainBuilder Text where
  build b = Draw \tc -> drawString tc genFP
    where
      genFP = unsafePerformIO (buildDomainGen b.bMaxLength)
      {-# NOINLINE genFP #-}
