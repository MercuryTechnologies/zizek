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

import Control.Exception (throwIO)
import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Gen.Builder (Build (..), GenValidationError (..))
import Hegel.Gen.Internal (Gen (..))
import Hegel.Internal.DataSource (buildDomainGen, drawString)
import System.IO.Unsafe (unsafePerformIO)

newtype DomainBuilder = DomainBuilder
  { bMaxLength :: Int
  }

-- | Generate a random fully qualified domain name. Default max length is
-- 255, RFC 1035's maximum.
domain :: DomainBuilder
domain = DomainBuilder {bMaxLength = 255}

-- | Set the maximum total length of the generated domain name, in @[4, 255]@.
maxLength :: Int -> DomainBuilder -> DomainBuilder
maxLength n b = b {bMaxLength = n}

instance Build DomainBuilder Text where
  build b = Draw \tc -> do
    checkMaxLength b.bMaxLength
    drawString tc genFP
    where
      genFP = unsafePerformIO (buildDomainGen (fromIntegral b.bMaxLength))
      {-# NOINLINE genFP #-}
      checkMaxLength :: Int -> IO ()
      checkMaxLength n
        | n < 4 || n > 255 =
            throwIO
              GenValidationError
                { context = "Hegel.Gen.Domain",
                  detail = "maxLength (" <> T.pack (show n) <> ") outside [4, 255]"
                }
        | otherwise = pure ()
