-- | 'ByteString' generator.
--
-- Bounded length via 'Hegel.Gen.Builder.minSize' and 'Hegel.Gen.Builder.maxSize':
--
-- > Gen.binary & Gen.minSize 4 & Gen.maxSize 64 & Gen.build
module Hegel.Gen.Binary
  ( BinaryBuilder,
    binary,
  )
where

import Data.ByteString (ByteString)
import Hegel.Gen.Builder (Build (..), HasSize (..), checkSizeBounds)
import Hegel.Gen.Internal (Gen (..))
import Hegel.Internal.DataSource (drawBytes)

data BinaryBuilder = BinaryBuilder
  { bMinSize :: !Int,
    bMaxSize :: !(Maybe Int)
  }

-- | Generate a random 'ByteString'.
binary :: BinaryBuilder
binary = BinaryBuilder {bMinSize = 0, bMaxSize = Nothing}

instance HasSize BinaryBuilder where
  minSize n b = b {bMinSize = n}
  maxSize n b = b {bMaxSize = Just n}

instance Build BinaryBuilder ByteString where
  build b = Draw \tc -> do
    checkSizeBounds "Hegel.Gen.Binary" b.bMinSize b.bMaxSize
    drawBytes tc wireLo wireHi
    where
      -- Converted once here, not per draw.
      wireLo = fromIntegral b.bMinSize
      wireHi = maybe maxBound fromIntegral b.bMaxSize
