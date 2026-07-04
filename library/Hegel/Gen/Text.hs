-- | 'Text' generator.
--
-- Bounded length via 'Hegel.Gen.Builder.minSize' and 'Hegel.Gen.Builder.maxSize':
--
-- > Gen.text & Gen.minSize 1 & Gen.maxSize 64 & Gen.build
--
-- Surrogates are excluded by default (not representable in 'Data.Text.Text').
module Hegel.Gen.Text
  ( TextBuilder,
    text,
  )
where

import Data.Text (Text)
import Hegel.Gen.Builder (Build (..), HasSize (..), checkSizeBounds)
import Hegel.Gen.Internal (Gen (..))
import Hegel.Internal.DataSource (buildTextGen, drawString)
import System.IO.Unsafe (unsafePerformIO)

data TextBuilder = TextBuilder
  { bMinSize :: !Int,
    bMaxSize :: !(Maybe Int)
  }

-- | Generate a random 'Text' value.
text :: TextBuilder
text = TextBuilder {bMinSize = 0, bMaxSize = Nothing}

instance HasSize TextBuilder where
  minSize n b = b {bMinSize = n}
  maxSize n b = b {bMaxSize = Just n}

instance Build TextBuilder Text where
  build b = Draw \tc -> do
    checkSizeBounds "Hegel.Gen.Text" b.bMinSize b.bMaxSize
    drawString tc genFP
    where
      genFP = unsafePerformIO gen
      {-# NOINLINE genFP #-}
      -- No codec\/codepoint\/category restriction beyond excluding
      -- surrogates, which 'Data.Text.Text' cannot represent.
      gen =
        buildTextGen
          (fromIntegral b.bMinSize)
          (maybe maxBound fromIntegral b.bMaxSize)
          Nothing
          0
          maxBound
          Nothing
          (Just ["Cs"])
          Nothing
          Nothing
