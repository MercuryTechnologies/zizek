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
import Hegel.Gen.Builder (Build (..), HasSize (..), requireOrdered)
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
  build b = case b.bMaxSize of
    Just hi -> requireOrdered "Gen.text" b.bMinSize hi go
    Nothing -> go
    where
      go = Draw \tc -> drawString tc genFP
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
