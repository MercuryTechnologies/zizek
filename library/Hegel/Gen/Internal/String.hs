-- | Shared construction pattern for the string-generator leaves: build a
-- caller-owned @HegelStringGenerator@ handle once per
-- 'Hegel.Gen.Internal.Gen' value via 'unsafePerformIO' and @NOINLINE@, then
-- draw from it any number of times.
--
-- A caller that must validate its configuration before the first draw binds
-- the 'stringGen'\/'stringDraw' result to a name outside any 'Draw' lambda,
-- so the handle still builds once, and runs the check from a 'Draw' that
-- calls 'Hegel.Gen.Internal.draw' on that name. Sequencing the check with
-- @('>>=')@ instead adds a @FLAT_MAP@ span the plain leaf doesn't have. See
-- "Hegel.Gen.Text" or "Hegel.Gen.Domain" for the shape.
module Hegel.Gen.Internal.String
  ( stringGen,
    stringDraw,
  )
where

import Data.Text (Text)
import Foreign.ForeignPtr (ForeignPtr)
import Hegel.Gen.Internal (Gen (..))
import Hegel.Internal.DataSource (HegelStringGenerator, drawString)
import Hegel.Internal.TestCase (TestCase)
import System.IO.Unsafe (unsafePerformIO)

-- | Draw the generator's raw 'Text' unchanged.
stringGen :: IO (ForeignPtr HegelStringGenerator) -> Gen Text
stringGen mkGen = Draw \tc -> drawString tc genFP
  where
    genFP = unsafePerformIO mkGen
    {-# NOINLINE genFP #-}

-- | Draw the generator's raw 'Text' and post-process it against the live
-- 'TestCase', so a failing post-process can throw rather than only 'fmap'
-- over the result.
stringDraw :: IO (ForeignPtr HegelStringGenerator) -> (TestCase -> Text -> IO a) -> Gen a
stringDraw mkGen post = Draw \tc -> drawString tc genFP >>= post tc
  where
    genFP = unsafePerformIO mkGen
    {-# NOINLINE genFP #-}
