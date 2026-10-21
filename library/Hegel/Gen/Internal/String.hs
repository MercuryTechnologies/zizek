-- | Shared construction pattern for the string-generator leaves: build a
-- caller-owned @HegelStringGenerator@ handle once per
-- 'Hegel.Gen.Internal.Gen' value via 'unsafePerformIO' and @NOINLINE@, then
-- draw from it any number of times.
--
-- A caller that must validate its configuration before the first draw binds
-- the 'stringGen'\/'stringDraw' result to a name outside any 'Draw' lambda,
-- so the handle still builds once, and runs the check from a 'Draw' with the
-- same label that calls 'Hegel.Gen.Internal.drawInline' on that name.
-- Sequencing the check with @('>>=')@ instead adds a flat-map span the
-- plain leaf doesn't have. See "Hegel.Gen.Text" or "Hegel.Gen.Domain" for the
-- shape.
module Hegel.Gen.Internal.String
  ( stringGen,
    stringDraw,
  )
where

import Data.Text (Text)
import Data.Word (Word64)
import Foreign.ForeignPtr (ForeignPtr)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hegel.Gen.Builder (engineRejection)
import Hegel.Gen.Internal (Gen (..))
import Hegel.Internal.DataSource (HegelStringGenerator, drawString)
import Hegel.Internal.TestCase (TestCase)
import System.IO.Unsafe (unsafePerformIO)

-- | Draw the generator's raw 'Text' unchanged, in a span labelled @label@.
--
-- An engine rejection of the configuration surfaces as a 'Hegel.Exception.ValidationError' in context @what@.
stringGen :: (HasCallStack) => Text -> Word64 -> IO (ForeignPtr HegelStringGenerator) -> Gen Text
stringGen what label mkGen = Draw label \tc -> withFrozenCallStack (engineRejection what (drawString tc genFP))
  where
    genFP = unsafePerformIO mkGen
    {-# NOINLINE genFP #-}

-- | Draw the generator's raw 'Text' and post-process it against the live
-- 'TestCase', so a failing post-process can throw rather than only 'fmap'
-- over the result. The draw runs in a span labelled @label@, and an engine
-- rejection of the configuration surfaces as a 'Hegel.Exception.ValidationError' in context @what@.
stringDraw :: (HasCallStack) => Text -> Word64 -> IO (ForeignPtr HegelStringGenerator) -> (TestCase -> Text -> IO a) -> Gen a
stringDraw what label mkGen post = Draw label \tc -> withFrozenCallStack (engineRejection what (drawString tc genFP)) >>= post tc
  where
    genFP = unsafePerformIO mkGen
    {-# NOINLINE genFP #-}
