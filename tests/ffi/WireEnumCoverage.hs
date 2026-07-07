-- | Cross-check that the wire values our conversions produce are accepted by
-- the libhegel enums.
--
-- The compile-time half of the guard lives in @cbits/wire_enum_guard.c@
-- (exhaustive @switch@es under @-Werror=switch-enum@): if hegel-rust /adds/ an
-- enumerator, this test target fails to build. This module is the runtime half:
-- it feeds every value our 'Witch.From' instances produce to the matching guard
-- and asserts it is recognized (returns @0@), catching /value drift/ — a
-- conversion whose code no longer matches the header.
module WireEnumCoverage (wireEnumCoverageSpec) where

import Data.Foldable (traverse_)
import Data.Word (Word32, Word64)
import Foreign.C.Types (CInt (..))
import Hegel.Backend (Backend (..))
import Hegel.HealthCheck (HealthCheck (..))
import Hegel.Internal.DataSource (Label (..))
import Hegel.Internal.TestCase (Status (..))
import Hegel.Phase (Phase (..))
import Hegel.Verbosity (Verbosity (..))
import Test.Hspec
import Witch qualified

foreign import ccall unsafe "hegel_guard_backend" guardBackend :: CInt -> IO CInt

foreign import ccall unsafe "hegel_guard_verbosity" guardVerbosity :: CInt -> IO CInt

foreign import ccall unsafe "hegel_guard_phase" guardPhase :: Word32 -> IO CInt

foreign import ccall unsafe "hegel_guard_health_check" guardHealthCheck :: Word32 -> IO CInt

foreign import ccall unsafe "hegel_guard_label" guardLabel :: Word64 -> IO CInt

foreign import ccall unsafe "hegel_guard_status" guardStatus :: CInt -> IO CInt

-- | Assert the guard recognizes (returns @0@ for) every supplied wire value.
allRecognized :: (w -> IO CInt) -> [w] -> Expectation
allRecognized guard = traverse_ \w -> guard w `shouldReturn` 0

wireEnumCoverageSpec :: Spec
wireEnumCoverageSpec = describe "wire enum coverage (conversion values vs hegel.h)" $ do
  it "Backend" $
    allRecognized guardBackend (Witch.into @CInt <$> [Auto, Default, Urandom])
  it "Verbosity" $
    allRecognized guardVerbosity (Witch.into @CInt <$> [Quiet, Normal, Verbose, Debug])
  it "Phase" $
    allRecognized guardPhase (Witch.into @Word32 <$> [Explicit, Reuse, Generate, Target, Shrink])
  it "HealthCheck" $
    allRecognized
      guardHealthCheck
      (Witch.into @Word32 <$> [FilterTooMuch, TooSlow, TestCasesTooLarge, LargeInitialTestCase])
  it "Label" $
    allRecognized
      guardLabel
      ( Witch.into @Word64
          <$> [ LabelList,
                LabelListElement,
                LabelSet,
                LabelSetElement,
                LabelMap,
                LabelMapEntry,
                LabelTuple,
                LabelOneOf,
                LabelOptional,
                LabelFixedDict,
                LabelFlatMap,
                LabelFilter,
                LabelMapped,
                LabelSampledFrom,
                LabelEnumVariant,
                LabelFeatureFlag
              ]
      )
  it "Status" $
    allRecognized guardStatus (Witch.into @CInt <$> [Valid, Invalid, Overrun, Interesting "x"])
