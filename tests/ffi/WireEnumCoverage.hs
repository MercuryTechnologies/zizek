-- | Cross-check that the wire values our conversions produce are accepted by
-- the libhegel enums.
--
-- The compile-time half of the guard lives in @cbits/wire_enum_guard.c@
-- (exhaustive @switch@es under @-Werror=switch-enum@): if hegel-rust /adds/ an
-- enumerator, this test target fails to build. This module is the runtime half:
-- it feeds every value our 'Witch.From' instances produce to the matching guard
-- and asserts it is recognized (returns @0@), catching /value drift/ — a
-- conversion whose code no longer matches the header. Span labels have no enum, so
-- their check compares our name-derived values against the engine's own
-- derivation.
module WireEnumCoverage (wireEnumCoverageSpec) where

import Data.ByteString qualified as BS
import Data.Foldable (for_, traverse_)
import Data.Int (Int64)
import Data.List (nub)
import Data.Word (Word32, Word64)
import Foreign (alloca, peek, withArrayLen)
import Foreign.C.Types (CInt (..))
import Hegel.Backend (Backend (..))
import Hegel.HealthCheck (HealthCheck (..))
import Hegel.Internal.DataSource (Label (..), combineLabels, labelName)
import Hegel.Internal.Foreign.Raw
import Hegel.Internal.TestCase (Status (..))
import Hegel.Nondeterminism (Nondeterminism (..))
import Hegel.Phase (Phase (..))
import Hegel.Verbosity (Verbosity (..))
import Test.Hspec
import Witch qualified

foreign import ccall unsafe "hegel_guard_backend" guardBackend :: Word32 -> IO CInt

foreign import ccall unsafe "hegel_guard_verbosity" guardVerbosity :: Word32 -> IO CInt

foreign import ccall unsafe "hegel_guard_nondeterminism" guardNondeterminism :: Word32 -> IO CInt

foreign import ccall unsafe "hegel_guard_phase" guardPhase :: Word32 -> IO CInt

foreign import ccall unsafe "hegel_guard_health_check" guardHealthCheck :: Word32 -> IO CInt

foreign import ccall unsafe "hegel_guard_status" guardStatus :: Word32 -> IO CInt

foreign import ccall unsafe "hegel_guard_result" guardResult :: CInt -> IO CInt

-- | Assert the guard recognizes (returns @0@ for) every supplied wire value.
allRecognized :: (w -> IO CInt) -> [w] -> Expectation
allRecognized guard = traverse_ \w -> guard w `shouldReturn` 0

wireEnumCoverageSpec :: Spec
wireEnumCoverageSpec = describe "wire enum coverage (conversion values vs hegel.h)" $ do
  it "Backend" $
    allRecognized guardBackend (Witch.into @Word32 <$> [Default, Urandom])
  it "Verbosity" $
    allRecognized guardVerbosity (Witch.into @Word32 <$> [Quiet, Normal, Verbose, Debug])
  it "Nondeterminism" $
    allRecognized guardNondeterminism (Witch.into @Word32 <$> [Tolerate, Warn, Forbid])
  it "Phase" $
    allRecognized guardPhase (Witch.into @Word32 <$> [Explicit, Reuse, Generate, Target, Shrink])
  it "HealthCheck" $
    allRecognized
      guardHealthCheck
      (Witch.into @Word32 <$> [FilterTooMuch, TooSlow, TestCasesTooLarge, LargeInitialTestCase])
  describe "Label" $ do
    it "derives every label the way hegel_label_from_name does" $
      withContext \ctx ->
        for_ [minBound .. maxBound :: Label] \label -> do
          engine <- BS.useAsCString (labelName label) \name ->
            alloca \out -> do
              hegel_label_from_name ctx name out >>= throwOnError ctx
              peek out
          Witch.into @Word64 label `shouldBe` engine
    it "gives every label a distinct value" $ do
      let labels = Witch.into @Word64 <$> [minBound .. maxBound :: Label]
      length (nub labels) `shouldBe` length labels
    it "combines labels the way hegel_label_combine does" $
      withContext \ctx ->
        for_ [[], [1], [1, 2], [2, 1], Witch.into @Word64 <$> [minBound .. maxBound :: Label]] \labels -> do
          engine <- withArrayLen labels \len arr ->
            alloca \out -> do
              hegel_label_combine ctx arr (fromIntegral len) out >>= throwOnError ctx
              peek out
          combineLabels labels `shouldBe` engine
  it "Status" $
    allRecognized guardStatus (Witch.into @Word32 <$> [Valid, Invalid, Overrun, Interesting "x"])
  it "HEGEL_STATE_MACHINE_DONE is INT64_MIN" $
    HEGEL_STATE_MACHINE_DONE `shouldBe` (minBound :: Int64)
  it "hegel_result_t" $
    allRecognized
      guardResult
      [ HEGEL_OK,
        HEGEL_E_STOP_TEST,
        HEGEL_E_ASSUME,
        HEGEL_E_BACKEND,
        HEGEL_E_INVALID_HANDLE,
        HEGEL_E_INVALID_ARG,
        HEGEL_E_ALREADY_COMPLETE,
        HEGEL_E_NOT_COMPLETE,
        HEGEL_E_INTERNAL,
        HEGEL_E_CONCURRENT_USE
      ]
