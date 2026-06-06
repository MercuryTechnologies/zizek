-- | Map 'Settings' onto @libhegel@ settings setters.
module Hegel.Native.Settings
  ( applySettings,
  )
where

import Data.Bits ((.|.))
import Data.Text qualified as T
import Data.Word (Word32)
import Foreign (Ptr)
import Foreign.C.String (withCString)
import Foreign.C.Types (CBool (..))
import Hegel.Native.FFI
import Hegel.Phase (Phase (..))
import Hegel.Settings (Settings (..))

applySettings :: Settings -> Ptr HegelSettings -> IO ()
applySettings s ptr = do
  hegel_settings_mode ptr HEGEL_MODE_TEST_RUN
  hegel_settings_test_cases ptr (fromIntegral s.testCases)
  hegel_settings_verbosity ptr HEGEL_VERBOSITY_QUIET
  case s.seed of
    Just seed -> hegel_settings_seed ptr seed (CBool 1)
    Nothing -> hegel_settings_seed ptr 0 (CBool 0)
  -- NOTE: derandomize derives the seed from a hash of the database key, but we
  -- disable the database below and never set a database key, so as configured
  -- it has no effect; once the database is implemented this can be enabled
  -- properly.
  hegel_settings_derandomize ptr (boolC s.derandomize)
  hegel_settings_report_multiple_failures ptr (boolC s.reportMultipleFailures)
  hegel_settings_phases ptr (phasesBitmask s.phases)
  hegel_settings_suppress_health_check ptr (hcBitmask s.suppressHealthCheck)
  withCString "" (hegel_settings_database ptr)

boolC :: Bool -> CBool
boolC b = CBool (if b then 1 else 0)

phasesBitmask :: [Phase] -> Word32
phasesBitmask [] = HEGEL_PHASE_ALL
phasesBitmask ps = foldl' (\acc p -> acc .|. phaseFlag p) 0 ps

phaseFlag :: Phase -> Word32
phaseFlag Explicit = HEGEL_PHASE_EXPLICIT
phaseFlag Reuse = HEGEL_PHASE_REUSE
phaseFlag Generate = HEGEL_PHASE_GENERATE
phaseFlag Target = HEGEL_PHASE_TARGET
phaseFlag Shrink = HEGEL_PHASE_SHRINK

hcBitmask :: [T.Text] -> Word32
hcBitmask = foldl' (\acc t -> acc .|. wireFlag t) 0

wireFlag :: T.Text -> Word32
wireFlag "filter_too_much" = HEGEL_HC_FILTER_TOO_MUCH
wireFlag "too_slow" = HEGEL_HC_TOO_SLOW
wireFlag "test_cases_too_large" = HEGEL_HC_TEST_CASES_TOO_LARGE
wireFlag "large_initial_test_case" = HEGEL_HC_LARGE_INITIAL_TEST_CASE
wireFlag _ = 0
