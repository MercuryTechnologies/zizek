-- | Map 'Settings' onto @libhegel@ settings setters.
module Hegel.Native.Settings
  ( applySettings,
  )
where

import Data.Bits ((.|.))
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word32)
import Foreign (Ptr)
import Foreign.C.String (withCString)
import Foreign.C.Types (CBool (..))
import Hegel.Database (Database (..))
import Hegel.HealthCheck (HealthCheck (..))
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
  hegel_settings_derandomize ptr (boolC s.derandomize)
  hegel_settings_report_multiple_failures ptr (boolC s.reportMultipleFailures)
  hegel_settings_phases ptr (phasesBitmask s.phases)
  hegel_settings_suppress_health_check ptr (hcBitmask s.suppressHealthCheck)
  -- "" disables the store; skipping the call leaves the engine default
  -- (.hegel/ under the cwd).
  case s.database of
    DatabaseDefault -> pure ()
    DatabaseDisabled -> withCString "" (hegel_settings_database ptr)
    DatabaseDirectory p -> withCString p (hegel_settings_database ptr)
  for_ s.databaseKey \key ->
    BS.useAsCString (encodeUtf8 key) (hegel_settings_database_key ptr)

boolC :: Bool -> CBool
boolC b = CBool (if b then 1 else 0)

-- | OR the per-phase flags into a bitmask. An empty list yields @0@ (no phases
-- enabled).
phasesBitmask :: [Phase] -> Word32
phasesBitmask = foldl' (\acc p -> acc .|. phaseFlag p) 0

phaseFlag :: Phase -> Word32
phaseFlag Explicit = HEGEL_PHASE_EXPLICIT
phaseFlag Reuse = HEGEL_PHASE_REUSE
phaseFlag Generate = HEGEL_PHASE_GENERATE
phaseFlag Target = HEGEL_PHASE_TARGET
phaseFlag Shrink = HEGEL_PHASE_SHRINK

hcBitmask :: [HealthCheck] -> Word32
hcBitmask = foldl' (\acc hc -> acc .|. healthCheckFlag hc) 0

healthCheckFlag :: HealthCheck -> Word32
healthCheckFlag FilterTooMuch = HEGEL_HC_FILTER_TOO_MUCH
healthCheckFlag TooSlow = HEGEL_HC_TOO_SLOW
healthCheckFlag TestCasesTooLarge = HEGEL_HC_TEST_CASES_TOO_LARGE
healthCheckFlag LargeInitialTestCase = HEGEL_HC_LARGE_INITIAL_TEST_CASE
