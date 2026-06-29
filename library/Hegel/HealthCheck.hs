-- | Health checks the engine can be told to suppress.
module Hegel.HealthCheck
  ( HealthCheck (..),
  )
where

import Data.Word (Word32)
import Hegel.Internal.FFI
  ( pattern HEGEL_HC_FILTER_TOO_MUCH,
    pattern HEGEL_HC_LARGE_INITIAL_TEST_CASE,
    pattern HEGEL_HC_TEST_CASES_TOO_LARGE,
    pattern HEGEL_HC_TOO_SLOW,
  )
import Witch qualified

-- | A health check that can be individually suppressed for a run.
data HealthCheck
  = -- | Too many generated examples were filtered out.
    FilterTooMuch
  | -- | Test cases took too long to run.
    TooSlow
  | -- | Generated test cases were too large.
    TestCasesTooLarge
  | -- | The first generated test case was already too large.
    LargeInitialTestCase
  deriving stock (Show, Eq)

-- | The @hegel_health_check_t@ single-bit wire flag.
--
-- OR these together for @hegel_settings_set_suppress_health_check@.
instance Witch.From HealthCheck Word32 where
  from FilterTooMuch = HEGEL_HC_FILTER_TOO_MUCH
  from TooSlow = HEGEL_HC_TOO_SLOW
  from TestCasesTooLarge = HEGEL_HC_TEST_CASES_TOO_LARGE
  from LargeInitialTestCase = HEGEL_HC_LARGE_INITIAL_TEST_CASE
