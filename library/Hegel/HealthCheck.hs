-- | Health checks the engine can be told to suppress.
module Hegel.HealthCheck
  ( HealthCheck (..),
    toWire,
  )
where

import Data.Text (Text)

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

toWire :: HealthCheck -> Text
toWire FilterTooMuch = "filter_too_much"
toWire TooSlow = "too_slow"
toWire TestCasesTooLarge = "test_cases_too_large"
toWire LargeInitialTestCase = "large_initial_test_case"
