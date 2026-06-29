-- | Health checks the engine can be told to suppress.
module Hegel.HealthCheck
  ( HealthCheck (..),
  )
where

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
