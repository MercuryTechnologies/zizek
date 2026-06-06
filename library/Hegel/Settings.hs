-- | Configuration for a single property run, shared across all backends.
module Hegel.Settings
  ( Settings (..),
    defaultSettings,
  )
where

import Data.Word (Word64)
import Hegel.HealthCheck (HealthCheck)
import Hegel.Phase (Phase (..))

-- | Configuration for a single property run.
data Settings = Settings
  { -- | Number of test cases to attempt.
    testCases :: !Int,
    -- | RNG seed. 'Nothing' picks a fresh seed each run.
    seed :: !(Maybe Word64),
    -- | Use a fixed, source-derived seed so failures reproduce; ignored when
    -- 'seed' is set.
    derandomize :: !Bool,
    -- | Phases the engine should execute, in order.
    phases :: ![Phase],
    -- | When 'True', the engine collects every distinct failure instead of
    -- stopping at the first.
    reportMultipleFailures :: !Bool,
    -- | Health checks to skip.
    suppressHealthCheck :: ![HealthCheck],
    -- | Action run after each test case, on both success and failure.
    perCaseFinalizer :: !(IO ())
  }

instance Show Settings where
  showsPrec p s =
    showParen (p > 10) $
      showString "Settings {testCases = "
        . shows s.testCases
        . showString ", seed = "
        . shows s.seed
        . showString ", derandomize = "
        . shows s.derandomize
        . showString ", phases = "
        . shows s.phases
        . showString ", reportMultipleFailures = "
        . shows s.reportMultipleFailures
        . showString ", suppressHealthCheck = "
        . shows s.suppressHealthCheck
        . showString ", perCaseFinalizer = <<function>>}"

-- | Defaults for a property run: 100 test cases, a fresh seed each run,
-- all phases enabled, and no per-case finalizer.
--
-- Customize by overriding individual fields:
--
-- > defaultSettings { testCases = 1000 }
defaultSettings :: Settings
defaultSettings =
  Settings
    { testCases = 100,
      seed = Nothing,
      derandomize = False,
      phases = [Explicit, Reuse, Generate, Target, Shrink],
      reportMultipleFailures = False,
      suppressHealthCheck = [],
      perCaseFinalizer = pure ()
    }
