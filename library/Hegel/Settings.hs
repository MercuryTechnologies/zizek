-- | Configuration for a single property run, shared across all backends.
module Hegel.Settings
  ( Settings (..),
    defaultSettings,
  )
where

import Data.Text (Text)
import Data.Word (Word64)
import Hegel.Database (Database (..))
import Hegel.HealthCheck (HealthCheck)
import Hegel.Phase (Phase (..))

-- | Configuration for a single property run.
data Settings = Settings
  { -- | Number of test cases to attempt.
    testCases :: !Int,
    -- | RNG seed. 'Nothing' picks a fresh seed each run.
    seed :: !(Maybe Word64),
    -- | Derive the seed from a hash of 'databaseKey' so runs are
    -- deterministic without an explicit seed.
    --
    -- Ignored when 'seed' is set; only meaningful when 'databaseKey' is set.
    derandomize :: !Bool,
    -- | Where failing examples are persisted for replay.
    database :: !Database,
    -- | Stable per-test identity inside the 'database'; replay only works
    -- when the same key is supplied on every run.
    databaseKey :: !(Maybe Text),
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
        . showString ", database = "
        . shows s.database
        . showString ", databaseKey = "
        . shows s.databaseKey
        . showString ", phases = "
        . shows s.phases
        . showString ", reportMultipleFailures = "
        . shows s.reportMultipleFailures
        . showString ", suppressHealthCheck = "
        . shows s.suppressHealthCheck
        . showString ", perCaseFinalizer = <<function>>}"

-- | Defaults for a property run: 100 test cases, a fresh seed each run,
-- all phases enabled, persistence disabled, and no per-case finalizer.
--
-- > defaultSettings { testCases = 1000 }
defaultSettings :: Settings
defaultSettings =
  Settings
    { testCases = 100,
      seed = Nothing,
      derandomize = False,
      database = DatabaseDisabled,
      databaseKey = Nothing,
      phases = [Explicit, Reuse, Generate, Target, Shrink],
      reportMultipleFailures = False,
      suppressHealthCheck = [],
      perCaseFinalizer = pure ()
    }
