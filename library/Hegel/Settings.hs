-- | Configuration for a single property run.
module Hegel.Settings
  ( Settings (..),
    defaultSettings,
    withDatabaseKey,
  )
where

import Data.Default.Class (Default (..))
import Data.Text (Text)
import Data.Word (Word64)
import Hegel.Backend (Backend (..))
import Hegel.Database (Database (..))
import Hegel.HealthCheck (HealthCheck)
import Hegel.Phase (Phase (..))
import Hegel.Verbosity (Verbosity (..))

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
    -- | The engine's source of randomness.
    backend :: !Backend,
    -- | How much diagnostic output the engine emits during a run.
    verbosity :: !Verbosity,
    -- | When 'True', the engine collects every distinct failure instead of
    -- stopping at the first.
    reportMultipleFailures :: !Bool,
    -- | Health checks to skip.
    suppressHealthCheck :: ![HealthCheck]
  }
  deriving stock (Show)

-- | Defaults for a property run: 100 test cases, a fresh seed each run,
-- all phases enabled, the automatic backend, quiet output, and persistence
-- disabled.
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
      backend = Auto,
      verbosity = Quiet,
      reportMultipleFailures = False,
      suppressHealthCheck = []
    }

-- | Alias for 'defaultSettings'.
instance Default Settings where
  def = defaultSettings

-- | Set the stable 'databaseKey' used to file and replay failures, leaving the
-- 'database' (where, or whether, they are persisted) untouched.
--
-- This backs the automatic keying in "Hegel.Hspec" and "Hegel.Tasty";
-- persistence itself is chosen by 'database'.
withDatabaseKey :: Text -> Settings -> Settings
withDatabaseKey key s = s {databaseKey = Just key}
