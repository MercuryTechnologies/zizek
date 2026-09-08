-- | Configuration for a single property run.
module Hegel.Settings
  ( Settings (..),
    defaultSettings,
    validate,
    SettingsError (..),
    withDatabaseKey,
  )
where

import Data.Default.Class (Default (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import GHC.Stack (HasCallStack, callStack)
import Hegel.Backend (Backend (..))
import Hegel.Database (Database (..))
import Hegel.Exception (Diagnostic (..), SettingsError (..))
import Hegel.HealthCheck (HealthCheck)
import Hegel.Phase (Phase (..))
import Hegel.Verbosity (Verbosity (..))

-- | Configuration for a single property run.
data Settings = Settings
  { -- | Nonnegative number of test cases to attempt.
    testCases :: !Int,
    -- | Target number of steps each stateful test case runs. Every case
    -- runs at least one step and at most this many. The engine chooses
    -- where in that range to stop.
    --
    -- Must be at least 1.
    statefulStepCount :: !Int,
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
    suppressHealthCheck :: ![HealthCheck],
    -- | Ceiling on how deeply 'Hegel.Property.Fork.spawn' and the
    -- @Branch.concurrently@ family may nest clone streams within one test
    -- case. This must be nonnegative; zero permits properties that create no clones.
    maxCloneDepth :: !Int
  }
  deriving stock (Show)

-- | Defaults for a property run: 100 test cases, up to 50 steps per
-- stateful test case, a fresh seed each run, all phases enabled, the
-- automatic backend, quiet output, and persistence disabled.
defaultSettings :: Settings
defaultSettings =
  Settings
    { testCases = 100,
      statefulStepCount = 50,
      seed = Nothing,
      derandomize = False,
      database = DatabaseDisabled,
      databaseKey = Nothing,
      phases = [Explicit, Reuse, Generate, Target, Shrink],
      backend = Auto,
      verbosity = Quiet,
      reportMultipleFailures = False,
      suppressHealthCheck = [],
      maxCloneDepth = 32
    }

-- | Alias for 'defaultSettings'.
instance Default Settings where
  def = defaultSettings

-- | Set the stable 'databaseKey' used to file and replay failures, leaving the
-- 'database' (where, or whether, they are persisted) untouched.
--
-- Keys must distinguish properties sharing a database; persistence itself
-- is chosen by 'database'.
withDatabaseKey :: Text -> Settings -> Settings
withDatabaseKey key s = s {databaseKey = Just key}

-- | Require nonnegative case and clone counts and at least one stateful step.
validate :: (HasCallStack) => Settings -> Either SettingsError ()
validate s
  | s.testCases < 0 = invalid "testCases" s.testCases "must be nonnegative"
  | s.statefulStepCount < 1 = invalid "statefulStepCount" s.statefulStepCount "must be at least 1"
  | s.maxCloneDepth < 0 = invalid "maxCloneDepth" s.maxCloneDepth "must be nonnegative"
  | otherwise = Right ()
  where
    invalid :: Text -> Int -> Text -> Either SettingsError ()
    invalid name value detail =
      Left
        ( SettingsError
            Diagnostic
              { context = "Hegel.Settings." <> name,
                detail,
                values = [(name, T.pack (show value))],
                callStack = callStack
              }
        )
