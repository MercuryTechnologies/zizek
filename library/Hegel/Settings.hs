-- | Configuration for a single property run.
--
-- Every run starts from a settings profile that @libhegel@ resolves: the
-- shipped @development@, @ci@, and @workload@ profiles, any defined in a
-- @hegel.toml@ in the working directory or an ancestor, and the engine's
-- @HEGEL_*@ settings environment variables applied over the profile. A
-- 'Settings' value holds overrides on top of that profile, and every field
-- left 'Nothing' keeps the profile's value.
module Hegel.Settings
  ( Settings (..),
    TestLocation (..),
    defaultSettings,
    defaultMaxCloneDepth,
    validate,
    SettingsError (..),
    withDatabaseKey,
  )
where

import Control.Applicative ((<|>))
import Data.Default.Class (Default (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32)
import GHC.Stack (HasCallStack, callStack)
import Hegel.Backend (Backend (..))
import Hegel.Database (Database (..))
import Hegel.Exception (Diagnostic (..), SettingsError (..))
import Hegel.HealthCheck (HealthCheck)
import Hegel.Nondeterminism (Nondeterminism (..))
import Hegel.Phase (Phase (..))
import Hegel.Seed (Seed (..))
import Hegel.Verbosity (Verbosity (..))

-- | Overrides for a single property run, layered over the resolved profile.
--
-- Combining two values with '<>' keeps every field the right-hand side sets
-- and falls back to the left-hand side for the rest.
data Settings = Settings
  { -- | The profile to start from. 'Nothing' selects the default profile,
    -- which is @ci@ on a CI server, @workload@ inside Antithesis, and
    -- @development@ otherwise, unless a @hegel.toml@ or
    -- @HEGEL_DEFAULT_PROFILE@ names another.
    profile :: !(Maybe Text),
    -- | Nonnegative number of valid test cases to run.
    testCases :: !(Maybe Int),
    -- | The seed random generation starts from.
    seed :: !(Maybe Seed),
    -- | Derive a 'SeedFresh' seed from a hash of 'databaseKey' so runs are
    -- deterministic without a 'SeedFixed' one.
    derandomize :: !(Maybe Bool),
    -- | Where failing examples are persisted for replay.
    database :: !(Maybe Database),
    -- | Stable per-test identity inside the 'database'. Nothing is persisted
    -- or replayed without one, and replay only works when the same key is
    -- supplied on every run.
    databaseKey :: !(Maybe Text),
    -- | Where the test is defined. Inside Antithesis the engine reports the
    -- verdict of every run as an assertion located here, and elsewhere the
    -- location is unused. Like the key, it is per-test identity, so a
    -- registered profile does not keep it.
    testLocation :: !(Maybe TestLocation),
    -- | Phases the engine should execute, in order.
    phases :: !(Maybe [Phase]),
    -- | The engine's source of randomness.
    backend :: !(Maybe Backend),
    -- | How much diagnostic output the engine emits during a run. 'Nothing'
    -- keeps the engine quiet unless the profile asks for 'Verbose' or 'Debug'
    -- output.
    --
    -- That output is collected into the run's 'Hegel.Report.engineOutput'.
    verbosity :: !(Maybe Verbosity),
    -- | When 'True', the engine collects every distinct failure instead of
    -- stopping at the first.
    reportMultipleFailures :: !(Maybe Bool),
    -- | Health checks to skip.
    suppressHealthCheck :: !(Maybe [HealthCheck]),
    -- | Let a test case make any number of choices. By default the engine
    -- concludes a test case as an overrun once it makes 2^20 of them, which
    -- bounds the memory a run spends recording choices.
    --
    -- A test case meant to run for a long time, such as a concurrent state
    -- machine driven for hours, needs this on.
    unboundedChoices :: !(Maybe Bool),
    -- | Print a statistics block at the end of the run, summarizing the
    -- labels recorded with 'Hegel.Property.event' and
    -- 'Hegel.Property.eventValue' over the generated test cases. The block
    -- arrives in the run's 'Hegel.Report.engineOutput'.
    showStatistics :: !(Maybe Bool),
    -- | How the run reacts to a test that behaves differently when the same
    -- choices are replayed.
    nondeterminism :: !(Maybe Nondeterminism),
    -- | Whether a failure's report carries the replay token that reproduces
    -- it.
    printBlob :: !(Maybe Bool),
    -- | Ceiling on how deeply 'Hegel.Property.Fork.spawn' and the
    -- @Branch.concurrently@ family may nest clone streams within one test
    -- case, 'defaultMaxCloneDepth' when unset. This must be nonnegative; zero
    -- permits properties that create no clones.
    maxCloneDepth :: !(Maybe Int)
  }
  deriving stock (Show)

-- | Where a test is defined, for the engine's Antithesis reporting, which
-- names the property @'scope'::'function' passes properties@.
data TestLocation = TestLocation
  { -- | The source file that defines the test.
    file :: !Text,
    -- | The line in 'file' where the test's definition begins, which must
    -- fit in an unsigned 32-bit integer.
    line :: !Int,
    -- | The module enclosing the test.
    scope :: !Text,
    -- | The test's name.
    function :: !Text
  }
  deriving stock (Eq, Show)

-- | Keeps every field the right-hand side sets.
instance Semigroup Settings where
  a <> b =
    Settings
      { profile = b.profile <|> a.profile,
        testCases = b.testCases <|> a.testCases,
        seed = b.seed <|> a.seed,
        derandomize = b.derandomize <|> a.derandomize,
        database = b.database <|> a.database,
        databaseKey = b.databaseKey <|> a.databaseKey,
        testLocation = b.testLocation <|> a.testLocation,
        phases = b.phases <|> a.phases,
        backend = b.backend <|> a.backend,
        verbosity = b.verbosity <|> a.verbosity,
        reportMultipleFailures = b.reportMultipleFailures <|> a.reportMultipleFailures,
        suppressHealthCheck = b.suppressHealthCheck <|> a.suppressHealthCheck,
        unboundedChoices = b.unboundedChoices <|> a.unboundedChoices,
        showStatistics = b.showStatistics <|> a.showStatistics,
        nondeterminism = b.nondeterminism <|> a.nondeterminism,
        printBlob = b.printBlob <|> a.printBlob,
        maxCloneDepth = b.maxCloneDepth <|> a.maxCloneDepth
      }

-- | 'defaultSettings', which overrides nothing.
instance Monoid Settings where
  mempty = defaultSettings

-- | Run under the default profile exactly as it resolves, with no key.
defaultSettings :: Settings
defaultSettings =
  Settings
    { profile = Nothing,
      testCases = Nothing,
      seed = Nothing,
      derandomize = Nothing,
      database = Nothing,
      databaseKey = Nothing,
      testLocation = Nothing,
      phases = Nothing,
      backend = Nothing,
      verbosity = Nothing,
      reportMultipleFailures = Nothing,
      suppressHealthCheck = Nothing,
      unboundedChoices = Nothing,
      showStatistics = Nothing,
      nondeterminism = Nothing,
      printBlob = Nothing,
      maxCloneDepth = Nothing
    }

-- | Alias for 'defaultSettings'.
instance Default Settings where
  def = defaultSettings

-- | The clone nesting ceiling used when 'maxCloneDepth' is unset.
defaultMaxCloneDepth :: Int
defaultMaxCloneDepth = 32

-- | Set the stable 'databaseKey' used to file and replay failures, leaving the
-- 'database' (where, or whether, they are persisted) untouched.
--
-- Keys must distinguish properties sharing a database.
withDatabaseKey :: Text -> Settings -> Settings
withDatabaseKey key s = s {databaseKey = Just key}

-- | Require nonnegative case and clone counts, and a test location line that
-- fits in an unsigned 32-bit integer.
validate :: (HasCallStack) => Settings -> Either SettingsError ()
validate s
  | Just n <- s.testCases, n < 0 = invalid "testCases" n "must be nonnegative"
  | Just n <- s.maxCloneDepth, n < 0 = invalid "maxCloneDepth" n "must be nonnegative"
  | Just loc <- s.testLocation, loc.line < 0 || toInteger loc.line > toInteger (maxBound :: Word32) = invalid "testLocation" loc.line "line must fit in an unsigned 32-bit integer"
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
