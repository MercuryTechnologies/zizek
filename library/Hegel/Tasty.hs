-- | tasty integration: run a 'Property' as a 'TestTree' leaf.
--
-- @
-- testGroup "reverse"
--   [ 'testProperty' "is involutive" do
--       xs <- 'Hegel.Property.forAll' (Gen.list (Gen.int & Gen.build) & Gen.build)
--       reverse (reverse xs) 'Hegel.Property.===' xs
--   ]
-- @
--
-- Native properties default to no persistence or identity. An explicit key
-- must distinguish properties sharing a database; a key alone enables no store.
-- To reuse legacy examples, enable the same store and supply the old
-- @Module:leaf label@ key explicitly. Existing database contents are preserved.
--
-- Source settings are overridden by shared @HEGEL_*@ environment variables,
-- then by Tasty's effective options, including @TASTY_HEGEL_*@ variables and
-- @localOption@. Missing overrides preserve earlier values. Counts must fit
-- a nonnegative 'Int', step limits must be positive, and seeds must fit Word64.
-- Database values are @off@, @default@, and @directory:PATH@.
--
-- Supply @--hegel-replay@ and @--hegel-replay-key@ together to replay the exact
-- matching explicit identity once, bypassing phases and database access.
-- Shared @HEGEL_REPLAY@ and @HEGEL_REPLAY_KEY@ provide the same request.
-- Filter with @-p@ and check the replay-selection message; a leaf cannot detect
-- that a requested key matched no selected test. Other tests explore normally.
--
-- Use @tasty-hspec@ for automatic Hspec identities. Converted specs use shared
-- @HEGEL_*@ configuration; native provider options do not configure them.
module Hegel.Tasty
  ( testProperty,
    testPropertyWith,
    testPropertyModify,
    HegelTestCases (..),
    HegelStatefulSteps (..),
    HegelSeed (..),
    HegelDatabase (..),
    HegelReplay (..),
    HegelReplayKey (..),
  )
where

import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import GHC.Clock (getMonotonicTimeNSec)
import Hegel.Internal.RunnerConfig qualified as Config
import Hegel.Property.Internal (Property)
import Hegel.Report (Report (..), Result (..), renderReportAuto)
import Hegel.Report.Style qualified as Style
import Hegel.Settings (Settings, defaultSettings)
import System.Environment (lookupEnv)
import System.IO (hIsTerminalDevice, stderr, stdout)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Ingredients.ConsoleReporter (UseColor (..))
import Test.Tasty.Options (IsOption (..), OptionDescription (..), OptionSet, lookupOption)
import Test.Tasty.Providers (IsTest (..), Progress (..), singleTest, testFailed, testPassed)
import UnliftIO.IORef (newIORef, readIORef, writeIORef)

-- | A property scheduled with its 'Settings'.
data HegelTest = HegelTest Settings (Property ())

instance IsTest HegelTest where
  testOptions =
    pure
      [ Option (Proxy @HegelTestCases),
        Option (Proxy @HegelStatefulSteps),
        Option (Proxy @HegelSeed),
        Option (Proxy @HegelDatabase),
        Option (Proxy @HegelReplay),
        Option (Proxy @HegelReplayKey)
      ]
  run opts (HegelTest settings prop) progress = do
    environment <- Config.readOverrides
    let configuration = do
          low <- environment
          high <- optionOverrides opts
          let combined = Config.overlay low high
          resolved <- Config.resolve settings combined
          pure (resolved, combined)
    case configuration of
      Left message -> pure (testFailed message)
      Right (resolved, overrides) -> do
        lastUpdate <- newIORef 0
        let completed count = do
              now <- getMonotonicTimeNSec
              previous <- readIORef lastUpdate
              if previous == 0 || now - previous >= 100000000
                then do
                  writeIORef lastUpdate now
                  progress (Progress (show count <> " completed cases") 0)
                else pure ()
        report <- Config.execute completed resolved overrides prop
        useColor <- resolveColor (lookupOption opts)
        pref <- Style.preference stdout
        rendered <- renderReportAuto useColor pref report
        let output = T.unpack (rendered <> Style.cleanFor pref (Config.replayInstructions True resolved overrides))
        pure case report.result of
          Ok -> testPassed output
          _ -> testFailed output

-- | Resolve a 'UseColor' setting to a concrete 'Bool'. 'Auto' honors the
-- @NO_COLOR@ environment variable (per <https://no-color.org>) and falls
-- back to a terminal check.
resolveColor :: UseColor -> IO Bool
resolveColor Never = pure False
resolveColor Always = pure True
resolveColor Auto = do
  noColor <- isJust <$> lookupEnv "NO_COLOR"
  if noColor
    then pure False
    else hIsTerminalDevice stderr

-- | Run a native Tasty property with persistence disabled.
-- Use 'testPropertyWith' with a unique explicit database key to persist failures.
-- For automatic Hspec identities inside Tasty, convert Hspec specs with tasty-hspec.
testProperty :: TestName -> Property () -> TestTree
testProperty = testPropertyWith defaultSettings

-- | Run with explicit settings. Persistence requires a nonempty database key
-- that distinguishes this property from every other property sharing its database.
-- A key alone leaves persistence disabled.
testPropertyWith :: Settings -> TestName -> Property () -> TestTree
testPropertyWith settings name prop = singleTest name (HegelTest settings prop)

-- | Customize native Tasty defaults, which disable persistence.
testPropertyModify :: (Settings -> Settings) -> TestName -> Property () -> TestTree
testPropertyModify modify = testPropertyWith (modify defaultSettings)

-- | Optional native Tasty override for @--hegel-test-cases@.
newtype HegelTestCases = HegelTestCases (Maybe String)
  deriving stock (Eq, Show)

instance IsOption HegelTestCases where
  defaultValue = HegelTestCases Nothing
  parseValue = Just . HegelTestCases . Just
  optionName = pure "hegel-test-cases"
  optionHelp = pure "Nonnegative case budget; also accepts TASTY_HEGEL_TEST_CASES"

-- | Optional native Tasty override for @--hegel-stateful-steps@.
newtype HegelStatefulSteps = HegelStatefulSteps (Maybe String)
  deriving stock (Eq, Show)

instance IsOption HegelStatefulSteps where
  defaultValue = HegelStatefulSteps Nothing
  parseValue = Just . HegelStatefulSteps . Just
  optionName = pure "hegel-stateful-steps"
  optionHelp = pure "Positive per-case stateful step limit; also accepts TASTY_HEGEL_STATEFUL_STEPS"

-- | Optional native Tasty override for @--hegel-seed@.
newtype HegelSeed = HegelSeed (Maybe String)
  deriving stock (Eq, Show)

instance IsOption HegelSeed where
  defaultValue = HegelSeed Nothing
  parseValue = Just . HegelSeed . Just
  optionName = pure "hegel-seed"
  optionHelp = pure "Unsigned 64-bit seed; also accepts TASTY_HEGEL_SEED"

-- | Optional native Tasty override for @--hegel-database@.
newtype HegelDatabase = HegelDatabase (Maybe String)
  deriving stock (Eq, Show)

instance IsOption HegelDatabase where
  defaultValue = HegelDatabase Nothing
  parseValue = Just . HegelDatabase . Just
  optionName = pure "hegel-database"
  optionHelp = pure "Store: off, default, or directory:PATH; persistence requires a key; also accepts TASTY_HEGEL_DATABASE"

-- | Optional native Tasty override for @--hegel-replay@.
newtype HegelReplay = HegelReplay (Maybe String)
  deriving stock (Eq, Show)

instance IsOption HegelReplay where
  defaultValue = HegelReplay Nothing
  parseValue = Just . HegelReplay . Just
  optionName = pure "hegel-replay"
  optionHelp = pure "Replay token paired with --hegel-replay-key; matching replay bypasses phases and database; also accepts TASTY_HEGEL_REPLAY"

-- | Optional native Tasty override for @--hegel-replay-key@.
newtype HegelReplayKey = HegelReplayKey (Maybe String)
  deriving stock (Eq, Show)

instance IsOption HegelReplayKey where
  defaultValue = HegelReplayKey Nothing
  parseValue = Just . HegelReplayKey . Just
  optionName = pure "hegel-replay-key"
  optionHelp = pure "Exact identity paired with --hegel-replay; filter to the intended test; also accepts TASTY_HEGEL_REPLAY_KEY"

optionOverrides :: OptionSet -> Either String Config.Overrides
optionOverrides opts = Config.parseOverrides (\name -> lookup name values >>= id)
  where
    HegelTestCases testCases = lookupOption opts
    HegelStatefulSteps statefulSteps = lookupOption opts
    HegelSeed seed = lookupOption opts
    HegelDatabase database = lookupOption opts
    HegelReplay replay = lookupOption opts
    HegelReplayKey replayKey = lookupOption opts
    values = [("test-cases", testCases), ("stateful-steps", statefulSteps), ("seed", seed), ("database", database), ("replay", replay), ("replay-key", replayKey)]
