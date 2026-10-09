{-# LANGUAGE ImplicitParams #-}

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
-- Native properties have no identity, so nothing persists until a key is
-- supplied. An explicit key must distinguish properties sharing a database,
-- which the resolved settings profile provides unless the settings override it.
-- To reuse legacy examples, supply the old @Module:leaf label@ key explicitly.
-- Existing database contents are preserved.
--
-- The engine's @HEGEL_*@ settings variables apply beneath the source settings,
-- and Tasty's effective options, including @TASTY_HEGEL_*@ variables and
-- @localOption@, apply over them. Missing overrides preserve earlier values.
-- Counts must fit a nonnegative 'Int', and seeds are @none@ or must fit
-- Word64. Database values are @disabled@ or a directory path.
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
import GHC.Stack (CallStack, HasCallStack, callStack, withFrozenCallStack)
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
data HegelTest = HegelTest CallStack Settings (Property ())

instance IsTest HegelTest where
  testOptions =
    pure
      [ Option (Proxy @HegelTestCases),
        Option (Proxy @HegelSeed),
        Option (Proxy @HegelDatabase),
        Option (Proxy @HegelReplay),
        Option (Proxy @HegelReplayKey)
      ]
  run opts (HegelTest cs settings prop) progress =
    let ?callStack = cs
     in withFrozenCallStack $ do
          environment <- Config.readOverrides
          let configuration = do
                low <- environment
                high <- optionOverrides opts
                let combined = low <> high
                resolved <- withFrozenCallStack (Config.resolve settings combined)
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

-- | Run a native Tasty property with no database key, so nothing persists.
-- Use 'testPropertyWith' with a unique explicit database key to persist failures.
-- For automatic Hspec identities inside Tasty, convert Hspec specs with tasty-hspec.
testProperty :: (HasCallStack) => TestName -> Property () -> TestTree
testProperty = withFrozenCallStack $ testPropertyWith defaultSettings

-- | Run with explicit settings. Persistence requires a nonempty database key
-- that distinguishes this property from every other property sharing its database.
testPropertyWith :: (HasCallStack) => Settings -> TestName -> Property () -> TestTree
testPropertyWith settings name prop = singleTest name (HegelTest callStack settings prop)

-- | Customize native Tasty defaults, which have no database key.
testPropertyModify :: (HasCallStack) => (Settings -> Settings) -> TestName -> Property () -> TestTree
testPropertyModify modify = withFrozenCallStack $ testPropertyWith (modify defaultSettings)

-- | Optional native Tasty override for @--hegel-test-cases@.
newtype HegelTestCases = HegelTestCases (Maybe String)
  deriving stock (Eq, Show)

instance IsOption HegelTestCases where
  defaultValue = HegelTestCases Nothing
  parseValue = Just . HegelTestCases . Just
  optionName = pure "hegel-test-cases"
  optionHelp = pure "Nonnegative case budget; also accepts TASTY_HEGEL_TEST_CASES"

-- | Optional native Tasty override for @--hegel-seed@.
newtype HegelSeed = HegelSeed (Maybe String)
  deriving stock (Eq, Show)

instance IsOption HegelSeed where
  defaultValue = HegelSeed Nothing
  parseValue = Just . HegelSeed . Just
  optionName = pure "hegel-seed"
  optionHelp = pure "Unsigned 64-bit seed, or none for a fresh one; also accepts TASTY_HEGEL_SEED"

-- | Optional native Tasty override for @--hegel-database@.
newtype HegelDatabase = HegelDatabase (Maybe String)
  deriving stock (Eq, Show)

instance IsOption HegelDatabase where
  defaultValue = HegelDatabase Nothing
  parseValue = Just . HegelDatabase . Just
  optionName = pure "hegel-database"
  optionHelp = pure "Store: disabled, or a directory path; persistence requires a key; also accepts TASTY_HEGEL_DATABASE"

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
    HegelSeed seed = lookupOption opts
    HegelDatabase database = lookupOption opts
    HegelReplay replay = lookupOption opts
    HegelReplayKey replayKey = lookupOption opts
    values = [("test-cases", testCases), ("seed", seed), ("database", database), ("replay", replay), ("replay-key", replayKey)]
