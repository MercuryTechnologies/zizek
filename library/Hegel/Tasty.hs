-- | tasty integration: run a 'Property' as a 'TestTree' leaf.
--
-- @
-- testGroup "reverse"
--   [ 'testProperty' "is involutive" do
--       xs <- 'Hegel.Property.forAll' (Gen.list (Gen.int & Gen.build) & Gen.build)
--       reverse (reverse xs) 'Hegel.Property.===' xs
--   ]
-- @
module Hegel.Tasty
  ( testProperty,
    testPropertyWith,
  )
where

import Data.Maybe (isJust)
import Data.Text qualified as T
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)
import Hegel.Database (Database (..))
import Hegel.Internal.DatabaseKey (propKey)
import Hegel.Property.Internal (Property)
import Hegel.Report (Report (..), Result (..), renderReportRich, renderReportRichAnsi)
import Hegel.Runner (check)
import Hegel.Settings (Settings (..), defaultSettings, withDatabaseKey)
import System.Environment (lookupEnv)
import System.IO (hIsTerminalDevice, stderr)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Ingredients.ConsoleReporter (UseColor (..))
import Test.Tasty.Options (lookupOption)
import Test.Tasty.Providers (IsTest (..), singleTest, testFailed, testPassed)

-- | A property scheduled with its 'Settings'.
data HegelTest = HegelTest Settings (Property ())

instance IsTest HegelTest where
  testOptions = pure []
  run opts (HegelTest settings prop) _progress = do
    report <- check settings prop
    useColor <- resolveColor (lookupOption opts)
    -- The rich renderer falls back internally to the plain renderer when the
    -- result is not a Counterexample or source files can't be read.
    let render = if useColor then renderReportRichAnsi else renderReportRich
    rendered <- T.unpack <$> render report
    pure case report.result of
      Ok -> testPassed rendered
      _ -> testFailed rendered

-- | Resolve a 'UseColor' setting to a concrete 'Bool'. 'Auto' honours the
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

-- | Run a property as a test-tree leaf with persistence enabled.
--
-- The example-database key is derived from the leaf @name@ (salted with the
-- call-site module) and persistence is switched on, so failures replay across
-- runs with no hand-written key.
--
-- A tasty leaf cannot see its enclosing 'Test.Tasty.testGroup', so the key is
-- @\"\<module\>:\<name\>\"@ (which means two identically-named leaves in one
-- module will collide).
--
-- To disambiguate, a 'Hegel.Settings.databaseKey' can be explicitly provided
-- via 'testPropertyWith'.
--
-- For no persistence, use @testPropertyWith def@.
testProperty :: (HasCallStack) => TestName -> Property () -> TestTree
testProperty = withFrozenCallStack (testPropertyWith defaultSettings {database = DatabaseDefault})

-- | 'testProperty' with explicit 'Settings'.
--
-- A key is derived from @name@ only when @settings@ has no
-- 'Hegel.Settings.databaseKey' of its own.
--
-- Persistence follows the settings as given, so @testPropertyWith def@ runs
-- with no database.
testPropertyWith :: (HasCallStack) => Settings -> TestName -> Property () -> TestTree
testPropertyWith settings name prop =
  let settings' = case settings.databaseKey of
        Just _ -> settings
        Nothing -> withDatabaseKey (propKey callStack [] name) settings
   in singleTest name (HegelTest settings' prop)
