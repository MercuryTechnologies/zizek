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
import Hegel.Property.Internal (Property)
import Hegel.Report (Report (..), Result (..), renderReport, renderReportAnsi)
import Hegel.Runner (check)
import Hegel.Settings (Settings, defaultSettings)
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
    let render = if useColor then renderReportAnsi else renderReport
        rendered = T.unpack (render report)
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

-- | Run a property as a test-tree leaf, with 'defaultSettings'.
testProperty :: TestName -> Property () -> TestTree
testProperty = testPropertyWith defaultSettings

-- | 'testProperty' with explicit 'Settings'.
testPropertyWith :: Settings -> TestName -> Property () -> TestTree
testPropertyWith settings name prop = singleTest name (HegelTest settings prop)
