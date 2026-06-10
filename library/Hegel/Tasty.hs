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

import Data.Text qualified as T
import Hegel.Native.Runner (check)
import Hegel.Property.Internal (Property)
import Hegel.Report (Report (..), Result (..), renderReport)
import Hegel.Settings (Settings, defaultSettings)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Providers (IsTest (..), singleTest, testFailed, testPassed)

-- | A property scheduled with its 'Settings'.
data HegelTest = HegelTest Settings (Property ())

instance IsTest HegelTest where
  testOptions = pure []
  run _opts (HegelTest settings prop) _progress = do
    report <- check settings prop
    let rendered = T.unpack (renderReport report)
    pure case report.result of
      Ok -> testPassed rendered
      _ -> testFailed rendered

-- | Run a property as a test-tree leaf, with 'defaultSettings'.
testProperty :: TestName -> Property () -> TestTree
testProperty = testPropertyWith defaultSettings

-- | 'testProperty' with explicit 'Settings'.
testPropertyWith :: Settings -> TestName -> Property () -> TestTree
testPropertyWith settings name prop = singleTest name (HegelTest settings prop)
