-- | Property-based testing with <https://hegel.dev>.
--
-- @
-- import Data.Function ((&))
-- import Hegel
-- import Hegel.Gen qualified as Gen
--
-- prop_addCommutes :: IO ()
-- prop_addCommutes =
--   'runProperty_' 'defaultSettings'
--     (Gen.int & Gen.build)
--     (\\x -> 'assert' (x + 0 == x) "identity")
-- @
module Hegel
  ( -- * Running properties
    Gen,
    runProperty,
    runPropertyWith,
    runProperty_,

    -- * Settings and reports
    module Hegel.Settings,
    module Hegel.Report,
    module Hegel.Phase,

    -- * Writing properties
    module Hegel.Assertion,
  )
where

import Control.Exception (throwIO)
import Hegel.Assertion
import Hegel.Gen.Internal (Gen)
import Hegel.Native.Runner (runProperty, runPropertyWith)
import Hegel.Phase
import Hegel.Report
import Hegel.Settings

-- | Run a property and throw on anything other than success.
--
-- Throws 'PropertyFailed' on a counterexample, re-throws on 'Errored',
-- and 'fail's on 'GaveUp' or health-check failure.
runProperty_ ::
  (Show a) =>
  Settings ->
  Gen a ->
  (a -> IO ()) ->
  IO ()
runProperty_ settings gen body = do
  report <- runProperty settings gen body
  case report.result of
    Ok -> pure ()
    Counterexample {message, notes} -> throwIO PropertyFailed {message, notes}
    GaveUp msg -> fail ("Property rejected all inputs: " <> show msg)
    Aborted (Errored exc) -> throwIO exc
    Aborted (UnhealthyInput msg) -> fail ("Health check failed: " <> show msg)
