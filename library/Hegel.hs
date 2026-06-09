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
    runProperty_,

    -- * Settings and outcomes
    module Hegel.Settings,
    module Hegel.Outcome,
    module Hegel.Phase,

    -- * Writing properties
    module Hegel.Assertion,
  )
where

import Control.Exception (throwIO)
import Hegel.Assertion
import Hegel.Gen.Internal (Gen)
import Hegel.Native.Runner (runProperty)
import Hegel.Outcome
import Hegel.Phase
import Hegel.Settings

-- | Run a property and throw on anything other than success.
--
-- Throws 'PropertyFailed' on a counterexample, re-throws on 'Errored',
-- and 'fail's on rejection or health-check failure.
runProperty_ ::
  (Show a) =>
  Settings ->
  Gen a ->
  (a -> IO ()) ->
  IO ()
runProperty_ settings gen body = do
  outcome <- runProperty settings gen body
  case outcome of
    Passed _ -> pure ()
    Failed {counterexample, message, notes} -> throwIO (PropertyFailed counterexample message notes)
    Errored exc -> throwIO exc
    Rejected msg -> fail ("Property rejected all inputs: " <> show msg)
    UnhealthyInput msg -> fail ("Health check failed: " <> show msg)
