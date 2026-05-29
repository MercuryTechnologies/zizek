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
    module Hegel.Runner,
    module Hegel.Outcome,
    module Hegel.Phase,

    -- * Writing properties
    module Hegel.Assertion,

    -- * Session management
    module Hegel.Session,

    -- * Lower-level client
    module Hegel.Client,

    -- * Errors
    module Hegel.Protocol.Error,
  )
where

import Control.Exception (throwIO)
import Hegel.Assertion
import Hegel.Client
import Hegel.Gen.Internal (Gen)
import Hegel.Outcome
import Hegel.Phase
import Hegel.Protocol.Error
import Hegel.Runner
import Hegel.Session

-- | Run a property against the global 'Session', returning a structured 'Outcome'.
--
-- Use this when integrating with a custom harness or when you need 'Stats',
-- notes, or the counterexample value programmatically.
runProperty ::
  Settings ->
  Gen a ->
  (a -> IO ()) ->
  IO (Outcome a)
runProperty = runPropertyOn globalSession

-- | Run a property and throw on anything other than success.
--
-- Throws 'PropertyFailed' on a counterexample, re-throws the original
-- exception on 'Errored', and 'fail's on rejection or health-check failure.
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
