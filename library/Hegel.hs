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

import Hegel.Assertion
import Hegel.Gen.Internal (Gen)
import Hegel.Native.Runner (runProperty, runPropertyWith)
import Hegel.Phase
import Hegel.Report
import Hegel.Settings

-- | Run a property and throw on anything other than success
-- (via 'throwOnFailure').
runProperty_ ::
  (Show a) =>
  Settings ->
  Gen a ->
  (a -> IO ()) ->
  IO ()
runProperty_ settings gen body = throwOnFailure =<< runProperty settings gen body
