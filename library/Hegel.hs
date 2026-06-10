-- | Property-based testing with <https://hegel.dev>.
--
-- @
-- import Data.Function ((&))
-- import Hegel
-- import Hegel.Gen qualified as Gen
--
-- prop_addCommutes :: IO ()
-- prop_addCommutes =
--   'prop' (Gen.int & Gen.build) (\\x -> 'assert' (x + 0 == x) "identity")
-- @
module Hegel
  ( -- * Running properties
    Gen,
    prop,
    forEach,
    forEachWith,

    -- * Settings and reports
    module Hegel.Settings,
    module Hegel.Database,
    module Hegel.Report,
    module Hegel.Phase,

    -- * Writing properties
    module Hegel.Assertion,
  )
where

import Hegel.Assertion
import Hegel.Database
import Hegel.Gen.Internal (Gen)
import Hegel.Phase
import Hegel.Property (check_, forEach, forEachWith)
import Hegel.Report
import Hegel.Settings

-- | 'check_' with 'defaultSettings' and 'forEach': the shortest spelling for
-- use inside a test framework's @it@\/@testCase@, where the framework owns the
-- label and reports the thrown failure.
prop :: (Show a) => Gen a -> (a -> IO ()) -> IO ()
prop gen body = check_ defaultSettings (forEach gen body)
