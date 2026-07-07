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
    registerFinalizer,

    -- * Settings and reports
    module Hegel.Settings,
    module Hegel.Backend,
    module Hegel.Verbosity,
    module Hegel.Database,
    module Hegel.Report,
    module Hegel.Phase,

    -- * Writing properties
    module Hegel.Assertion,

    -- * Stateful testing
    module Hegel.Pool,
    Machine (..),
    Rule (..),
    Invariant (..),
    runMachine,
  )
where

import Hegel.Assertion
import Hegel.Backend
import Hegel.Database
import Hegel.Gen.Internal (Gen)
import Hegel.Phase
import Hegel.Pool
import Hegel.Property (check_, forEach, forEachWith, registerFinalizer)
import Hegel.Property.Internal (PropertyT)
import Hegel.Report
import Hegel.Settings
import Hegel.Stateful (Invariant (..), Machine (..), Rule (..))
import Hegel.Stateful qualified as Stateful
import Hegel.Verbosity
import UnliftIO (MonadUnliftIO)

-- | 'check_' with 'defaultSettings' and 'forEach': the shortest spelling for
-- use inside a test framework's @it@\/@testCase@, where the framework owns the
-- label and reports the thrown failure.
prop :: (Show a) => Gen a -> (a -> IO ()) -> IO ()
prop gen body = check_ defaultSettings (forEach gen body)

-- | Run a stateful test specified by a 'Machine'. Sugar for 'Stateful.run'.
runMachine :: (MonadUnliftIO m) => Machine s m -> PropertyT m ()
runMachine = Stateful.run
