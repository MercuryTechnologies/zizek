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
    resource,
    resource_,

    -- * Sampling
    sample,
    samples,

    -- * Properties
    Property,
    PropertyT,
    hoist,
    check,
    forAll,
    forAllWith,
    forAllWithLabel,
    forAllSilent,
    annotate,
    annotateShow,
    footnote,
    assume,
    discard,

    -- * Forks

    -- | Only the handle type is exported here; import
    -- "Hegel.Property.Fork" or "Hegel.Property.Branch" qualified for the
    -- operations, since their bare names collide with
    -- @Control.Concurrent.Async@\/@UnliftIO.Async@'s own.
    Fork,

    -- * Settings and reports
    module Hegel.Settings,
    module Hegel.Backend,
    module Hegel.Verbosity,
    module Hegel.Database,
    module Hegel.HealthCheck,
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
import Hegel.HealthCheck
import Hegel.Phase
import Hegel.Pool
import Hegel.Property
  ( Fork,
    Property,
    PropertyT,
    annotate,
    annotateShow,
    assume,
    check,
    check_,
    discard,
    footnote,
    forAll,
    forAllSilent,
    forAllWith,
    forAllWithLabel,
    forEach,
    forEachWith,
    hoist,
    registerFinalizer,
    resource,
    resource_,
  )
import Hegel.Report
import Hegel.Runner (sample, samples)
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
