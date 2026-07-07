-- | A property monad that interleaves test logic with generator draws.
--
-- Where @'forEach' gen body@ separates generation from the test body, a
-- 'Property' may draw ('forAll'), perform 'IO', and assert in any order.
--
-- @
-- import Data.Function ((&))
-- import Hegel (defaultSettings)
-- import Hegel.Gen qualified as Gen
-- import Hegel.Property
--
-- prop_reverseInvolutive :: IO 'Hegel.Report.Report'
-- prop_reverseInvolutive =
--   'check' defaultSettings do
--     n  <- 'forAll' (Gen.int & Gen.min 0 & Gen.max 100 & Gen.build)
--     xs <- 'forAll' (Gen.list (Gen.int & Gen.build) & Gen.maxSize n & Gen.build)
--     'annotate' "comparing double reversal"
--     'assert' (reverse (reverse xs) == xs) "reverse is involutive"
-- @
--
-- Failures are reported through the journal: each 'forAll' value and
-- 'annotate' call becomes a 'Hegel.Report.Note' on the
-- 'Hegel.Report.Counterexample', which are collected when the engine's
-- minimal failing case is replayed.
module Hegel.Property
  ( -- * Properties
    Property,
    PropertyT,
    hoist,

    -- * Running properties
    check,
    check_,

    -- * Draws
    forEach,
    forEachWith,
    forAll,
    forAllWith,
    forAllWithLabel,
    forAllSilent,

    -- * Notes
    annotate,
    annotateShow,
    footnote,

    -- * Discards
    assume,
    discard,

    -- * Finalizers
    registerFinalizer,

    -- * Assertions
    assert,
    failure,
    (===),
    (/==),
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Hegel.Assertion (assert, failure, (/==), (===))
import Hegel.Gen.Internal (Gen)
import Hegel.Property.Internal
  ( Property,
    PropertyT,
    annotate,
    annotateShow,
    assume,
    discard,
    footnote,
    forAll,
    forAllSilent,
    forAllWith,
    forAllWithLabel,
    hoist,
    registerFinalizer,
  )
import Hegel.Report (throwOnFailure)
import Hegel.Runner (check)
import Hegel.Settings (Settings)

-- | Run a property and throw on anything other than success
-- (via 'throwOnFailure').
check_ :: Settings -> Property () -> IO ()
check_ settings prop = throwOnFailure =<< check settings prop

-- | Draw a value and run a test body against it, rendering drawn values via
-- their 'Show' instance.
forEach :: (Show a) => Gen a -> (a -> IO ()) -> Property ()
forEach gen body = forAll gen >>= liftIO . body

-- | 'forEach' with an explicit renderer, for values without a useful 'Show'.
forEachWith :: (a -> Text) -> Gen a -> (a -> IO ()) -> Property ()
forEachWith render gen body = forAllWith render gen >>= liftIO . body
