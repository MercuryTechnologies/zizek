-- | A property monad that interleaves test logic with generator draws.
--
-- Where @'forEach' gen body@ separates generation from the test body, a
-- 'Property' may draw ('forAll'), perform 'IO', and assert in any order.
--
-- @
-- import Data.Function ((&))
-- import Data.Default.Class (def)
-- import Hegel.Gen qualified as Gen
-- import Hegel.Property
--
-- prop_reverseInvolutive :: IO 'Hegel.Report.Report'
-- prop_reverseInvolutive =
--   'check' def do
--     n  <- 'forAll' (Gen.int & Gen.min 0 & Gen.max 100 & Gen.build)
--     xs <- 'forAll' (Gen.list (Gen.int & Gen.build) & Gen.maxSize n & Gen.build)
--     'annotate' "comparing double reversal"
--     'assert' (reverse (reverse xs) == xs) "reverse is involutive"
-- @
--
-- Each captured failure retains its 'forAll' values and 'annotate'
-- entries as 'Hegel.Report.Note' values in the report.
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

    -- * Events
    event,
    eventValue,

    -- * Discards
    assume,
    discard,

    -- * Finalizers
    registerFinalizer,
    resource,
    resource_,

    -- * Forks

    -- | Only the handle type is exported here. Its operations
    -- ('Hegel.Property.Fork.spawn', 'Hegel.Property.Fork.join', etc.) and the
    -- bracketed 'Hegel.Property.Branch.concurrently' family are meant for
    -- qualified import, since their bare names collide with
    -- @Control.Concurrent.Async@\/@UnliftIO.Async@'s own.
    Fork,

    -- * Assertions
    assert,
    failure,
    (===),
    (/==),
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text.IO qualified as T
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hegel.Assertion (assert, failure, (/==), (===))
import Hegel.Gen.Internal (Gen)
import Hegel.Property.Fork (Fork)
import Hegel.Property.Internal
  ( Property,
    PropertyT,
    annotate,
    annotateShow,
    assume,
    discard,
    event,
    eventValue,
    footnote,
    forAll,
    forAllSilent,
    forAllWith,
    forAllWithLabel,
    hoist,
    registerFinalizer,
    resource,
    resource_,
  )
import Hegel.Report (Report (..), Result (Ok), throwOnFailure)
import Hegel.Report.Encoding qualified as Encoding
import Hegel.Runner (check)
import Hegel.Settings (Settings)
import System.IO (stderr)

-- | Run a property and throw on anything other than success
-- (via 'throwOnFailure').
--
-- A passing run writes whatever the engine printed, such as the block that
-- 'Hegel.Settings.showStatistics' enables, to 'stderr'.
check_ :: (HasCallStack) => Settings -> Property () -> IO ()
check_ settings prop = withFrozenCallStack do
  report <- check settings prop
  case report.result of
    Ok -> writeEngineOutput report.engineOutput
    _ -> pure ()
  throwOnFailure report

-- | Write engine output lines to 'stderr', transliterated to 7-bit ASCII when
-- 'stderr' can't encode them.
writeEngineOutput :: [Text] -> IO ()
writeEngineOutput [] = pure ()
writeEngineOutput output = do
  pref <- Encoding.preference stderr
  mapM_ (T.hPutStrLn stderr . Encoding.cleanFor pref) output

-- | Draw a value and run a test body against it, rendering drawn values via
-- their 'Show' instance.
forEach :: (Show a) => Gen a -> (a -> IO ()) -> Property ()
forEach gen body = forAll gen >>= liftIO . body

-- | 'forEach' with an explicit renderer, for values without a useful 'Show'.
forEachWith :: (a -> Text) -> Gen a -> (a -> IO ()) -> Property ()
forEachWith render gen body = forAllWith render gen >>= liftIO . body
