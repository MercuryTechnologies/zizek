-- | A property monad: test logic interleaved with generator draws.
--
-- Where @'Hegel.runProperty' gen body@ separates generation from the test
-- body, a 'Property' may draw ('forAll'), perform 'IO', and assert in any
-- order — the engine shrinks across the whole interleaving:
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
-- 'annotate' becomes a 'Hegel.Report.Note' on the
-- 'Hegel.Report.Counterexample', re-collected by replaying the engine's
-- minimal failing case.
--
-- 'check' runs against the native backend; use
-- 'Hegel.Server.Runner.check' for the server backend. For application
-- monads, run @'PropertyT' App ()@ and collapse with 'hoist' before
-- checking.
module Hegel.Property
  ( -- * Properties
    Property,
    PropertyT,
    hoist,

    -- * Running properties
    check,

    -- * Draws
    forAll,
    forAllWith,
    forAllSilent,

    -- * Notes
    annotate,
    footnote,

    -- * Discards
    assume,
    discard,

    -- * Assertions
    assert,
    failure,
  )
where

import Hegel.Assertion (assert, failure)
import Hegel.Native.Runner (check)
import Hegel.Property.Internal
  ( Property,
    PropertyT,
    annotate,
    assume,
    discard,
    footnote,
    forAll,
    forAllSilent,
    forAllWith,
    hoist,
  )
