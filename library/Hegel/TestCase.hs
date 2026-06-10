-- | Per-test-case vtable: a record of closures through which generators
-- communicate with whichever backend is driving the current run.
--
-- Each backend constructs a 'TestCase' value that closes over its own
-- transport; the 'Gen.*' tree and 'Hegel.Collection' are entirely
-- independent of the backend chosen.
module Hegel.TestCase
  ( -- * Test case
    TestCase (..),

    -- * Generation
    generate,
    TestStopped (..),

    -- * Collections
    newCollection,
    collectionMore,
    collectionReject,

    -- * Spans
    Label (..),
    labelValue,
    startSpan,
    stopSpan,

    -- * Completion
    Status (..),
    markComplete,

    -- * Capabilities
    UnsupportedCapability (..),
  )
where

import CBOR.Value (Value)
import Control.Exception (Exception)
import Data.Text (Text)

-- | A per-test-case vtable. Each field is a closure that dispatches to the
-- active backend. The 'Gen.*' tree and 'Hegel.Collection' call through
-- these fields via the free-function shims below.
--
-- Construct via 'Hegel.Native.TestCase.mkTestCase'.
data TestCase = TestCase
  { -- | Ask the engine for a value matching the CBOR schema; throws
    -- 'TestStopped' when the choice budget is exhausted.
    generate :: Value -> IO Value,
    -- | Begin a variable-length collection; returns its integer ID.
    -- Throws 'TestStopped' on exhaustion.
    newCollection :: Int -> Maybe Int -> IO Int,
    -- | Ask whether the engine wants another element.
    -- Throws 'TestStopped' on exhaustion.
    collectionMore :: Int -> IO Bool,
    -- | Notify the engine that the last element was rejected.
    -- Throws 'TestStopped' if the engine gives up.
    collectionReject :: Int -> Maybe Text -> IO (),
    -- | Open a labeled span.
    startSpan :: Label -> IO (),
    -- | Close the most-recently-opened span.
    -- Pass 'True' to mark it discarded.
    stopSpan :: Bool -> IO (),
    -- | Report the final outcome for this test case.
    markComplete :: Status -> IO ()
  }

-- | Thrown when the engine signals that the current test case should be
-- abandoned (choice budget exhausted). The runner treats this as a discard.
data TestStopped = TestStopped
  deriving stock (Show)

instance Exception TestStopped

-- | Thrown when a generator uses a per-case primitive the active backend does
-- not implement (e.g. pools on a backend whose protocol lacks them). The
-- server runner maps this to an 'Hegel.Report.Errored' abort.
newtype UnsupportedCapability = UnsupportedCapability Text
  deriving stock (Show)

instance Exception UnsupportedCapability

-- | Final outcome of a test case, sent via 'markComplete'.
data Status
  = -- | The case completed successfully.
    Valid
  | -- | The case was deliberately discarded (an assume\/filter rejection).
    -- Runners tally these as invalid cases, distinct from 'Overrun'.
    Invalid
  | -- | The case ran out of entropy mid-generation. Not counted as a
    -- rejection; it is a budget-exhaustion signal (e.g. a shrink probe).
    Overrun
  | -- | The case failed; the payload is the origin string used for
    -- deduplication.
    Interesting Text

-- | Span labels used to group related draws so the engine can shrink them
-- as a unit. Numeric values match @libhegel@'s constants.
data Label
  = LabelList
  | LabelListElement
  | LabelSet
  | LabelSetElement
  | LabelMap
  | LabelMapEntry
  | LabelTuple
  | LabelOneOf
  | LabelOptional
  | LabelFixedDict
  | LabelFlatMap
  | LabelFilter
  | LabelMapped
  | LabelSampledFrom
  | LabelEnumVariant
  deriving stock (Show)

-- | Map a 'Label' to its integer wire identifier (1–15, matching
-- @HEGEL_LABEL_*@ constants).
labelValue :: Label -> Int
labelValue LabelList = 1
labelValue LabelListElement = 2
labelValue LabelSet = 3
labelValue LabelSetElement = 4
labelValue LabelMap = 5
labelValue LabelMapEntry = 6
labelValue LabelTuple = 7
labelValue LabelOneOf = 8
labelValue LabelOptional = 9
labelValue LabelFixedDict = 10
labelValue LabelFlatMap = 11
labelValue LabelFilter = 12
labelValue LabelMapped = 13
labelValue LabelSampledFrom = 14
labelValue LabelEnumVariant = 15

-- * Free-function shims

-- Call sites in 'Hegel.Gen.Internal', 'Hegel.Collection', and the collection
-- generators use these as ordinary functions — e.g. @generate tc schema@.
-- Each shim simply delegates to the vtable field, so none of those modules
-- need to change.

-- | Shim for 'TestCase.generate'.
generate :: TestCase -> Value -> IO Value
generate tc = tc.generate

-- | Shim for 'TestCase.newCollection'.
newCollection :: TestCase -> Int -> Maybe Int -> IO Int
newCollection tc = tc.newCollection

-- | Shim for 'TestCase.collectionMore'.
collectionMore :: TestCase -> Int -> IO Bool
collectionMore tc = tc.collectionMore

-- | Shim for 'TestCase.collectionReject'.
collectionReject :: TestCase -> Int -> Maybe Text -> IO ()
collectionReject tc = tc.collectionReject

-- | Shim for 'TestCase.startSpan'.
startSpan :: TestCase -> Label -> IO ()
startSpan tc = tc.startSpan

-- | Shim for 'TestCase.stopSpan'.
stopSpan :: TestCase -> Bool -> IO ()
stopSpan tc = tc.stopSpan

-- | Shim for 'TestCase.markComplete'.
markComplete :: TestCase -> Status -> IO ()
markComplete tc = tc.markComplete
