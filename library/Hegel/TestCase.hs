-- | Per-test-case vtable and native constructor.
--
-- Defines the 'TestCase' record through which generators communicate with the
-- native @libhegel@ backend, plus the 'mkTestCase' and 'mkReplayTestCase'
-- constructors that wrap a C test-case pointer in that record.
module Hegel.TestCase
  ( -- * Construction
    mkTestCase,
    mkReplayTestCase,

    -- * Test case
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

    -- * Exceptions
    AssumeRejected (..),
  )
where

import CBOR.Decode qualified as CD
import CBOR.Encode qualified as CE
import CBOR.Value (Value)
import Control.Exception (Exception)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Foreign (Ptr, alloca, nullPtr, peek)
import Foreign.C.String (withCString)
import Foreign.C.Types (CBool (..), CInt)
import Hegel.FFI hiding (generate)
import Hegel.FFI qualified as FFI (generate)
import UnliftIO.Exception (catch, throwIO)

-- * Construction

-- | Wrap a run-owned @hegel_test_case_t*@ pointer in the 'TestCase' vtable.
--
-- The pointer is borrowed from the run handle and remains valid only for the
-- duration of the current test case (until 'markComplete' is called and the
-- runner fetches the next case via 'hegel_next_test_case').
mkTestCase :: Ptr HegelTestCase -> TestCase
mkTestCase tc =
  TestCase
    { generate = nativeGenerate tc,
      newCollection = nativeNewCollection tc,
      collectionMore = nativeCollectionMore tc,
      collectionReject = nativeCollectionReject tc,
      startSpan = nativeStartSpan tc,
      stopSpan = nativeStopSpan tc,
      markComplete = nativeMarkComplete tc
    }

-- | Like 'mkTestCase', but for a caller-owned replay handle obtained from
-- 'Hegel.FFI.withTestCaseFromBlob'.
--
-- Its 'markComplete' is a no-op: @hegel_mark_complete@ panics on a standalone
-- (from-blob) handle, and 'draw' may invoke 'markComplete' internally (e.g. an
-- exhausted 'Hegel.Gen.filtered').
mkReplayTestCase :: Ptr HegelTestCase -> TestCase
mkReplayTestCase tc = (mkTestCase tc) {markComplete = \_ -> pure ()}

nativeGenerate :: Ptr HegelTestCase -> Value -> IO Value
nativeGenerate tc schema = do
  resultBytes <-
    FFI.generate tc (CE.encode schema)
      `catch` \e@(HegelError {code}) -> case code of
        HEGEL_E_STOP_TEST -> throwIO TestStopped
        HEGEL_E_ASSUME -> throwIO AssumeRejected
        _ -> throwIO e
  case CD.decode resultBytes of
    Left err -> ioError (userError ("native backend: CBOR decode failed: " <> err))
    Right v -> pure v

-- | Interpret a return code from a per-test-case operation. The engine may
-- signal 'HEGEL_E_STOP_TEST' (choice budget exhausted) or 'HEGEL_E_ASSUME'
-- (assumption rejected) as ordinary control flow rather than an error, so map
-- those to the exceptions the generator layer expects. Everything else falls
-- through to 'throwOnError'.
handleReturnCode :: CInt -> IO ()
handleReturnCode HEGEL_E_STOP_TEST = throwIO TestStopped
handleReturnCode HEGEL_E_ASSUME = throwIO AssumeRejected
handleReturnCode rc = throwOnError rc

nativeNewCollection :: Ptr HegelTestCase -> Int -> Maybe Int -> IO Int
nativeNewCollection tc minSz maxSz =
  alloca \outId -> do
    hegel_new_collection tc (fromIntegral minSz) (maybe maxBound fromIntegral maxSz) outId
      >>= handleReturnCode
    fromIntegral <$> (peek outId :: IO Int64)

nativeCollectionMore :: Ptr HegelTestCase -> Int -> IO Bool
nativeCollectionMore tc cid =
  alloca \outMore -> do
    hegel_collection_more tc (fromIntegral cid) outMore >>= handleReturnCode
    (/= 0) . (\(CBool b) -> b) <$> peek outMore

nativeCollectionReject :: Ptr HegelTestCase -> Int -> Maybe Text -> IO ()
nativeCollectionReject tc cid mWhy =
  case mWhy of
    Nothing -> hegel_collection_reject tc (fromIntegral cid) nullPtr >>= handleReturnCode
    Just why -> withCString (T.unpack why) \p ->
      hegel_collection_reject tc (fromIntegral cid) p >>= handleReturnCode

nativeStartSpan :: Ptr HegelTestCase -> Label -> IO ()
-- The span label's wire identifier is the single source of truth in
-- 'labelValue' (1–15, matching @HEGEL_LABEL_*@); reuse it rather than
-- maintaining a parallel @Label -> Word64@ mapping that could drift.
nativeStartSpan tc label = hegel_start_span tc (fromIntegral (labelValue label)) >>= throwOnError

nativeStopSpan :: Ptr HegelTestCase -> Bool -> IO ()
nativeStopSpan tc isDiscard =
  hegel_stop_span tc (CBool (if isDiscard then 1 else 0)) >>= throwOnError

-- | Swallow HEGEL_E_STOP_TEST for all statuses — the engine may return it as
-- a normal "continue" signal at any point during the run (not only after
-- INTERESTING).
nativeMarkComplete :: Ptr HegelTestCase -> Status -> IO ()
nativeMarkComplete tc status = do
  rc <- case status of
    Valid -> hegel_mark_complete tc HEGEL_STATUS_VALID nullPtr
    Invalid -> hegel_mark_complete tc HEGEL_STATUS_INVALID nullPtr
    Overrun -> hegel_mark_complete tc HEGEL_STATUS_OVERRUN nullPtr
    Interesting origin ->
      withCString (T.unpack origin) \p ->
        hegel_mark_complete tc HEGEL_STATUS_INTERESTING p
  case rc of
    HEGEL_OK -> pure ()
    HEGEL_E_STOP_TEST -> pure ()
    _ -> throwOnError rc

-- * Vtable

-- | A per-test-case vtable. Each field is a closure over the native
-- @libhegel@ backend. The 'Gen.*' tree and 'Hegel.Collection' call through
-- these fields via the free-function shims below.
--
-- Construct via 'mkTestCase' (run-owned) or 'mkReplayTestCase' (replay).
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

-- | Thrown when a test case is deliberately discarded, either via
-- 'Hegel.Property.assume' or 'Hegel.Property.discard', or by an exhausted
-- 'Hegel.Gen.filtered'\/'Hegel.Gen.mapMaybe' retry budget.
data AssumeRejected = AssumeRejected
  deriving stock (Show)

instance Exception AssumeRejected

-- | Thrown when a generator uses a per-case primitive the active backend does
-- not implement (e.g. pools on a backend whose protocol lacks them).
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
