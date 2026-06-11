-- | Per-test-case handle and native operations.
--
-- Defines 'TestCase' — a thin wrapper around a @hegel_test_case_t*@ pointer —
-- and the operations through which generators communicate with the native
-- @libhegel@ backend.
module Hegel.TestCase
  ( -- * Construction
    mkTestCase,

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

-- | Wrap a run-owned @hegel_test_case_t*@ pointer.
--
-- The pointer is borrowed from the run handle and remains valid only for the
-- duration of the current test case (until 'markComplete' is called and the
-- runner fetches the next case via 'hegel_next_test_case').
mkTestCase :: Ptr HegelTestCase -> TestCase
mkTestCase = TestCase

-- * Test case

-- | A thin wrapper around a @hegel_test_case_t*@ pointer. Generators and
-- collections call the free functions below, passing 'TestCase' as the first
-- argument, rather than touching the pointer directly.
newtype TestCase = TestCase {ptr :: Ptr HegelTestCase}

-- * Generation

-- | Ask the engine for a value matching the CBOR schema.
--
-- Throws 'TestStopped' when the choice budget is exhausted, or 'AssumeRejected'
-- when the engine signals that the current case should be discarded.
generate :: TestCase -> Value -> IO Value
generate tc schema = do
  resultBytes <-
    FFI.generate tc.ptr (CE.encode schema)
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

-- * Collections

-- | Begin a variable-length collection; returns its integer ID.
--
-- Throws 'TestStopped' on exhaustion.
newCollection :: TestCase -> Int -> Maybe Int -> IO Int
newCollection tc minSz maxSz =
  alloca \outId -> do
    hegel_new_collection tc.ptr (fromIntegral minSz) (maybe maxBound fromIntegral maxSz) outId
      >>= handleReturnCode
    fromIntegral <$> (peek outId :: IO Int64)

-- | Ask whether the engine wants another element.
--
-- Throws 'TestStopped' on exhaustion.
collectionMore :: TestCase -> Int -> IO Bool
collectionMore tc cid =
  alloca \outMore -> do
    hegel_collection_more tc.ptr (fromIntegral cid) outMore >>= handleReturnCode
    (/= 0) . (\(CBool b) -> b) <$> peek outMore

-- | Notify the engine that the last element was rejected.
--
-- Throws 'TestStopped' if the engine gives up.
collectionReject :: TestCase -> Int -> Maybe Text -> IO ()
collectionReject tc cid mWhy =
  case mWhy of
    Nothing -> hegel_collection_reject tc.ptr (fromIntegral cid) nullPtr >>= handleReturnCode
    Just why -> withCString (T.unpack why) \p ->
      hegel_collection_reject tc.ptr (fromIntegral cid) p >>= handleReturnCode

-- * Spans

-- | Open a labeled span.
startSpan :: TestCase -> Label -> IO ()
-- The span label's wire identifier is the single source of truth in
-- 'labelValue' (1–15, matching @HEGEL_LABEL_*@); reuse it rather than
-- maintaining a parallel @Label -> Word64@ mapping that could drift.
startSpan tc label = hegel_start_span tc.ptr (fromIntegral (labelValue label)) >>= throwOnError

-- | Close the most-recently-opened span.
-- Pass 'True' to mark it discarded.
stopSpan :: TestCase -> Bool -> IO ()
stopSpan tc isDiscard =
  hegel_stop_span tc.ptr (CBool (if isDiscard then 1 else 0)) >>= throwOnError

-- * Completion

-- | Report the final outcome for this test case.
--
-- Swallows 'HEGEL_E_STOP_TEST' for all statuses — the engine may return it as
-- a normal "continue" signal at any point during the run (not only after
-- INTERESTING).
--
-- Only called from the live run path ('Hegel.Runner.runTestCase').
--
-- The replay path ('Hegel.Runner.reconstructProperty') only draws and journals;
-- it never marks completion, so from-blob handles are safe to pass through
-- 'mkTestCase'.
markComplete :: TestCase -> Status -> IO ()
markComplete tc status = do
  rc <- case status of
    Valid -> hegel_mark_complete tc.ptr HEGEL_STATUS_VALID nullPtr
    Invalid -> hegel_mark_complete tc.ptr HEGEL_STATUS_INVALID nullPtr
    Overrun -> hegel_mark_complete tc.ptr HEGEL_STATUS_OVERRUN nullPtr
    Interesting origin ->
      withCString (T.unpack origin) \p ->
        hegel_mark_complete tc.ptr HEGEL_STATUS_INTERESTING p
  case rc of
    HEGEL_OK -> pure ()
    HEGEL_E_STOP_TEST -> pure ()
    _ -> throwOnError rc

-- * Exceptions

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

-- * Status

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

-- * Spans

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
