-- | The generator-facing engine channel: the per-test-case operations a
-- generator draws from.
--
-- __Internal module.__ Implementation substrate of @zizek@ itself, exposed so
-- you can reach past the public API when you must; it is not part of the
-- stable public interface and may change without notice.
--
-- These are plain functions over a 'TestCase' (libhegel is the only engine, so
-- there is no @DataSource@ typeclass to implement). This is also the home for
-- non-schema engine primitives — 'primitiveBoolean' today, and pools /
-- state-machine / targeting as stateful testing lands.
module Hegel.Internal.DataSource
  ( -- * Generation
    generate,
    primitiveBoolean,

    -- * Collections
    newCollection,
    collectionMore,
    collectionReject,

    -- * Spans
    Label (..),
    labelValue,
    startSpan,
    stopSpan,
  )
where

import CBOR.Decode qualified as CD
import CBOR.Encode qualified as CE
import CBOR.Value (Value)
import Control.Exception (throwIO)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Foreign (alloca, nullPtr, peek)
import Foreign.C.String (withCString)
import Foreign.C.Types (CBool (..), CDouble (..), CInt)
import Hegel.Internal.Control (AssumeRejected (..), TestStopped (..))
import Hegel.Internal.FFI hiding (generate)
import Hegel.Internal.FFI qualified as FFI (generate)
import Hegel.Internal.TestCase (TestCase (..))
import UnliftIO.Exception (catch)

-- * Generation

-- NOTE: This function _needs_ to use 'Control.Exception.throwIO' so that
-- 'TestStopped' & 'AssumeRejected' can be thrown as proper async exceptions.

-- | Ask the engine for a value matching the CBOR schema.
--
-- Throws 'TestStopped' when the choice budget is exhausted, or 'AssumeRejected'
-- when the engine signals that the current case should be discarded.
generate :: TestCase -> Value -> IO Value
generate tc schema = do
  resultBytes <-
    FFI.generate tc.ctx tc.ptr (CE.encode schema)
      `catch` \e@(HegelError {code}) -> case code of
        HEGEL_E_STOP_TEST -> throwIO TestStopped
        HEGEL_E_ASSUME -> throwIO AssumeRejected
        _ -> throwIO e
  case CD.decode resultBytes of
    Left err -> ioError (userError ("libhegel: CBOR decode failed: " <> err))
    Right v -> pure v

-- | Interpret a return code from a per-test-case operation. The engine may
-- signal 'HEGEL_E_STOP_TEST' (choice budget exhausted) or 'HEGEL_E_ASSUME'
-- (assumption rejected) as ordinary control flow rather than an error, so map
-- those to the exceptions the generator layer expects. Everything else falls
-- through to 'throwOnError'.
handleReturnCode :: TestCase -> CInt -> IO ()
handleReturnCode _ HEGEL_E_STOP_TEST = throwIO TestStopped
handleReturnCode _ HEGEL_E_ASSUME = throwIO AssumeRejected
handleReturnCode tc rc = throwOnError tc.ctx rc

-- | Draw a single boolean that is 'True' with probability @p@ (clamped to
-- @[0,1]@ by the engine).
--
-- Throws 'TestStopped' on exhaustion.
primitiveBoolean :: TestCase -> Double -> IO Bool
primitiveBoolean tc p =
  alloca \outValue -> do
    -- The engine's forced-draw support (forced / has_forced) is unused here;
    -- pass has_forced = 0 so the draw always consults the data stream.
    hegel_primitive_boolean tc.ctx tc.ptr (CDouble p) (CBool 0) (CBool 0) outValue
      >>= handleReturnCode tc
    (/= 0) . (\(CBool b) -> b) <$> peek outValue

-- * Collections

-- | Begin a variable-length collection; returns its integer ID.
--
-- Throws 'TestStopped' on exhaustion.
newCollection :: TestCase -> Int -> Maybe Int -> IO Int
newCollection tc minSz maxSz =
  alloca \outId -> do
    hegel_new_collection tc.ctx tc.ptr (fromIntegral minSz) (maybe maxBound fromIntegral maxSz) outId
      >>= handleReturnCode tc
    fromIntegral <$> (peek outId :: IO Int64)

-- | Ask whether the engine wants another element.
--
-- Throws 'TestStopped' on exhaustion.
collectionMore :: TestCase -> Int -> IO Bool
collectionMore tc cid =
  alloca \outMore -> do
    hegel_collection_more tc.ctx tc.ptr (fromIntegral cid) outMore >>= handleReturnCode tc
    (/= 0) . (\(CBool b) -> b) <$> peek outMore

-- | Notify the engine that the last element was rejected.
--
-- Throws 'TestStopped' if the engine gives up.
collectionReject :: TestCase -> Int -> Maybe Text -> IO ()
collectionReject tc cid mWhy =
  case mWhy of
    Nothing -> hegel_collection_reject tc.ctx tc.ptr (fromIntegral cid) nullPtr >>= handleReturnCode tc
    Just why -> withCString (T.unpack why) \p ->
      hegel_collection_reject tc.ctx tc.ptr (fromIntegral cid) p >>= handleReturnCode tc

-- * Spans

-- | Open a labeled span.
startSpan :: TestCase -> Label -> IO ()
-- The span label's wire identifier is the single source of truth in
-- 'labelValue' (1–15, matching @HEGEL_LABEL_*@); reuse it rather than
-- maintaining a parallel @Label -> Word64@ mapping that could drift.
startSpan tc label = hegel_start_span tc.ctx tc.ptr (fromIntegral (labelValue label)) >>= throwOnError tc.ctx

-- | Close the most-recently-opened span.
-- Pass 'True' to mark it discarded.
stopSpan :: TestCase -> Bool -> IO ()
stopSpan tc isDiscard =
  hegel_stop_span tc.ctx tc.ptr (CBool (if isDiscard then 1 else 0)) >>= throwOnError tc.ctx

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
