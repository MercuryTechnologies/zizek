-- | The generator-facing engine channel: the per-test-case operations a
-- generator draws from.
--
-- __Internal module.__ Implementation substrate of @zizek@ itself, exposed so
-- you can reach past the public API when you must; it is not part of the
-- stable public interface and may change without notice.
--
-- These are plain functions over a 'TestCase' (libhegel is the only engine, so
-- there is no @DataSource@ typeclass to implement). This is also the home for
-- non-schema engine primitives: 'primitiveBoolean', pools, and state machines.
module Hegel.Internal.DataSource
  ( -- * Generation
    generate,
    generateEncoded,
    primitiveBoolean,

    -- * Collections
    newCollection,
    collectionMore,
    collectionReject,

    -- * Pools
    newPool,
    poolAdd,
    poolGenerate,

    -- * State machines
    newStateMachine,
    stateMachineNextRule,

    -- * Spans
    Label (..),
    startSpan,
    stopSpan,
  )
where

import CBOR.Decode qualified as CD
import CBOR.Encode qualified as CE
import CBOR.Value (Value)
import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Word (Word64)
import Foreign (alloca, nullPtr, peek, withArray, withMany)
import Foreign.C.Types (CBool (..), CDouble (..), CInt)
import Hegel.Internal.CString qualified as CString
import Hegel.Internal.Control (AssumeRejected (..), TestStopped (..))
import Hegel.Internal.FFI hiding (generate)
import Hegel.Internal.FFI qualified as FFI (generate)
import Hegel.Internal.TestCase (TestCase (..))
import UnliftIO.Exception (catch)
import Witch qualified

-- * Generation

-- NOTE: This function _needs_ to use 'Control.Exception.throwIO' so that
-- 'TestStopped' & 'AssumeRejected' can be thrown as proper async exceptions.

-- | Ask the engine for a value matching the CBOR schema.
--
-- Throws 'TestStopped' when the choice budget is exhausted, or 'AssumeRejected'
-- when the engine signals that the current case should be discarded.
generate :: TestCase -> Value -> IO Value
generate tc schema = generateEncoded tc (CE.encode schema)

-- | 'generate' with the schema already CBOR-encoded — the hot path for
-- 'Hegel.Gen.Internal.BasicGenerator' draws, which cache their encoding at
-- construction so repeated draws skip the encode entirely.
generateEncoded :: TestCase -> ByteString -> IO Value
generateEncoded tc schemaBytes = do
  resultBytes <-
    FFI.generate tc.ctx tc.ptr tc.slot schemaBytes
      `catch` \e@(HegelError {code}) -> case code of
        HEGEL_E_STOP_TEST -> throwIO TestStopped
        HEGEL_E_ASSUME -> throwIO AssumeRejected
        _ -> throwIO e
  case CD.decode resultBytes of
    Left err -> ioError (userError ("libhegel: CBOR decode failed: " <> err))
    Right v -> pure v

-- | Interpret a return code from a per-test-case operation.
--
-- The engine may signal 'HEGEL_E_STOP_TEST' or 'HEGEL_E_ASSUME' as ordinary
-- control flow rather than an error, so map those to the exceptions the
-- generator layer expects.
--
-- Everything else falls through to 'throwOnError'.
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
    -- has_forced = 0: forced-draw support is unused (see 'hegel_primitive_boolean').
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
    Nothing -> do
      result <- hegel_collection_reject tc.ctx tc.ptr (fromIntegral cid) nullPtr
      handleReturnCode tc result
    Just why -> CString.withText why \p -> do
      result <- hegel_collection_reject tc.ctx tc.ptr (fromIntegral cid) p
      handleReturnCode tc result

-- * Pools

-- | Create a new variable pool; returns its ID.
--
-- Throws 'TestStopped' on exhaustion.
newPool :: TestCase -> IO Int
newPool tc =
  alloca \outId -> do
    hegel_new_pool tc.ctx tc.ptr outId >>= handleReturnCode tc
    fromIntegral <$> (peek outId :: IO Int64)

-- | Register a new variable in the pool; returns the engine-assigned
-- variable id.
poolAdd :: TestCase -> Int -> IO Int
poolAdd tc pid =
  alloca \outId -> do
    hegel_pool_add tc.ctx tc.ptr (fromIntegral pid) outId >>= handleReturnCode tc
    fromIntegral <$> (peek outId :: IO Int64)

-- | Draw a variable id from the pool.
--
-- Pass 'True' to consume the variable (remove it from the pool).
--
-- Throws 'AssumeRejected' when the pool is empty, discarding the test case.
poolGenerate :: TestCase -> Int -> Bool -> IO Int
poolGenerate tc pid consume =
  alloca \outId -> do
    hegel_pool_generate tc.ctx tc.ptr (fromIntegral pid) (CBool (if consume then 1 else 0)) outId
      >>= handleReturnCode tc
    fromIntegral <$> (peek outId :: IO Int64)

-- * State machines

-- | Register an engine-owned state machine; returns its ID.
--
-- @ruleNames@ must be non-empty.
--
-- Throws 'TestStopped' on exhaustion.
newStateMachine :: TestCase -> [Text] -> [Text] -> IO Int
newStateMachine tc ruleNames invariantNames =
  withMany CString.withText ruleNames \rulePtrs ->
    withMany CString.withText invariantNames \invPtrs ->
      withArray rulePtrs \rulesArr ->
        withArray invPtrs \invArr ->
          alloca \outId -> do
            hegel_new_state_machine
              tc.ctx
              tc.ptr
              rulesArr
              (fromIntegral (length ruleNames))
              invArr
              (fromIntegral (length invariantNames))
              outId
              >>= handleReturnCode tc
            fromIntegral <$> (peek outId :: IO Int64)

-- | Draw the next rule index for the state machine.
--
-- Throws 'TestStopped' when the choice budget is exhausted.
stateMachineNextRule :: TestCase -> Int -> IO Int
stateMachineNextRule tc mid =
  alloca \outIdx -> do
    hegel_state_machine_next_rule tc.ctx tc.ptr (fromIntegral mid) outIdx
      >>= handleReturnCode tc
    fromIntegral <$> (peek outIdx :: IO Int64)

-- * Spans

-- | Open a labeled span.
startSpan :: TestCase -> Label -> IO ()
startSpan tc label = do
  result <- hegel_start_span tc.ctx tc.ptr (Witch.into @Word64 label)
  throwOnError tc.ctx result

-- | Close the most-recently-opened span.
-- Pass 'True' to mark it discarded.
stopSpan :: TestCase -> Bool -> IO ()
stopSpan tc isDiscard = do
  result <- hegel_stop_span tc.ctx tc.ptr (CBool (if isDiscard then 1 else 0))
  throwOnError tc.ctx result

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
  | LabelFeatureFlag
  deriving stock (Show)

-- | The @hegel_label_t@ wire identifier (the @HEGEL_LABEL_*@ constants are the
-- single source of truth).
instance Witch.From Label Word64 where
  from LabelList = HEGEL_LABEL_LIST
  from LabelListElement = HEGEL_LABEL_LIST_ELEMENT
  from LabelSet = HEGEL_LABEL_SET
  from LabelSetElement = HEGEL_LABEL_SET_ELEMENT
  from LabelMap = HEGEL_LABEL_MAP
  from LabelMapEntry = HEGEL_LABEL_MAP_ENTRY
  from LabelTuple = HEGEL_LABEL_TUPLE
  from LabelOneOf = HEGEL_LABEL_ONE_OF
  from LabelOptional = HEGEL_LABEL_OPTIONAL
  from LabelFixedDict = HEGEL_LABEL_FIXED_DICT
  from LabelFlatMap = HEGEL_LABEL_FLAT_MAP
  from LabelFilter = HEGEL_LABEL_FILTER
  from LabelMapped = HEGEL_LABEL_MAPPED
  from LabelSampledFrom = HEGEL_LABEL_SAMPLED_FROM
  from LabelEnumVariant = HEGEL_LABEL_ENUM_VARIANT
  from LabelFeatureFlag = HEGEL_LABEL_FEATURE_FLAG
