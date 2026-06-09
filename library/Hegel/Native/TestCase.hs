-- | Native-backend 'TestCase' constructor.
--
-- 'mkTestCase' wraps a run-owned handle.
module Hegel.Native.TestCase
  ( mkTestCase,
    mkReplayTestCase,
  )
where

import CBOR.Decode qualified as CD
import CBOR.Encode qualified as CE
import CBOR.Value (Value)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Foreign (Ptr, alloca, nullPtr, peek)
import Foreign.C.String (withCString)
import Foreign.C.Types (CBool (..), CInt)
import Hegel.Gen.Internal (AssumeRejected (..))
import Hegel.Native.FFI
import Hegel.TestCase (Label, Status (..), TestCase (..), TestStopped (..), labelValue)
import UnliftIO.Exception (catch, throwIO)

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
-- 'Hegel.Native.FFI.withTestCaseFromBlob'.
--
-- Its 'markComplete' is a no-op: calling @hegel_mark_complete@ on a standalone
-- (from-blob) test case aborts the process via a Rust panic, and replay only
-- needs to redraw the counterexample value. Note that 'draw' may still invoke
-- 'markComplete' internally (e.g. an exhausted 'Hegel.Gen.filtered'), so the
-- no-op is load-bearing, not merely defensive.
mkReplayTestCase :: Ptr HegelTestCase -> TestCase
mkReplayTestCase tc = (mkTestCase tc) {markComplete = \_ -> pure ()}

nativeGenerate :: Ptr HegelTestCase -> Value -> IO Value
nativeGenerate tc schema = do
  resultBytes <-
    generate tc (CE.encode schema)
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
-- those to the exceptions the generator layer expects — as 'nativeGenerate'
-- and the server backend do. Everything else falls through to 'throwOnError'.
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
-- 'Hegel.TestCase.labelValue' (1–15, matching @HEGEL_LABEL_*@); reuse it rather
-- than maintaining a parallel @Label -> Word64@ mapping that could drift.
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
