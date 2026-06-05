-- | Native-backend test suite.
--
-- Phase 3: verifies the raw FFI binding end-to-end using the boolean
-- schema — no 'Hegel.Gen', no 'Hegel.TestCase', no transport.
-- Later phases add further test cases alongside 'boolRoundTrip'.
module Main (main) where

import CBOR.Class (ToCBOR (toCBOR))
import CBOR.Decode qualified as CD
import CBOR.Encode qualified as CE
import CBOR.Value (Value (..))
import Control.Concurrent (runInBoundThread)
import Control.Exception (throwIO, try)
import Data.ByteString (ByteString)
import Data.Word (Word64)
import Foreign (Ptr, nullPtr)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt)
import Hegel.Native.FFI
import Hegel.Schema qualified as Schema
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

main :: IO ()
main =
  defaultMain $
    testGroup
      "zizek:native"
      [ testCase "boolean schema round-trip" boolRoundTrip,
        testCase "failure reproduction round-trip" reproductionRoundTrip
      ]

-- | Drive 50 boolean test cases through the raw C API and assert that every
-- returned CBOR value decodes to a 'Bool' and the run passes overall.
--
-- The entire sequence runs in a bound thread so that 'checkReturn' always reads
-- 'hegel_last_error_message' on the OS thread that made the failing call.
boolRoundTrip :: IO ()
boolRoundTrip = runInBoundThread $ do
  let schemaBytes :: ByteString
      schemaBytes = CE.encode (toCBOR Schema.bool)
  withSettings $ \s -> do
    hegel_settings_test_cases s 50
    hegel_settings_verbosity s HEGEL_VERBOSITY_QUIET
    -- Disable the on-disk database so the test does not create .hegel/ dirs.
    withCString "" (hegel_settings_database s)
    withRun s $ \run -> do
      driveRun schemaBytes run
      resultPtr <- hegel_run_result run
      passed <- hegel_run_result_passed resultPtr
      assertBool "all 50 boolean cases should pass" (passed /= 0)

-- | Loop over every test case the engine produces, draw one boolean each
-- time, assert the CBOR decodes correctly, and mark the case valid.
driveRun :: ByteString -> Ptr HegelRun -> IO ()
driveRun schemaBytes run = go
  where
    go :: IO ()
    go = do
      tc <- hegel_next_test_case run
      if tc == nullPtr
        then pure () -- run finished
        else do
          bs <- generate tc schemaBytes
          case CD.decode bs of
            Left err ->
              assertFailure ("CBOR decode failed: " <> err)
            Right (Bool _) ->
              pure ()
            Right v ->
              assertFailure ("expected Bool, got: " <> show v)
          hegel_mark_complete tc HEGEL_STATUS_VALID nullPtr >>= checkReturn
          go

-- | Verify the failure-reproduction round-trip:
--
-- 1. Run a deliberately-failing property (integers >= threshold are
--    \"interesting\") to produce a shrunk failure blob.
-- 2. Extract the blob from the failure via 'failureReproductionBlob'.
-- 3. Replay the blob via 'withTestCaseFromBlob' and confirm:
--    - 'hegel_test_case_is_final_replay' is @true@ on the replayed handle.
--    - The replayed draw reproduces the original failure condition.
--
-- A fixed seed and disabled database keep the run deterministic.
reproductionRoundTrip :: IO ()
reproductionRoundTrip = runInBoundThread $ do
  -- Integer schema: draw values from [0, 255].
  let schemaBytes :: ByteString
      schemaBytes = CE.encode (toCBOR (Schema.integer @Word64 0 255))
      -- Values >= threshold are "interesting" (i.e. the property fails).
      threshold :: Word64
      threshold = 10
      origin :: String
      origin = "tests/native/Main.hs:reproductionRoundTrip"

  -- Phase 1: discover a failure and collect its blob.
  blob <- withSettings $ \s -> do
    hegel_settings_test_cases s 200
    hegel_settings_verbosity s HEGEL_VERBOSITY_QUIET
    -- Fix seed for determinism across runs.
    hegel_settings_seed s 42 1
    withCString "" (hegel_settings_database s)
    withRun s $ \run -> do
      driveRunForFailure schemaBytes threshold origin run
      resultPtr <- hegel_run_result run
      passed <- hegel_run_result_passed resultPtr
      assertBool "run should have failed (found an integer >= threshold)" (passed == 0)
      count <- hegel_run_result_failure_count resultPtr
      assertBool "run should record at least one failure" (count > 0)
      failurePtr <- hegel_run_result_failure resultPtr 0
      assertBool "failure pointer must be non-null" (failurePtr /= nullPtr)
      mb <- failureReproductionBlob failurePtr
      case mb of
        Nothing -> assertFailure "failure should carry a reproduction blob" >> pure mempty
        Just b -> pure b

  -- Phase 2: replay the blob and confirm reproduction.
  withSettings $ \s -> do
    hegel_settings_verbosity s HEGEL_VERBOSITY_QUIET
    withCString "" (hegel_settings_database s)
    withTestCaseFromBlob s blob $ \tc -> do
      isFinal <- hegel_test_case_is_final_replay tc
      assertBool "replayed test case should report is_final_replay = true" (isFinal /= 0)
      bs <- generate tc schemaBytes
      case CD.decode bs of
        Left err ->
          assertFailure ("CBOR decode failed during replay: " <> err)
        Right (UInt v) ->
          -- Standalone test cases (from_blob) have no run to report back to;
          -- the caller simply inspects the drawn value to confirm reproduction.
          -- Do NOT call hegel_mark_complete here — there is no ack channel.
          assertBool
            ( "replayed value should reproduce the failure: expected >= "
                <> show threshold
                <> ", got "
                <> show v
            )
            (v >= fromIntegral threshold)
        Right v ->
          assertFailure ("expected UInt during replay, got: " <> show v)

-- | Attempt to draw one value from a test case, returning 'Nothing' when the
-- choice budget is exhausted ('HEGEL_E_STOP_TEST').
--
-- __NOTE__: This is a run-loop-level workaround. The 'generate' helper throws
-- 'HegelError' on 'HEGEL_E_STOP_TEST' by design (it is a within-test-case
-- building block, not a run-loop primitive). Budget exhaustion genuinely occurs
-- during shrinking — the engine tries candidates with shorter choice sequences
-- than the original failure — so the run loop must handle it here by marking
-- 'HEGEL_STATUS_OVERRUN' and continuing. Remove this once a higher-level
-- abstraction over the full test-case lifecycle exists.
tryDraw :: Ptr HegelTestCase -> ByteString -> IO (Maybe ByteString)
tryDraw tc schema = do
  result <- try @HegelError (generate tc schema)
  case result of
    Right bs -> pure (Just bs)
    Left HegelError {code = HEGEL_E_STOP_TEST} -> pure Nothing
    Left err -> throwIO err

-- | Mark a test case complete, treating 'HEGEL_E_STOP_TEST' as a normal
-- control-flow code rather than an error.
--
-- When marking 'HEGEL_STATUS_INTERESTING', the engine returns
-- 'HEGEL_E_STOP_TEST' as a signal that it has received the failure and the
-- caller should continue the run loop; it is not an error condition.
markComplete :: Ptr HegelTestCase -> CInt -> CString -> IO ()
markComplete tc status origin = do
  rc <- hegel_mark_complete tc status origin
  case rc of
    HEGEL_OK -> pure ()
    HEGEL_E_STOP_TEST -> pure ()
    _ -> checkReturn rc

-- | Drive a run to failure: mark each drawn integer as 'HEGEL_STATUS_INTERESTING'
-- when it is >= @threshold@, and 'HEGEL_STATUS_VALID' otherwise.
driveRunForFailure :: ByteString -> Word64 -> String -> Ptr HegelRun -> IO ()
driveRunForFailure schemaBytes threshold origin run = go
  where
    go :: IO ()
    go = do
      tc <- hegel_next_test_case run
      if tc == nullPtr
        then pure ()
        else do
          mbs <- tryDraw tc schemaBytes
          case mbs of
            Nothing ->
              markComplete tc HEGEL_STATUS_OVERRUN nullPtr
            Just bs ->
              case CD.decode bs of
                Left err ->
                  assertFailure ("CBOR decode failed: " <> err)
                Right (UInt v) ->
                  if v >= fromIntegral threshold
                    then withCString origin $ \originPtr ->
                      markComplete tc HEGEL_STATUS_INTERESTING originPtr
                    else markComplete tc HEGEL_STATUS_VALID nullPtr
                Right v ->
                  assertFailure ("expected UInt, got: " <> show v)
          go
