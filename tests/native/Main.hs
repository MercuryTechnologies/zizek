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
import Data.Function ((&))
import Data.Word (Word64)
import Foreign (Ptr, nullPtr)
import Foreign.C.String (withCString)
import Hegel.Gen qualified as Gen
import Hegel.Gen.Internal (Gen, draw)
import Hegel.Native.FFI
import Hegel.Native.TestCase (mkTestCase)
import Hegel.Schema qualified as Schema
import Hegel.TestCase (Status (..), TestStopped (..))
import Hegel.TestCase qualified as TC
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

main :: IO ()
main =
  defaultMain $
    testGroup
      "zizek:native"
      [ testCase "boolean schema round-trip" boolRoundTrip,
        testCase "mark interesting smoke test" markInterestingSmoke,
        testCase "integer shrink smoke test" integerShrinkSmoke,
        testCase "draw via Gen machinery smoke test" drawGenSmoke,
        testCase "draw+fail+shrink via Gen smoke test" drawFailShrinkSmoke
      ]

-- | Drive 50 boolean test cases through the raw C API and assert that every
-- returned CBOR value decodes to a 'Bool' and the run passes overall.
--
-- The entire sequence runs in a bound thread so that 'throwOnError' always reads
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

-- | Smoke test: run 5 boolean cases, mark the first INTERESTING to verify
-- hegel_mark_complete with INTERESTING doesn't crash.
markInterestingSmoke :: IO ()
markInterestingSmoke = runInBoundThread $ do
  withSettings $ \s -> do
    hegel_settings_test_cases s 5
    hegel_settings_verbosity s HEGEL_VERBOSITY_QUIET
    withCString "" (hegel_settings_database s)
    withRun s $ \run -> go run True
    pure ()
  where
    go :: Ptr HegelRun -> Bool -> IO ()
    go run markFirst = do
      tc <- hegel_next_test_case run
      if tc == nullPtr
        then pure ()
        else do
          _ <- generate tc (CE.encode (toCBOR Schema.bool))
          if markFirst
            then do
              rc <- withCString "smoke:0" $ \p ->
                hegel_mark_complete tc HEGEL_STATUS_INTERESTING p
              case rc of
                HEGEL_E_STOP_TEST -> pure () -- normal continue signal
                _ -> throwOnError rc
              go run False
            else do
              hegel_mark_complete tc HEGEL_STATUS_VALID nullPtr >>= throwOnError
              go run False

-- | Drive an integer property that fails for n >= 10, with full shrinking.
-- Verifies the entire failure+shrink cycle at the raw FFI level.
integerShrinkSmoke :: IO ()
integerShrinkSmoke = runInBoundThread $ do
  let schemaBytes = CE.encode (toCBOR (Schema.integer @Word64 0 255))
      threshold = 10 :: Word64
  withSettings $ \s -> do
    hegel_settings_test_cases s 50
    hegel_settings_verbosity s HEGEL_VERBOSITY_QUIET
    withCString "" (hegel_settings_database s)
    withRun s $ \run -> do
      shrinkLoop schemaBytes threshold run
      resultPtr <- hegel_run_result run
      passed <- hegel_run_result_passed resultPtr
      assertBool "run should have failed" (passed == 0)
  where
    shrinkLoop :: ByteString -> Word64 -> Ptr HegelRun -> IO ()
    shrinkLoop schemaBytes threshold run = do
      tc <- hegel_next_test_case run
      if tc == nullPtr
        then pure ()
        else do
          mbs <- tryDraw tc schemaBytes
          case mbs of
            Nothing -> do
              rc <- hegel_mark_complete tc HEGEL_STATUS_OVERRUN nullPtr
              case rc of HEGEL_OK -> pure (); HEGEL_E_STOP_TEST -> pure (); _ -> throwOnError rc
            Just bs -> case CD.decode bs of
              Right (UInt v)
                | v >= fromIntegral threshold ->
                    withCString "smoke:0" $ \p -> do
                      rc <- hegel_mark_complete tc HEGEL_STATUS_INTERESTING p
                      case rc of HEGEL_OK -> pure (); HEGEL_E_STOP_TEST -> pure (); _ -> throwOnError rc
              _ -> hegel_mark_complete tc HEGEL_STATUS_VALID nullPtr >>= throwOnError
          shrinkLoop schemaBytes threshold run

-- | Verify that 'draw' via the Haskell Gen machinery works with the native backend.
drawGenSmoke :: IO ()
drawGenSmoke = runInBoundThread $ do
  let gen = Gen.integral @Int & Gen.min 0 & Gen.max 100 & Gen.build
  withSettings $ \s -> do
    hegel_settings_test_cases s 10
    hegel_settings_verbosity s HEGEL_VERBOSITY_QUIET
    withCString "" (hegel_settings_database s)
    withRun s $ \run -> loop gen run
  where
    loop :: Gen Int -> Ptr HegelRun -> IO ()
    loop gen run = do
      tcPtr <- hegel_next_test_case run
      if tcPtr == nullPtr
        then pure ()
        else do
          let tc = mkTestCase tcPtr
          n <- draw tc gen
          assertBool ("expected 0-100, got " <> show n) (n >= 0 && n <= 100)
          TC.markComplete tc Valid
          loop gen run

-- | Full failing+shrinking cycle driven via 'draw' and body failure.
drawFailShrinkSmoke :: IO ()
drawFailShrinkSmoke = runInBoundThread $ do
  let gen = Gen.integral @Int & Gen.min 0 & Gen.max 100 & Gen.build
  withSettings $ \s -> do
    hegel_settings_test_cases s 100
    hegel_settings_verbosity s HEGEL_VERBOSITY_QUIET
    withCString "" (hegel_settings_database s)
    withRun s $ \run -> do
      loop gen run
      resultPtr <- hegel_run_result run
      passed <- hegel_run_result_passed resultPtr
      assertBool "run should have failed (found 42)" (passed == 0)
  where
    loop gen run = do
      tcPtr <- hegel_next_test_case run
      if tcPtr == nullPtr
        then pure ()
        else do
          let tc = mkTestCase tcPtr
          eVal <- try @TestStopped (draw tc gen)
          case eVal of
            Left TestStopped -> do
              -- Budget exhausted for this shrink probe; mark overrun.
              TC.markComplete tc Overrun
            Right n ->
              if n == (42 :: Int)
                then withCString "smoke:0" $ \p -> do
                  rc <- hegel_mark_complete tcPtr HEGEL_STATUS_INTERESTING p
                  case rc of HEGEL_OK -> pure (); HEGEL_E_STOP_TEST -> pure (); _ -> throwOnError rc
                else TC.markComplete tc Valid
          loop gen run

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
          hegel_mark_complete tc HEGEL_STATUS_VALID nullPtr >>= throwOnError
          go

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
