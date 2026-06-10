-- | Native-backend test suite.
--
-- Exercises the raw @libhegel@ FFI binding end-to-end: driving runs, drawing
-- values (via the boolean schema directly and through the 'Hegel.Gen'
-- machinery), the failure+shrink cycle, and per-case completion semantics.
-- These talk to the FFI directly rather than going through
-- 'Hegel.Native.Runner', so they live here rather than in the
-- backend-parameterized unit suite.
--
-- Every sequence runs in a bound thread so that 'throwOnError' always reads
-- 'hegel_last_error_message' on the OS thread that made the failing call, and
-- disables the on-disk database so tests do not create @.hegel/@ dirs.
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
import Test.Hspec
import Test.Tasty (defaultMain)
import Test.Tasty.Hspec (testSpec)

main :: IO ()
main = defaultMain =<< testSpec "zizek:native" spec

spec :: Spec
spec = do
  rawCApiSpec
  genMachinerySpec
  completionSpec

-- | Drive runs straight through the C API with CBOR schema bytes and raw
-- @hegel_mark_complete@ status codes.
rawCApiSpec :: Spec
rawCApiSpec = describe "raw C API" $ do
  it "round-trips 50 boolean cases" $ runInBoundThread $ do
    let schemaBytes :: ByteString
        schemaBytes = CE.encode (toCBOR Schema.bool)
    withSettings $ \s -> do
      hegel_settings_test_cases s 50
      hegel_settings_verbosity s HEGEL_VERBOSITY_QUIET
      withCString "" (hegel_settings_database s)
      withRun s $ \run -> do
        driveRun schemaBytes run
        resultPtr <- hegel_run_result run
        passed <- hegel_run_result_passed resultPtr
        passed `shouldSatisfy` (/= 0)

  it "marks a case INTERESTING without crashing" $ runInBoundThread $ do
    withSettings $ \s -> do
      hegel_settings_test_cases s 5
      hegel_settings_verbosity s HEGEL_VERBOSITY_QUIET
      withCString "" (hegel_settings_database s)
      let go :: Ptr HegelRun -> Bool -> IO ()
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
      withRun s $ \run -> go run True

  it "drives a full integer failure+shrink cycle" $ runInBoundThread $ do
    let schemaBytes = CE.encode (toCBOR (Schema.integer @Word64 0 255))
        threshold = 10 :: Word64
    withSettings $ \s -> do
      hegel_settings_test_cases s 50
      hegel_settings_verbosity s HEGEL_VERBOSITY_QUIET
      withCString "" (hegel_settings_database s)
      let shrinkLoop :: Ptr HegelRun -> IO ()
          shrinkLoop run = do
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
                shrinkLoop run
      withRun s $ \run -> do
        shrinkLoop run
        resultPtr <- hegel_run_result run
        passed <- hegel_run_result_passed resultPtr
        passed `shouldSatisfy` (== 0)

-- | Drive runs through the 'Hegel.Gen' machinery: 'mkTestCase', 'draw', and the
-- 'Hegel.TestCase' vtable rather than raw schema bytes.
genMachinerySpec :: Spec
genMachinerySpec = describe "Gen machinery" $ do
  it "draws values within range" $ runInBoundThread $ do
    let gen = Gen.integral @Int & Gen.min 0 & Gen.max 100 & Gen.build
    withSettings $ \s -> do
      hegel_settings_test_cases s 10
      hegel_settings_verbosity s HEGEL_VERBOSITY_QUIET
      withCString "" (hegel_settings_database s)
      let loop :: Ptr HegelRun -> IO ()
          loop run = do
            tcPtr <- hegel_next_test_case run
            if tcPtr == nullPtr
              then pure ()
              else do
                let tc = mkTestCase tcPtr
                n <- draw tc gen
                n `shouldSatisfy` (\x -> x >= 0 && x <= 100)
                TC.markComplete tc Valid
                loop run
      withRun s loop

  it "draws, fails, and shrinks" $ runInBoundThread $ do
    let gen = Gen.integral @Int & Gen.min 0 & Gen.max 100 & Gen.build
    withSettings $ \s -> do
      hegel_settings_test_cases s 100
      hegel_settings_verbosity s HEGEL_VERBOSITY_QUIET
      withCString "" (hegel_settings_database s)
      let loop :: Ptr HegelRun -> IO ()
          loop run = do
            tcPtr <- hegel_next_test_case run
            if tcPtr == nullPtr
              then pure ()
              else do
                let tc = mkTestCase tcPtr
                eVal <- try @TestStopped (draw tc gen)
                case eVal of
                  -- Budget exhausted for this shrink probe; mark overrun.
                  Left TestStopped -> TC.markComplete tc Overrun
                  Right n ->
                    if n >= (42 :: Int)
                      then withCString "smoke:0" $ \p -> do
                        rc <- hegel_mark_complete tcPtr HEGEL_STATUS_INTERESTING p
                        case rc of HEGEL_OK -> pure (); HEGEL_E_STOP_TEST -> pure (); _ -> throwOnError rc
                      else TC.markComplete tc Valid
                loop run
      withRun s $ \run -> do
        loop run
        resultPtr <- hegel_run_result run
        passed <- hegel_run_result_passed resultPtr
        passed `shouldSatisfy` (== 0)

-- | Per-case completion error semantics.
completionSpec :: Spec
completionSpec = describe "completion semantics" $
  -- A run-owned test case may be completed exactly once. A second
  -- 'TC.markComplete' is rejected by libhegel with a non-control-flow error
  -- code, which the native vtable raises as a 'HegelError'. In
  -- 'Hegel.Native.Runner' such an error escapes the per-case @catches@ (the
  -- handlers only classify; 'markComplete' runs outside them) and surfaces as
  -- an 'Hegel.Report.Errored' abort rather than crashing the run; this pins
  -- the premise that 'markComplete' genuinely throws.
  it "raises HegelError when a run-owned case is completed twice" $
    runInBoundThread $ do
      withSettings $ \s -> do
        hegel_settings_test_cases s 1
        hegel_settings_verbosity s HEGEL_VERBOSITY_QUIET
        withCString "" (hegel_settings_database s)
        withRun s $ \run -> do
          tcPtr <- hegel_next_test_case run
          tcPtr `shouldNotBe` nullPtr
          let tc = mkTestCase tcPtr
          _ <- draw tc (Gen.bool & Gen.build)
          TC.markComplete tc Valid
          result <- try @HegelError (TC.markComplete tc Valid)
          case result of
            Left _ -> pure ()
            Right () -> expectationFailure "expected HegelError on double completion"

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
              expectationFailure ("CBOR decode failed: " <> err)
            Right (Bool _) ->
              pure ()
            Right v ->
              expectationFailure ("expected Bool, got: " <> show v)
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
