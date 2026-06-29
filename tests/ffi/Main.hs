-- | Low-level FFI test suite.
--
-- Exercises the raw @libhegel@ C API directly: driving runs with CBOR schema
-- bytes, calling @hegel_mark_complete@ by hand, the failure+shrink cycle, and
-- per-case completion semantics. These tests work below 'Hegel.Runner' and
-- 'Hegel.Property', complementing the library-behaviour tests in the unit
-- suite.
--
-- Each sequence allocates a 'HegelContext' ('withContext') that carries the
-- error buffer for 'throwOnError', and runs in a bound thread (the run still
-- drives a blocking @safe@ FFI call). The on-disk database is disabled so
-- tests do not create @.hegel/@ dirs.
module Main (main) where

import CBOR.Class (ToCBOR (toCBOR))
import CBOR.Decode qualified as CD
import CBOR.Encode qualified as CE
import CBOR.Value (Value (..))
import Control.Concurrent (runInBoundThread, threadDelay)
import Control.Concurrent.Async (cancel, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, finally, throwIO, try)
import Data.ByteString (ByteString)
import Data.Either (isLeft)
import Data.Function ((&))
import Data.Word (Word64)
import Foreign (Ptr, alloca, nullPtr, peek)
import Foreign.C.String (withCString)
import Hegel.Gen qualified as Gen
import Hegel.Gen.Internal (draw)
import Hegel.Internal.Control (TestStopped (..))
import Hegel.Internal.FFI
import Hegel.Internal.Schema qualified as Schema
import Hegel.Internal.TestCase (Status (..), mkTestCase)
import Hegel.Internal.TestCase qualified as TC
import Hegel.Property (forEach)
import Hegel.Runner qualified as Runner
import Hegel.Settings (defaultSettings)
import Test.Hspec
import Test.Tasty (defaultMain)
import Test.Tasty.Hspec (testSpec)
import UnliftIO.IORef (newIORef, readIORef, writeIORef)

main :: IO ()
main = defaultMain =<< testSpec "zizek:ffi" spec

spec :: Spec
spec = do
  rawCApiSpec
  genMachinerySpec
  completionSpec
  asyncTeardownSpec

-- * Helpers over the out-parameter calling convention

-- | Apply the standard test settings: a fixed case budget, quiet output, and
-- the on-disk database disabled.
configure :: Ptr HegelContext -> Ptr HegelSettings -> Word64 -> IO ()
configure ctx s n = do
  hegel_settings_set_test_cases ctx s n >>= throwOnError ctx
  hegel_settings_set_verbosity ctx s HEGEL_VERBOSITY_QUIET >>= throwOnError ctx
  withCString "" (hegel_settings_set_database ctx s) >>= throwOnError ctx

-- | Pull the next test case (or 'nullPtr' when the run is finished).
nextTestCase :: Ptr HegelContext -> Ptr HegelRun -> IO (Ptr HegelTestCase)
nextTestCase ctx run = alloca \out -> do
  throwOnError ctx =<< hegel_next_test_case ctx run out
  peek out

-- | Read the aggregated run result.
runResult :: Ptr HegelContext -> Ptr HegelRun -> IO (Ptr HegelRunResult)
runResult ctx run = alloca \out -> do
  throwOnError ctx =<< hegel_run_result ctx run out
  peek out

-- | Whether the run passed (as opposed to failing or erroring).
runPassed :: Ptr HegelContext -> Ptr HegelRunResult -> IO Bool
runPassed ctx res = alloca \out -> do
  throwOnError ctx =<< hegel_run_result_status ctx res out
  (== HEGEL_RUN_STATUS_PASSED) <$> peek out

-- | Drive runs straight through the C API with CBOR schema bytes and raw
-- @hegel_mark_complete@ status codes.
rawCApiSpec :: Spec
rawCApiSpec = describe "raw C API" $ do
  it "round-trips 50 boolean cases" $ runInBoundThread $ do
    let schemaBytes :: ByteString
        schemaBytes = CE.encode (toCBOR Schema.bool)
    withContext $ \ctx -> withSettings ctx $ \s -> do
      configure ctx s 50
      withRun ctx s $ \run -> do
        driveRun ctx schemaBytes run
        resultPtr <- runResult ctx run
        passed <- runPassed ctx resultPtr
        passed `shouldBe` True

  it "marks a case INTERESTING without crashing" $ runInBoundThread $ do
    withContext $ \ctx -> withSettings ctx $ \s -> do
      configure ctx s 5
      let go :: Ptr HegelRun -> Bool -> IO ()
          go run markFirst = do
            tc <- nextTestCase ctx run
            if tc == nullPtr
              then pure ()
              else do
                _ <- generate ctx tc (CE.encode (toCBOR Schema.bool))
                if markFirst
                  then do
                    rc <- withCString "smoke:0" $ \p ->
                      hegel_mark_complete ctx tc HEGEL_STATUS_INTERESTING p
                    case rc of
                      HEGEL_E_STOP_TEST -> pure () -- normal continue signal
                      _ -> throwOnError ctx rc
                    go run False
                  else do
                    hegel_mark_complete ctx tc HEGEL_STATUS_VALID nullPtr >>= throwOnError ctx
                    go run False
      withRun ctx s $ \run -> go run True

  it "drives a full integer failure+shrink cycle" $ runInBoundThread $ do
    let schemaBytes = CE.encode (toCBOR (Schema.integer @Word64 0 255))
        threshold = 10 :: Word64
    withContext $ \ctx -> withSettings ctx $ \s -> do
      configure ctx s 50
      let shrinkLoop :: Ptr HegelRun -> IO ()
          shrinkLoop run = do
            tc <- nextTestCase ctx run
            if tc == nullPtr
              then pure ()
              else do
                mbs <- tryDraw ctx tc schemaBytes
                case mbs of
                  Nothing -> do
                    rc <- hegel_mark_complete ctx tc HEGEL_STATUS_OVERRUN nullPtr
                    case rc of HEGEL_OK -> pure (); HEGEL_E_STOP_TEST -> pure (); _ -> throwOnError ctx rc
                  Just bs -> case CD.decode bs of
                    Right (UInt v)
                      | v >= threshold ->
                          withCString "smoke:0" $ \p -> do
                            rc <- hegel_mark_complete ctx tc HEGEL_STATUS_INTERESTING p
                            case rc of HEGEL_OK -> pure (); HEGEL_E_STOP_TEST -> pure (); _ -> throwOnError ctx rc
                    _ -> hegel_mark_complete ctx tc HEGEL_STATUS_VALID nullPtr >>= throwOnError ctx
                shrinkLoop run
      withRun ctx s $ \run -> do
        shrinkLoop run
        resultPtr <- runResult ctx run
        passed <- runPassed ctx resultPtr
        passed `shouldBe` False

-- | Drive runs through the 'Hegel.Gen' machinery: 'mkTestCase', 'draw', and the
-- 'Hegel.Internal.TestCase' operations rather than raw schema bytes.
genMachinerySpec :: Spec
genMachinerySpec = describe "Gen machinery" $ do
  it "draws values within range" $ runInBoundThread $ do
    let gen = Gen.integral @Int & Gen.min 0 & Gen.max 100 & Gen.build
    withContext $ \ctx -> withSettings ctx $ \s -> do
      configure ctx s 10
      let loop :: Ptr HegelRun -> IO ()
          loop run = do
            tcPtr <- nextTestCase ctx run
            if tcPtr == nullPtr
              then pure ()
              else do
                let tc = mkTestCase ctx tcPtr
                n <- draw tc gen
                n `shouldSatisfy` (\x -> x >= 0 && x <= 100)
                TC.markComplete tc Valid
                loop run
      withRun ctx s loop

  it "draws, fails, and shrinks" $ runInBoundThread $ do
    let gen = Gen.integral @Int & Gen.min 0 & Gen.max 100 & Gen.build
    withContext $ \ctx -> withSettings ctx $ \s -> do
      configure ctx s 100
      let loop :: Ptr HegelRun -> IO ()
          loop run = do
            tcPtr <- nextTestCase ctx run
            if tcPtr == nullPtr
              then pure ()
              else do
                let tc = mkTestCase ctx tcPtr
                eVal <- try @TestStopped (draw tc gen)
                case eVal of
                  -- Budget exhausted for this shrink probe; mark overrun.
                  Left TestStopped -> TC.markComplete tc Overrun
                  Right n ->
                    if n >= (42 :: Int)
                      then withCString "smoke:0" $ \p -> do
                        rc <- hegel_mark_complete ctx tcPtr HEGEL_STATUS_INTERESTING p
                        case rc of HEGEL_OK -> pure (); HEGEL_E_STOP_TEST -> pure (); _ -> throwOnError ctx rc
                      else TC.markComplete tc Valid
                loop run
      withRun ctx s $ \run -> do
        loop run
        resultPtr <- runResult ctx run
        passed <- runPassed ctx resultPtr
        passed `shouldBe` False

-- | Per-case completion error semantics.
completionSpec :: Spec
completionSpec = describe "completion semantics" $
  -- A run-owned test case may be completed exactly once. A second
  -- 'TC.markComplete' is rejected by libhegel with a non-control-flow error
  -- code, and 'markComplete' raises a 'HegelError'. In 'Hegel.Runner' such
  -- an error escapes the per-case @catches@ (the handlers only classify;
  -- 'markComplete' runs outside them) and surfaces as an
  -- 'Hegel.Report.Errored' abort rather than crashing the run; this pins the
  -- premise that 'markComplete' genuinely throws.
  it "raises HegelError when a run-owned case is completed twice" $
    runInBoundThread $ do
      withContext $ \ctx -> withSettings ctx $ \s -> do
        configure ctx s 1
        withRun ctx s $ \run -> do
          tcPtr <- nextTestCase ctx run
          tcPtr `shouldNotBe` nullPtr
          let tc = mkTestCase ctx tcPtr
          _ <- draw tc (Gen.bool & Gen.build)
          TC.markComplete tc Valid
          result <- try @HegelError (TC.markComplete tc Valid)
          case result of
            Left _ -> pure ()
            Right () -> expectationFailure "expected HegelError on double completion"

-- | Loop over every test case the engine produces, draw one boolean each
-- time, assert the CBOR decodes correctly, and mark the case valid.
driveRun :: Ptr HegelContext -> ByteString -> Ptr HegelRun -> IO ()
driveRun ctx schemaBytes run = go
  where
    go :: IO ()
    go = do
      tc <- nextTestCase ctx run
      if tc == nullPtr
        then pure () -- run finished
        else do
          bs <- generate ctx tc schemaBytes
          case CD.decode bs of
            Left err ->
              expectationFailure ("CBOR decode failed: " <> err)
            Right (Bool _) ->
              pure ()
            Right v ->
              expectationFailure ("expected Bool, got: " <> show v)
          hegel_mark_complete ctx tc HEGEL_STATUS_VALID nullPtr >>= throwOnError ctx
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
tryDraw :: Ptr HegelContext -> Ptr HegelTestCase -> ByteString -> IO (Maybe ByteString)
tryDraw ctx tc schema = do
  result <- try @HegelError (generate ctx tc schema)
  case result of
    Right bs -> pure (Just bs)
    Left HegelError {code = HEGEL_E_STOP_TEST} -> pure Nothing
    Left err -> throwIO err

-- | Async teardown tests. These go through 'Runner.check' rather than the
-- raw FFI because the fix ('withAsyncBound' in 'Hegel.Runner.check') lives
-- there.
asyncTeardownSpec :: Spec
asyncTeardownSpec = describe "async teardown" $ do
  it "cancels the bound worker when the check caller is interrupted" $ do
    started <- newEmptyMVar
    cleanedUp <- newIORef False
    withAsync
      ( Runner.check
          defaultSettings
          ( forEach (Gen.bool & Gen.build) \_ ->
              putMVar started () *> threadDelay 5_000_000 `finally` writeIORef cleanedUp True
          )
      )
      \a -> takeMVar started *> cancel a
    readIORef cleanedUp `shouldReturn` True

  -- Bailing out of the test loop with an active, un-completed test case must
  -- not hang; if 'hegel_run_free' blocked on the in-flight case the test
  -- would time out.
  it "drains an active, un-completed test case on early exit" $
    runInBoundThread $
      withContext \ctx -> withSettings ctx \s -> do
        configure ctx s 50
        r <- try @SomeException @() $ withRun ctx s \run -> do
          tc <- nextTestCase ctx run
          _ <- generate ctx tc (CE.encode (toCBOR Schema.bool))
          throwIO (userError "bail mid-case")
        r `shouldSatisfy` isLeft
