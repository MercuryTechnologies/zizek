-- | Low-level FFI test suite.
--
-- Exercises the raw @libhegel@ C API directly: driving runs with typed draws,
-- calling @hegel_mark_complete@ by hand, the failure+shrink cycle, and
-- per-case completion semantics. These tests work below 'Hegel.Runner' and
-- 'Hegel.Property', complementing the library-behavior tests in the unit
-- suite.
--
-- Each sequence allocates a 'HegelContext' ('withContext') that carries the
-- error buffer for 'throwOnError', and runs in a bound thread (the run still
-- drives a blocking @safe@ FFI call). The on-disk database is disabled so
-- tests do not create @.hegel/@ dirs.
module Main (main) where

import Control.Concurrent (runInBoundThread, threadDelay)
import Control.Concurrent.Async (cancel, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, finally, throwIO, try)
import Data.Default.Class (def)
import Data.Either (isLeft)
import Data.Function ((&))
import Data.Int (Int64)
import Data.Word (Word64, Word8)
import Foreign (Ptr, alloca, castPtr, nullPtr, peek, poke)
import Foreign.C.String (withCString)
import Foreign.C.Types (CBool (..), CDouble (..))
import Hegel.Gen qualified as Gen
import Hegel.Gen.Internal (draw)
import Hegel.Internal.Control (TestStopped (..))
import Hegel.Internal.Foreign.Raw
import Hegel.Internal.TestCase (Handle (..), Status (..), mkTestCase)
import Hegel.Internal.TestCase qualified as TC
import Hegel.Internal.Tick qualified as Tick
import Hegel.Property (forEach)
import Hegel.Runner qualified as Runner
import Test.Hspec
import Test.Tasty (defaultMain)
import Test.Tasty.Hspec (testSpec)
import UnliftIO.IORef (newIORef, readIORef, writeIORef)
import WireEnumCoverage (wireEnumCoverageSpec)

main :: IO ()
main = defaultMain =<< testSpec "zizek:ffi" spec

spec :: Spec
spec = do
  rawCApiSpec
  genMachinerySpec
  completionSpec
  asyncTeardownSpec
  slotSpec
  wireEnumCoverageSpec

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

-- | Draw a single fair-coin boolean straight off the C API (no schema, no
-- 'Hegel.Internal.TestCase.TestCase' wrapper).
drawBooleanRaw :: Ptr HegelContext -> Ptr HegelTestCase -> IO Bool
drawBooleanRaw ctx tc = alloca \outValue -> do
  hegel_generate_boolean ctx tc (CDouble 0.5) (CBool 0) (CBool 0) outValue >>= throwOnError ctx
  (/= 0) . (\(CBool b) -> b) <$> peek outValue

-- | Attempt to draw an integer in @[lo, hi]@, returning 'Nothing' when the
-- choice budget is exhausted ('HEGEL_E_STOP_TEST'). Budget exhaustion
-- genuinely occurs during shrinking (the engine tries candidates with
-- shorter choice sequences than the original failure), so the run loop must
-- handle it by marking 'HEGEL_STATUS_OVERRUN' and continuing.
tryDrawInteger :: Ptr HegelContext -> Ptr HegelTestCase -> Int64 -> Int64 -> IO (Maybe Int64)
tryDrawInteger ctx tc lo hi = alloca \outValue -> do
  rc <- hegel_generate_integer ctx tc lo hi outValue
  if rc == HEGEL_E_STOP_TEST
    then pure Nothing
    else do
      throwOnError ctx rc
      Just <$> peek outValue

-- | Drive runs straight through the C API with the typed boolean draw and
-- raw @hegel_mark_complete@ status codes.
rawCApiSpec :: Spec
rawCApiSpec = describe "raw C API" $ do
  it "round-trips 50 boolean cases" $ runInBoundThread $ do
    withContext $ \ctx -> withSettings ctx $ \s -> do
      configure ctx s 50
      withRun ctx s $ \run -> do
        driveBooleanRun ctx run
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
                _ <- drawBooleanRaw ctx tc
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
    let threshold = 10 :: Int64
    withContext $ \ctx -> withSettings ctx $ \s -> do
      configure ctx s 50
      let shrinkLoop :: Ptr HegelRun -> IO ()
          shrinkLoop run = do
            tc <- nextTestCase ctx run
            if tc == nullPtr
              then pure ()
              else do
                mv <- tryDrawInteger ctx tc 0 255
                case mv of
                  Nothing -> do
                    rc <- hegel_mark_complete ctx tc HEGEL_STATUS_OVERRUN nullPtr
                    case rc of HEGEL_OK -> pure (); HEGEL_E_STOP_TEST -> pure (); _ -> throwOnError ctx rc
                  Just v
                    | v >= threshold ->
                        withCString "smoke:0" $ \p -> do
                          rc <- hegel_mark_complete ctx tc HEGEL_STATUS_INTERESTING p
                          case rc of HEGEL_OK -> pure (); HEGEL_E_STOP_TEST -> pure (); _ -> throwOnError ctx rc
                    | otherwise -> hegel_mark_complete ctx tc HEGEL_STATUS_VALID nullPtr >>= throwOnError ctx
                shrinkLoop run
      withRun ctx s $ \run -> do
        shrinkLoop run
        resultPtr <- runResult ctx run
        passed <- runPassed ctx resultPtr
        passed `shouldBe` False

-- | Loop over every test case the engine produces, draw one boolean each
-- time, and mark the case valid.
driveBooleanRun :: Ptr HegelContext -> Ptr HegelRun -> IO ()
driveBooleanRun ctx run = go
  where
    go = do
      tc <- nextTestCase ctx run
      if tc == nullPtr
        then pure () -- run finished
        else do
          _ <- drawBooleanRaw ctx tc
          hegel_mark_complete ctx tc HEGEL_STATUS_VALID nullPtr >>= throwOnError ctx
          go

-- | Drive runs through the 'Hegel.Gen' machinery: 'mkTestCase', 'draw', and the
-- 'Hegel.Internal.TestCase' operations rather than raw typed-draw calls.
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
                tc <- mkTestCase Tick.Silent Handle {ctx, ptr = tcPtr}
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
                tc <- mkTestCase Tick.Silent Handle {ctx, ptr = tcPtr}
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
          tc <- mkTestCase Tick.Silent Handle {ctx, ptr = tcPtr}
          _ <- draw tc (Gen.bool & Gen.build)
          TC.markComplete tc Valid
          result <- try @HegelError (TC.markComplete tc Valid)
          case result of
            Left _ -> pure ()
            Right () -> expectationFailure "expected HegelError on double completion"

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
          def
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
          _ <- drawBooleanRaw ctx tc
          throwIO (userError "bail mid-case")
        r `shouldSatisfy` isLeft

-- | 'withSlotOf'\/'withSlotBytes' assert the pointee fits the shared 'Slot'
-- buffer rather than silently overrunning it. These cases cover ordinary
-- in-bounds usage and confirm the guard fires on misuse.
slotSpec :: Spec
slotSpec = describe "Slot capacity guard" $ do
  it "withSlotOf round-trips an in-bounds Storable value" $ do
    slot <- newSlot
    withSlotOf slot \p -> poke p (12345 :: Int64)
    v <- withSlotOf slot (peek :: Ptr Int64 -> IO Int64)
    v `shouldBe` 12345

  it "withSlotBytes accepts exactly the slot's capacity" $ do
    slot <- newSlot
    withSlotBytes 16 slot \p -> poke (castPtr p :: Ptr Word8) 0xAB
    v <- withSlotBytes 16 slot (peek . castPtr :: Ptr Word8 -> IO Word8)
    v `shouldBe` 0xAB

  it "withSlotBytes rejects a size larger than the slot's capacity" $ do
    slot <- newSlot
    (withSlotBytes 17 slot (\_ -> pure ()) :: IO ()) `shouldThrow` anyErrorCall
