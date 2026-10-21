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
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64, Word8)
import Foreign (Ptr, alloca, castPtr, nullPtr, peek, poke)
import Foreign.C.Types (CBool (..), CDouble (..))
import Hegel.Gen qualified as Gen
import Hegel.Gen.Internal (draw)
import Hegel.Internal.Control (TestStopped (..))
import Hegel.Internal.Foreign.Raw
import Hegel.Internal.Foreign.String qualified as String
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
  stringAbiSpec
  wireEnumCoverageSpec

-- * Helpers over the out-parameter calling convention

-- | Apply the standard test settings: a fixed case budget, quiet output, and
-- the on-disk database disabled.
configure :: Ptr HegelContext -> Ptr HegelSettings -> Word64 -> IO ()
configure ctx s n = do
  hegel_settings_set_test_cases ctx s n >>= throwOnError ctx
  hegel_settings_set_verbosity ctx s HEGEL_VERBOSITY_QUIET >>= throwOnError ctx
  String.withText "" (hegel_settings_set_database ctx s) >>= throwOnError ctx

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
      withRun ctx s Nothing $ \run -> do
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
                    rc <- String.withText "smoke:0" $ \p ->
                      hegel_mark_complete ctx tc HEGEL_STATUS_INTERESTING p
                    case rc of
                      HEGEL_E_STOP_TEST -> pure () -- normal continue signal
                      _ -> throwOnError ctx rc
                    go run False
                  else do
                    hegel_mark_complete ctx tc HEGEL_STATUS_VALID nullPtr >>= throwOnError ctx
                    go run False
      withRun ctx s Nothing $ \run -> go run True

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
                        String.withText "smoke:0" $ \p -> do
                          rc <- hegel_mark_complete ctx tc HEGEL_STATUS_INTERESTING p
                          case rc of HEGEL_OK -> pure (); HEGEL_E_STOP_TEST -> pure (); _ -> throwOnError ctx rc
                    | otherwise -> hegel_mark_complete ctx tc HEGEL_STATUS_VALID nullPtr >>= throwOnError ctx
                shrinkLoop run
      withRun ctx s Nothing $ \run -> do
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
      withRun ctx s Nothing loop

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
                      then String.withText "smoke:0" $ \p -> do
                        rc <- hegel_mark_complete ctx tcPtr HEGEL_STATUS_INTERESTING p
                        case rc of HEGEL_OK -> pure (); HEGEL_E_STOP_TEST -> pure (); _ -> throwOnError ctx rc
                      else TC.markComplete tc Valid
                loop run
      withRun ctx s Nothing $ \run -> do
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
        withRun ctx s Nothing $ \run -> do
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
        r <- try @SomeException @() $ withRun ctx s Nothing \run -> do
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

-- | Strings cross the boundary as @hegel_string_t@ in both directions. These
-- cases send strings in and read them back out, so a binding whose layout or
-- calling convention disagrees with @hegel.h@ fails here.
stringAbiSpec :: Spec
stringAbiSpec = describe "hegel_string_t round trips" $ do
  it "reads back a non-ASCII origin from the failure it was marked with" $
    runInBoundThread $
      withContext \ctx -> withSettings ctx \s -> do
        configure ctx s 5
        let origin = "smoke:ünïcødé ✓" :: Text
            loop run = do
              tc <- nextTestCase ctx run
              if tc == nullPtr
                then pure ()
                else do
                  rc <- String.withText origin (hegel_mark_complete ctx tc HEGEL_STATUS_INTERESTING)
                  case rc of HEGEL_OK -> pure (); HEGEL_E_STOP_TEST -> pure (); _ -> throwOnError ctx rc
                  loop run
        withRun ctx s Nothing \run -> do
          loop run
          res <- runResult ctx run
          failure <- alloca \out -> do
            throwOnError ctx =<< hegel_run_result_failure ctx res 0 out
            peek out
          failure `shouldNotBe` nullPtr
          got <- alloca \out -> do
            throwOnError ctx =<< hegel_failure_origin ctx failure out
            String.peekText out
          _ <- hegel_failure_free ctx failure
          _ <- hegel_run_result_free ctx res
          got `shouldBe` origin

  it "accepts an event label with an interior NUL" $
    runInBoundThread $
      withContext \ctx -> withSettings ctx \s -> do
        configure ctx s 1
        withRun ctx s Nothing \run -> do
          tc <- nextTestCase ctx run
          tc `shouldNotBe` nullPtr
          String.withText "a\0b" (hegel_event ctx tc) `shouldReturn` HEGEL_OK
          hegel_mark_complete ctx tc HEGEL_STATUS_VALID nullPtr >>= throwOnError ctx

  it "reads back the default, disabled, and directory databases" $
    withContext \ctx -> withSettings ctx \s -> do
      let readDatabase = alloca \out -> do
            throwOnError ctx =<< hegel_settings_get_database ctx s out
            String.peekOptText =<< peek out
      hegel_settings_set_database ctx s nullPtr >>= throwOnError ctx
      readDatabase `shouldReturn` Nothing
      String.withText "" (hegel_settings_set_database ctx s) >>= throwOnError ctx
      readDatabase `shouldReturn` Just ""
      String.withText "zizek-ffi-db" (hegel_settings_set_database ctx s) >>= throwOnError ctx
      readDatabase `shouldReturn` Just "zizek-ffi-db"

  it "sets and clears the default profile, and reports a rejected name" $
    withContext \ctx -> do
      let clear = String.withText "" (hegel_set_default_profile ctx) >>= throwOnError ctx
      -- The default profile is process-wide, so it is cleared even when an
      -- assertion fails, leaving later specs unaffected.
      ( do
          String.withText "ci" (hegel_set_default_profile ctx) >>= throwOnError ctx
          clear
          rc <- String.withText "default" (hegel_set_default_profile ctx)
          rc `shouldBe` HEGEL_E_INVALID_ARG
          lastErrorMessage ctx >>= (`shouldSatisfy` maybe False (not . T.null))
        )
        `finally` clear

  -- A text generator with no include or exclude set passes empty strings for
  -- both, which the engine must read as no restriction. The restricted case
  -- confirms a non-empty include string still narrows the alphabet.
  it "reads empty include and exclude strings as no restriction, and a non-empty one as a restriction" do
    let distinct gen = Set.size . Set.fromList . concatMap T.unpack <$> Runner.samples def 200 gen
    unrestricted <- distinct (Gen.text & Gen.minSize 1 & Gen.build)
    restricted <- distinct (Gen.text & Gen.minSize 1 & Gen.alphabet (Gen.char & Gen.includeCharacters "ab" & Gen.categories []) & Gen.build)
    restricted `shouldSatisfy` (<= 2)
    unrestricted `shouldSatisfy` (> 20)

  it "reads the engine version" $
    withContext \ctx -> do
      version <- alloca \out -> do
        throwOnError ctx =<< hegel_version ctx out
        String.peekText out
      version `shouldSatisfy` (not . T.null)
