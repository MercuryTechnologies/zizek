-- | @libhegel@ property runner.
module Hegel.Runner
  ( check,
    checkWithProgress,
    replay,
    sample,
    samples,
  )
where

import Control.Concurrent.Async (wait, withAsyncBound)
import Control.Exception (SomeException, bracket, finally, mask, toException, try)
import Control.Monad (unless, void)
import Data.Bits ((.|.))
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32)
import Foreign (Ptr, Storable, alloca, fromBool, nullPtr, peek)
import Foreign.C.Types (CBool (..), CInt, CSize)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hegel.Assertion (originOf)
import Hegel.Database (Database (..))
import Hegel.Gen.Internal (Gen, draw)
import Hegel.HealthCheck (HealthCheck)
import Hegel.Internal.Control (ControlSignal (..), FinalizerFailed (..), NoBacktrace (..), catchControl, isAborting)
import Hegel.Internal.Foreign.CString qualified as CString
import Hegel.Internal.Foreign.Raw
import Hegel.Internal.Reconstruction (Failure (..), ReplayResult (..), reconstructFailures, replayOne)
import Hegel.Internal.TestCase (Handle (..), Status (..), TestCase (..), markComplete, mkTestCase)
import Hegel.Internal.Tick qualified as Tick
import Hegel.Phase (Phase (Generate))
import Hegel.Property.Internal
  ( Finalizers,
    Journal (..),
    OpenForks,
    Property,
    cleanupFailures,
    closeOpenForks,
    collectLeaks,
    drainFinalizers,
    failureDetails,
    newFinalizers,
    newOpenForks,
    newRecordingJournal,
    propertyAction,
  )
import Hegel.Replay (ReplayToken)
import Hegel.Report (Abort (..), Event (..), FailureEvidence (..), FailureEvidenceStatus (..), FailureOutcome (..), Note (..), ReplayStats (..), Report (..), Reproduction (..), Result (..), aborted, pattern ReplayedStats, pattern RunStats)
import Hegel.Settings (Settings (..))
import Hegel.Settings qualified as Settings
import UnliftIO.Exception (catchAny, throwIO)
import UnliftIO.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Witch qualified

-- | Run a 'Property' through @libhegel@.
check :: (HasCallStack) => Settings -> Property () -> IO Report
check = withFrozenCallStack $ checkWithProgress (\_ -> pure ())

-- | Run a property and report cumulative completed engine cases after cleanup.
-- Shrink probes count as cases; reconstruction replays do not.
checkWithProgress :: (HasCallStack) => (Int -> IO ()) -> Settings -> Property () -> IO Report
checkWithProgress progress settings prop =
  abortFramework (withAsyncBound go wait)
  where
    go = either throwIO pure (withFrozenCallStack (Settings.validate settings)) *> execute
    execute
      | settings.testCases == 0, DatabaseDisabled <- settings.database = pure (Report (GaveUp "no valid examples found") (RunStats 0 0) Unstored)
      | otherwise = withContext \ctx ->
          withSettings ctx \s -> do
            applySettings HEGEL_MODE_TEST_RUN ctx settings s
            -- Read and copy everything out of the run handle before withRun frees
            -- it on bracket exit (see 'readRunOutcome').
            lastFailure <- newIORef Nothing
            (nValid, nInvalid, outcome) <- withRun ctx s \run -> do
              (nv, ni) <- driveLoop ctx (\journal -> propertyAction journal settings.maxCloneDepth prop) run lastFailure progress
              o <- readRunOutcome ctx run
              pure (nv, ni, o)
            result <- case outcome.status of
              RunPassed
                | nValid == 0 -> pure (GaveUp "no valid examples found")
                | otherwise -> pure Ok
              RunFailed -> case outcome.failures of
                [] -> pure (Aborted (Errored (toException (userError "run reported a failure but exposed no counterexample"))))
                failure : failures -> do
                  version <- engineVersion ctx
                  reconstructed <- reconstructFailures ctx prop s settings.maxCloneDepth version (failure :| failures)
                  pure (Failures reconstructed)
              -- The run itself failed (a health check, an engine panic) and
              -- produced no verdict on the property.
              RunErrored -> pure (Aborted (UnhealthyInput (fromMaybe "the run failed" outcome.runError)))
              RunNondeterministic -> case outcome.failures of
                f : _ -> do
                  mCapture <- readIORef lastFailure
                  pure (unreproducibleCounterexample mCapture f.origin)
                [] ->
                  pure . Aborted . Errored . toException $
                    userError "the run reported a nondeterministic failure but exposed no counterexample"
            pure
              Report
                { result,
                  stats = RunStats nValid nInvalid,
                  reproduction = case result of
                    Failures {} -> failureReproduction outcome.status settings
                    _ -> Unstored
                }

failureReproduction :: RunStatus -> Settings -> Reproduction
failureReproduction status settings = case (status, settings.database, settings.databaseKey) of
  (RunNondeterministic, _, _) -> Unreproducible
  (_, DatabaseDisabled, _) -> Unstored
  (_, _, Just key) -> Stored key
  _ -> Unstored

-- | Replay one failure token against a property without using the example database.
replay :: (HasCallStack) => Settings -> ReplayToken -> Property () -> IO Report
replay settings token prop =
  abortFramework (withAsyncBound go wait)
  where
    go =
      either throwIO pure (withFrozenCallStack (Settings.validate settings)) *> withContext \ctx ->
        withSettings ctx \s -> do
          applySettings HEGEL_MODE_SINGLE_TEST_CASE ctx settings {database = DatabaseDisabled, databaseKey = Nothing} s
          version <- engineVersion ctx
          result <- replayOne ctx s settings.maxCloneDepth version token prop
          pure
            Report
              { result = Failures (result.outcome :| []),
                stats = ReplayedStats result.accounting.replayValid result.accounting.replayInvalid result.accounting,
                reproduction = Unstored
              }

-- | A test case 'runTestCase' itself classified 'Interesting'.
--
-- 'driveLoop' stashes this into a single shared 'IORef', unconditionally
-- overwritten on every 'Interesting' case once the run is recording live.
--
-- __NOTE__: This relies on the engine reporting at most one case worth
-- explaining once it has declared a run nondeterministic; if it ever produced
-- more than one, only the last would be kept.
data LiveFailure = LiveFailure
  { exception :: !SomeException,
    notes :: [Note],
    events :: [Event]
  }

-- | Describe a failure from a run a concurrent state machine declared
-- nondeterministic.
unreproducibleCounterexample :: Maybe LiveFailure -> Text -> Result
unreproducibleCounterexample mCapture origin =
  Failures (FailureOutcome origin Nothing (Observed evidence) [] :| [])
  where
    (message, loc, diff) = maybe (origin, Nothing, Nothing) (failureDetails . (.exception)) mCapture
    evidence = FailureEvidence {message, notes = foldMap (.notes) mCapture, events = foldMap (.events) mCapture, loc, diff}

-- * Sampling

-- | Draw a single value from @gen@ outside a property run.
--
-- An unsatisfied 'Hegel.Property.assume', an exhausted 'Hegel.Gen.filtered'
-- retry budget, and an exhausted choice budget all throw an 'IOError'.
--
-- Every draw comes from the same distribution an ordinary property test
-- case draws from, which tends to be biased toward edge cases and boundary
-- conditions. As such, this function should be used to iterate on generators
-- or probe fixtures in a REPL, not to generate realistic-looking data.
--
-- __Do not call this from inside a 'Property' body!__
--
-- It starts its own engine run, so the value it draws never enters the
-- enclosing run's choice sequence.
--
-- A shrink probe or the final reconstruction replay then draws a
-- different value than the live case did; the enclosing run detects this as
-- nondeterminism, but only once it replays that case's prefix, so the
-- failure may not appear on the first affected case.
sample :: (HasCallStack) => Settings -> Gen a -> IO a
sample settings gen =
  withAsyncBound go wait
  where
    go =
      either throwIO pure (withFrozenCallStack (Settings.validate settings)) *> withContext \ctx ->
        withSettings ctx \s -> do
          applySettings HEGEL_MODE_SINGLE_TEST_CASE ctx settings s
          withRun ctx s (drawOneCase ctx gen)

-- | Pull the one test case a single-test-case run offers, draw @gen@
-- against it, and report the outcome.
drawOneCase :: Ptr HegelContext -> Gen a -> Ptr HegelRun -> IO a
drawOneCase ctx gen run = do
  tcPtr <- alloca \out -> do
    throwOnError ctx =<< hegel_next_test_case ctx run out
    peek out
  if tcPtr == nullPtr
    then throwIO (userError "sample: the engine produced no test case")
    else runCase tcPtr `finally` void (hegel_test_case_free ctx tcPtr)
  where
    runCase tcPtr = do
      tc <- mkTestCase Tick.Silent Handle {ctx, ptr = tcPtr}
      (Right <$> draw tc gen)
        `catchControl` (pure . Left)
        >>= \case
          Right a -> markComplete tc Valid $> a
          Left Assume -> markComplete tc Invalid *> throwIO (userError "sample: the generator discarded its only case")
          Left Stop -> markComplete tc Overrun *> throwIO (userError "sample: the engine's choice budget was exhausted")

-- | Draw up to @n@ values from @gen@, with no shrinking and no persistence.
--
-- Values are more varied than @n@ independent 'sample' calls would give, as
-- they are drawn from the same underlying choice stream.
--
-- A generator that discards yields fewer than @n@ values, possibly none, and
-- one whose valid rate stays low throws an 'IOError' from
-- 'Hegel.HealthCheck.FilterTooMuch'.
--
-- __Do not call this from inside a 'Property' body!__
--
-- See the 'sample' documentation for additional details.
samples :: (HasCallStack) => Settings -> Int -> Gen a -> IO [a]
samples settings n gen =
  withAsyncBound go wait
  where
    go = either throwIO pure (withFrozenCallStack (Settings.validate settings {testCases = n})) *> execute
    execute
      | n == 0 = pure []
      | otherwise = withContext \ctx ->
          withSettings ctx \s -> do
            applySettings HEGEL_MODE_TEST_RUN ctx settings {testCases = n, phases = [Generate], database = DatabaseDisabled, databaseKey = Nothing} s
            acc <- newIORef []
            outcome <- withRun ctx s \run -> do
              collectCases ctx gen acc run
              readRunOutcome ctx run
            case outcome.status of
              RunErrored -> throwIO (userError (T.unpack (fromMaybe "the run failed" outcome.runError)))
              -- 'samples' runs no property body, so nothing can mark a case
              -- 'Interesting', and 'RunFailed' should not arise here.
              --
              -- This arm covers it anyway, returning whatever was collected rather
              -- than trying to interpret an outcome that should not occur.
              _ -> reverse <$> readIORef acc

-- | Pull every test case the engine offers, drawing @gen@ against each and
-- consing successes onto @acc@.
collectCases :: Ptr HegelContext -> Gen a -> IORef [a] -> Ptr HegelRun -> IO ()
collectCases ctx gen acc run = loop
  where
    loop = do
      tcPtr <- alloca \out -> do
        throwOnError ctx =<< hegel_next_test_case ctx run out
        peek out
      unless (tcPtr == nullPtr) do
        runCase tcPtr `finally` void (hegel_test_case_free ctx tcPtr)
        loop
    runCase tcPtr = do
      tc <- mkTestCase Tick.Silent Handle {ctx, ptr = tcPtr}
      (draw tc gen >>= \a -> modifyIORef' acc (a :) *> markComplete tc Valid)
        `catchControl` \case
          Assume -> markComplete tc Invalid
          Stop -> markComplete tc Overrun

-- * Settings

-- | Map a 'Settings' value onto the corresponding @libhegel@ settings
-- setters, under the given @hegel_mode_t@ wire value.
--
-- The mode is not part of 'Settings'.
--
-- 'check' always drives the full generate\/shrink\/replay loop ('HEGEL_MODE_TEST_RUN').
--
-- 'sample' and 'samples' are the only callers that ask for
-- 'HEGEL_MODE_SINGLE_TEST_CASE' or a generation-only phase set.
applySettings :: Word32 -> Ptr HegelContext -> Settings -> Ptr HegelSettings -> IO ()
applySettings mode ctx s ptr = do
  chk $ hegel_settings_set_mode ctx ptr mode
  chk $ hegel_settings_set_backend ctx ptr (Witch.into @Word32 s.backend)
  chk $ hegel_settings_set_test_cases ctx ptr (fromIntegral s.testCases)
  chk $ hegel_settings_set_stateful_step_count ctx ptr (fromIntegral s.statefulStepCount)
  chk $ hegel_settings_set_verbosity ctx ptr (Witch.into @Word32 s.verbosity)

  case s.seed of
    Nothing -> chk $ hegel_settings_set_seed ctx ptr 0 (CBool 0)
    Just seed ->
      chk $ hegel_settings_set_seed ctx ptr seed (CBool 1)

  chk $ hegel_settings_set_derandomize ctx ptr (fromBool s.derandomize)
  chk $ hegel_settings_set_report_multiple_failures ctx ptr (fromBool s.reportMultipleFailures)
  chk $ hegel_settings_set_phases ctx ptr (phasesBitmask s.phases)
  chk $ hegel_settings_set_suppress_health_check ctx ptr (hcBitmask s.suppressHealthCheck)

  -- "" disables the store; skipping the call leaves the engine default
  -- (.hegel/ under the cwd).
  case s.database of
    DatabaseDefault -> pure ()
    DatabaseDisabled -> CString.withFilePath "" \p -> chk $ hegel_settings_set_database ctx ptr p
    DatabaseDirectory dir -> CString.withFilePath dir \p -> chk $ hegel_settings_set_database ctx ptr p

  for_ s.databaseKey \key ->
    CString.withText key \p -> chk $ hegel_settings_set_database_key ctx ptr p
  where
    chk io = io >>= throwOnError ctx

-- | OR the per-phase wire flags into a bitmask.
--
-- An empty list yields @0@, which disables all phases.
phasesBitmask :: [Phase] -> Word32
phasesBitmask = foldl' (\acc p -> acc .|. Witch.into @Word32 p) 0

-- | OR the per-health-check wire flags into a suppression bitmask.
hcBitmask :: [HealthCheck] -> Word32
hcBitmask = foldl' (\acc hc -> acc .|. Witch.into @Word32 hc) 0

-- * Failures

-- | The aggregate verdict of a finished run.
data RunStatus
  = -- | The property held across every generated test case.
    RunPassed
  | -- | The property failed; inspect the counterexample(s).
    RunFailed
  | -- | The run itself failed and produced no verdict on the property.
    RunErrored
  | -- | The property failed on a run a concurrent state machine declared
    -- nondeterministic; the failure carries no reproduce blob.
    RunNondeterministic
  deriving stock (Show, Eq)

-- | Decode the @hegel_run_status_t@ wire code; an unrecognized code is treated
-- as 'RunErrored'.
instance Witch.TryFrom CInt RunStatus where
  tryFrom = Witch.maybeTryFrom \case
    HEGEL_RUN_STATUS_PASSED -> Just RunPassed
    HEGEL_RUN_STATUS_FAILED -> Just RunFailed
    HEGEL_RUN_STATUS_ERROR -> Just RunErrored
    HEGEL_RUN_STATUS_FAILED_NONDETERMINISTIC -> Just RunNondeterministic
    _ -> Nothing

-- | The aggregated verdict of a finished run.
data RunOutcome = RunOutcome
  { -- | The decoded run status.
    status :: !RunStatus,
    -- | Distinct failures, in engine order.
    failures :: ![Failure],
    -- | The run-level error message, when the run errored.
    runError :: !(Maybe Text)
  }

-- | Read the aggregate status, the primary failure, and the run-level error
-- out of the engine's result, copying anything we keep.
readRunOutcome :: Ptr HegelContext -> Ptr HegelRun -> IO RunOutcome
readRunOutcome ctx run =
  bracket (outWith (hegel_run_result ctx run)) (void . hegel_run_result_free ctx) \res -> do
    rawStatus <- outWith (hegel_run_result_status ctx res)
    let status = either (const RunErrored) id (Witch.tryInto rawStatus)
    failures <- readFailures ctx res
    runError <- readRunError ctx res
    pure RunOutcome {status, failures, runError}
  where
    -- Run one @out_*@ call, checking its return code and reading the result.
    outWith :: (Storable a) => (Ptr a -> IO CInt) -> IO a
    outWith act = alloca \out -> do
      throwOnError ctx =<< act out
      peek out

-- | Read and copy every failure from the engine's run result, in engine order.
readFailures :: Ptr HegelContext -> Ptr HegelRunResult -> IO [Failure]
readFailures ctx res = do
  count <- alloca \out -> do
    throwOnError ctx =<< hegel_run_result_failure_count ctx res out
    peek out
  traverse (readFailure ctx res . fromIntegral) [0 .. (fromIntegral count :: Int) - 1]

readFailure :: Ptr HegelContext -> Ptr HegelRunResult -> CSize -> IO Failure
readFailure ctx res index = bracket
  ( alloca \out -> do
      throwOnError ctx =<< hegel_run_result_failure ctx res index out
      peek out
  )
  (void . hegel_failure_free ctx)
  \f -> do
    if f == nullPtr
      then pure Failure {origin = "<missing failure>", reproductionBlob = Nothing}
      else do
        org <- alloca \out -> do
          throwOnError ctx =<< hegel_failure_origin ctx f out
          peekUtf8 =<< peek out
        blob <- failureReproductionBlob ctx f
        pure Failure {origin = org, reproductionBlob = blob}

-- | Read and copy the run-level error message, if the run carries one.
readRunError :: Ptr HegelContext -> Ptr HegelRunResult -> IO (Maybe Text)
readRunError ctx res =
  alloca \out -> do
    throwOnError ctx =<< hegel_run_result_error ctx res out
    ptr <- peek out
    if ptr == nullPtr
      then pure Nothing
      else do
        msg <- peekUtf8 ptr
        pure (if T.null msg then Nothing else Just msg)

engineVersion :: Ptr HegelContext -> IO Text
engineVersion ctx = alloca \out -> do
  throwOnError ctx =<< hegel_version ctx out
  peekUtf8 =<< peek out

-- * Per-case loop

driveLoop ::
  Ptr HegelContext ->
  (Journal -> Finalizers -> OpenForks -> TestCase -> IO ()) ->
  Ptr HegelRun ->
  IORef (Maybe LiveFailure) ->
  (Int -> IO ()) ->
  IO (Int, Int)
driveLoop ctx action run lastFailure progress = loop 0 0 0
  where
    loop !nValid !nInvalid !completed = do
      tcPtr <- alloca \out -> do
        throwOnError ctx =<< hegel_next_test_case ctx run out
        peek out
      if tcPtr == nullPtr
        then pure (nValid, nInvalid)
        else do
          status <- runTestCase ctx action tcPtr lastFailure `finally` void (hegel_test_case_free ctx tcPtr)
          progress (completed + 1)
          case status of
            Valid -> loop (nValid + 1) nInvalid (completed + 1)
            Invalid -> loop nValid (nInvalid + 1) (completed + 1)
            Interesting _ -> loop nValid nInvalid (completed + 1)
            Overrun -> loop nValid nInvalid (completed + 1)

-- | Run one engine-produced test case.
runTestCase ::
  Ptr HegelContext ->
  (Journal -> Finalizers -> OpenForks -> TestCase -> IO ()) ->
  Ptr HegelTestCase ->
  IORef (Maybe LiveFailure) ->
  IO Status
runTestCase ctx action tcPtr lastFailure = do
  finalizers <- newFinalizers
  forks <- newOpenForks
  -- The body exception is retained separately from the engine's deduplication
  -- origin so a teardown abort still explains the original failure.
  caseFailure <- newIORef Nothing
  -- Every exit drains cleanup, retaining a runner error alongside any
  -- cleanup diagnostics.
  mask \restore -> do
    result <- try $ restore $ run finalizers forks caseFailure
    _ <- drainFinalizers finalizers
    -- Defense in depth: 'run' already settles every fork itself, before
    -- 'markComplete'. This only does anything if 'run' escaped via a
    -- genuinely asynchronous exception before reaching that point, since
    -- 'catchAny' below absorbs every synchronous one.
    _ <- collectLeaks forks
    failures <- cleanupFailures finalizers
    case result of
      Left (e :: SomeException)
        | not (null failures),
          isAborting e ->
            throwIO $ FinalizerFailed (Just e) failures
        | otherwise -> throwIO $ NoBacktrace e
      Right status -> case failures of
        [] -> pure status
        -- A captured finalizer failure aborts the run; 'drainFinalizers'
        -- captures every finalizer exception, so nothing escapes uncaught.
        es -> do
          bodyFailure <- readIORef caseFailure
          throwIO $ FinalizerFailed bodyFailure es
  where
    run finalizers forks caseFailure = do
      nondeterministic <- isNondeterministic ctx tcPtr
      (recording, journal, drainNotes) <-
        if nondeterministic
          then do
            recording <- Tick.newRecording
            (journal, drainNotes) <- newRecordingJournal
            pure (recording, journal, drainNotes)
          else pure (Tick.Silent, Silent, pure [])
      tc <- mkTestCase recording Handle {ctx, ptr = tcPtr}
      status <-
        -- 'catchControl' catches only Hegel's async control signals via base
        -- 'E.catches'; 'catchAny' (unliftio) then catches all remaining
        -- synchronous user exceptions; framework errors abort exploration.
        (action journal finalizers forks tc $> Valid)
          `catchControl` \case
            Assume -> pure Invalid
            -- @libhegel@ owns the choice budget but does not observe that we
            -- stopped; report Overrun explicitly to let the engine shrink
            Stop -> pure Overrun
          `catchAny` \e -> case isAborting e of
            True -> throwIO e
            False -> do
              writeIORef caseFailure (Just e)
              -- Stashed for 'check''s 'RunNondeterministic' arm, which has no
              -- reproduction blob to replay for its own failure content.
              case recording of
                Tick.Silent -> pure ()
                Tick.Active _ -> do
                  notes <- drainNotes
                  events <- Tick.drain tc.events
                  writeIORef lastFailure (Just LiveFailure {exception = e, notes, events})
              pure . Interesting $ originOf e
      -- Must settle every fork before markComplete: one still drawing
      -- against its clone when the family completes fails with an engine
      -- error of its own, rather than the well-formed 'MalformedTest'
      -- 'closeOpenForks' produces here.
      closeOpenForks forks
      markComplete tc status
      pure status

-- | Whether the engine has already flagged this test case as belonging to a
-- run declared nondeterministic (see 'HEGEL_RUN_STATUS_FAILED_NONDETERMINISTIC').
isNondeterministic :: Ptr HegelContext -> Ptr HegelTestCase -> IO Bool
isNondeterministic ctx tcPtr = alloca \out -> do
  throwOnError ctx =<< hegel_test_case_is_nondeterministic ctx tcPtr out
  (\(CBool b) -> b /= 0) <$> peek out

-- | Report framework aborts while preserving unrelated exceptions and cancellation.
abortFramework :: IO Report -> IO Report
abortFramework action =
  action `catchAny` \e ->
    if isAborting e
      then pure (aborted (Errored e))
      else throwIO (NoBacktrace e)
