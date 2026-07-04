-- | @libhegel@ property runner.
module Hegel.Runner
  ( check,
    sample,
    samples,
  )
where

import Control.Concurrent.Async (wait, withAsyncBound)
import Control.Exception (SomeException, bracket, finally, fromException, mask, toException, try)
import Control.Monad (unless, void)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32)
import Foreign (Ptr, Storable, alloca, fromBool, nullPtr, peek)
import Foreign.C.Types (CBool (..), CInt, CSize)
import Hegel.Assertion (originOf)
import Hegel.Database (Database (..))
import Hegel.Gen.Internal (Gen, draw)
import Hegel.HealthCheck (HealthCheck)
import Hegel.Internal.Control (ControlSignal (..), FinalizerFailed (..), MalformedTest, NoBacktrace (..), catchControl, isControlSignal)
import Hegel.Internal.Foreign.CString qualified as CString
import Hegel.Internal.Foreign.Raw
import Hegel.Internal.TestCase (Handle (..), Status (..), TestCase, markComplete, mkTestCase)
import Hegel.Internal.Tick qualified as Tick
import Hegel.Phase (Phase (Generate))
import Hegel.Property.Internal
  ( Finalizers,
    OpenForks,
    Property,
    closeOpenForks,
    collectLeaks,
    drainFinalizers,
    failureDetails,
    newFinalizers,
    newOpenForks,
    observeProperty,
    propertyAction,
  )
import Hegel.Report (Abort (..), Report (..), Result (..), Stats (..), aborted)
import Hegel.Settings (Settings (..))
import UnliftIO.Exception (catch, catchAny, throwIO)
import UnliftIO.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Witch qualified

-- | Run a 'Property' through @libhegel@.
--
-- The engine's run result decides the verdict: a failure with
-- a reproduction blob is replayed through the property to describe the
-- counterexample ('reconstructProperty'); one without is a health-check
-- abort; otherwise the tally decides between 'GaveUp' and 'Ok'.
check :: Settings -> Property () -> IO Report
check settings prop =
  -- Note: 'safe' blocking FFI calls (notably 'hegel_next_test_case') cannot
  -- be interrupted by async exceptions; a cancellation signal is deferred
  -- until that call returns.
  --
  -- A @libhegel@ call outside the per-case try (e.g. engine startup, blob
  -- replay, markComplete) can fail with a HegelError; surface it as
  -- Errored rather than letting it escape the runner.
  --
  -- A MalformedTest (e.g. a state-machine test with no rules) is likewise a
  -- run-level abort, not a counterexample.
  withAsyncBound go wait
    `catch` (\(e :: MalformedTest) -> pure . aborted . Errored $ toException e)
    `catch` (\(e :: HegelError) -> pure . aborted . Errored $ toException e)
    -- A finalizer that threw while draining at the case boundary; teardown
    -- failed, so per-case isolation may be broken.
    --
    -- Abort rather than trust the remaining cases or a shrink built on a
    -- contaminated environment.
    `catch` (\(e :: FinalizerFailed) -> pure . aborted . Errored $ toException e)
  where
    go = withContext \ctx ->
      withSettings ctx \s -> do
        applySettings HEGEL_MODE_TEST_RUN ctx settings s
        -- Read and copy everything out of the run handle before withRun frees
        -- it on bracket exit (see 'readRunOutcome').
        (nValid, nInvalid, outcome) <- withRun ctx s \run -> do
          (nv, ni) <- driveLoop ctx (propertyAction settings.maxCloneDepth prop) run
          o <- readRunOutcome ctx run
          pure (nv, ni, o)
        result <- case outcome.status of
          RunPassed
            | nValid == 0 -> pure (GaveUp "no valid examples found")
            | otherwise -> pure Ok
          RunFailed -> case outcome.failure of
            Just f
              | Just blob <- f.reproductionBlob -> reconstructProperty ctx prop s settings.maxCloneDepth blob
              | otherwise -> pure (Aborted (UnhealthyInput f.origin))
            Nothing ->
              pure (Aborted (Errored (toException (userError "run reported a failure but exposed no counterexample"))))
          -- The run itself failed (a health check, a nondeterministic test, an
          -- engine panic) and produced no verdict on the property.
          RunErrored -> pure (Aborted (UnhealthyInput (fromMaybe "the run failed" outcome.runError)))
        pure
          Report
            { result,
              stats = Stats {valid = nValid, invalid = nInvalid},
              -- The reproduction surface, for the failure footer: only a
              -- persisted key is honest to point at.
              databaseKey = case settings.database of
                DatabaseDisabled -> Nothing
                _ -> settings.databaseKey
            }

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
sample :: Settings -> Gen a -> IO a
sample settings gen =
  withAsyncBound go wait
  where
    go = withContext \ctx ->
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
samples :: Settings -> Int -> Gen a -> IO [a]
samples settings n gen =
  withAsyncBound go wait
  where
    go = withContext \ctx ->
      withSettings ctx \s -> do
        applySettings HEGEL_MODE_TEST_RUN ctx settings {testCases = n, phases = [Generate]} s
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

-- | A single failure copied out of the run result.
data Failure = Failure
  { -- | Stable, draw-independent deduplication key (e.g. @\"file:line\"@).
    origin :: !Text,
    -- | Base64 reproduction blob, or 'Nothing' for failures that carry none
    -- (e.g. a health-check failure).
    reproductionBlob :: !(Maybe ByteString)
  }

-- | The aggregate verdict of a finished run.
data RunStatus
  = -- | The property held across every generated test case.
    RunPassed
  | -- | The property failed; inspect the counterexample(s).
    RunFailed
  | -- | The run itself failed and produced no verdict on the property.
    RunErrored
  deriving stock (Show, Eq)

-- | Decode the @hegel_run_status_t@ wire code; an unrecognized code is treated
-- as 'RunErrored'.
instance Witch.TryFrom CInt RunStatus where
  tryFrom = Witch.maybeTryFrom \case
    HEGEL_RUN_STATUS_PASSED -> Just RunPassed
    HEGEL_RUN_STATUS_FAILED -> Just RunFailed
    HEGEL_RUN_STATUS_ERROR -> Just RunErrored
    _ -> Nothing

-- | The aggregated verdict of a finished run.
data RunOutcome = RunOutcome
  { -- | The decoded run status.
    status :: !RunStatus,
    -- | The first distinct failure, when the run failed.
    --
    -- 'Report' carries a single counterexample, so any additional distinct
    -- failures are not surfaced.
    failure :: !(Maybe Failure),
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
    failure <- readPrimaryFailure ctx res
    runError <- readRunError ctx res
    pure RunOutcome {status, failure, runError}
  where
    -- Run one @out_*@ call, checking its return code and reading the result.
    outWith :: (Storable a) => (Ptr a -> IO CInt) -> IO a
    outWith act = alloca \out -> do
      throwOnError ctx =<< act out
      peek out

-- | Read and copy the first failure from the engine's run result, if any.
readPrimaryFailure :: Ptr HegelContext -> Ptr HegelRunResult -> IO (Maybe Failure)
readPrimaryFailure ctx res = do
  count <- alloca \out -> do
    throwOnError ctx =<< hegel_run_result_failure_count ctx res out
    peek out
  if (count :: CSize) == 0
    then pure Nothing
    else bracket
      ( alloca \out -> do
          throwOnError ctx =<< hegel_run_result_failure ctx res 0 out
          peek out
      )
      (void . hegel_failure_free ctx)
      \f ->
        if f == nullPtr
          then pure Nothing
          else do
            org <- alloca \out -> do
              throwOnError ctx =<< hegel_failure_origin ctx f out
              peekUtf8 =<< peek out
            blob <- failureReproductionBlob ctx f
            pure (Just Failure {origin = org, reproductionBlob = blob})

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

-- * Counterexample reconstruction

-- | Replay a reproduction blob through the 'Property' to harvest its journal.
--
-- The failure is expected to recur; its notes become the counterexample
-- description, and its exception supplies the message and source location
-- (via 'failureDetails').
--
-- A replay that passes, discards, or runs out of choices did not reproduce the
-- engine's failure and will be reported as an unexpected divergence.
reconstructProperty :: Ptr HegelContext -> Property () -> Ptr HegelSettings -> Int -> ByteString -> IO Result
reconstructProperty ctx prop s cloneDepthLimit blob =
  withTestCaseFromBlob ctx s blob \tcPtr -> do
    recording <- Tick.newRecording
    tc <- mkTestCase recording Handle {ctx, ptr = tcPtr}
    (eRes, notes, events) <- observeProperty cloneDepthLimit tc prop
    pure case eRes of
      Left e
        -- A discard or budget stop during replay means the engine's failure
        -- did not recur.
        | isControlSignal e -> diverged
        | otherwise ->
            let (message, loc, diff) = failureDetails e
             in Counterexample {message, notes, events, loc, diff}
      Right () -> diverged
  where
    diverged =
      Aborted (ReplayDiverged "the engine reported a failure, but its stored example passed (or discarded) on replay")

-- * Per-case loop

driveLoop ::
  Ptr HegelContext ->
  (Finalizers -> OpenForks -> TestCase -> IO ()) ->
  Ptr HegelRun ->
  IO (Int, Int)
driveLoop ctx action run = loop 0 0
  where
    loop !nValid !nInvalid = do
      tcPtr <- alloca \out -> do
        throwOnError ctx =<< hegel_next_test_case ctx run out
        peek out
      if tcPtr == nullPtr
        then pure (nValid, nInvalid)
        else do
          status <- runTestCase ctx action tcPtr `finally` void (hegel_test_case_free ctx tcPtr)
          case status of
            Valid -> loop (nValid + 1) nInvalid
            Invalid -> loop nValid (nInvalid + 1)
            -- A failure (counted via the run result) or an overrun (a
            -- budget-exhausted shrink probe) — neither is a valid example
            -- nor an assume\/filter rejection, so it affects neither tally.
            Interesting _ -> loop nValid nInvalid
            Overrun -> loop nValid nInvalid

-- | Run one engine-produced test case: execute the per-case action against a
-- live 'TestCase', classify how it finished, and report that 'Status' to the
-- engine.
--
-- The classification covers the whole action, draw and body alike, so a
-- discard ('AssumeRejected') or budget stop ('TestStopped') raised at any
-- point is honored — this is what lets a property body interleave its own
-- draws with test logic.
--
-- The handlers only classify; 'markComplete' runs once, outside the 'catches'
-- scope, so an engine error raised while reporting (a 'HegelError') propagates
-- to 'check''s outer handler instead of being misread as a test failure.
--
-- Finalizers registered during the case ('Hegel.Property.registerFinalizer')
-- are drained under 'mask' after the run returns so a finalizer that throws
-- will result in an 'Errored' result rather than being misclassified as a
-- counterexample.
runTestCase ::
  Ptr HegelContext ->
  (Finalizers -> OpenForks -> TestCase -> IO ()) ->
  Ptr HegelTestCase ->
  IO Status
runTestCase ctx action tcPtr = do
  finalizers <- newFinalizers
  forks <- newOpenForks
  -- The case's own failure origin, if it failed before teardown ran; carried
  -- into 'FinalizerFailed' so aborting on teardown does not silently drop the
  -- fact that a counterexample was in hand.
  caseOrigin <- newIORef Nothing
  -- We must drain on /every/ exit, /except/ for the runner's own exception (a
  -- 'MalformedTest' or a 'HegelError' from 'markComplete'); that is the
  -- primary diagnostic and must win over a 'FinalizerFailed' the drain would
  -- raise.
  mask \restore -> do
    result <- try $ restore $ run finalizers forks caseOrigin
    failures <- drainFinalizers finalizers
    -- Defense in depth: 'run' already settles every fork itself, before
    -- 'markComplete'. This only does anything if 'run' escaped via a
    -- genuinely asynchronous exception before reaching that point, since
    -- 'catchAny' below absorbs every synchronous one.
    _ <- collectLeaks forks
    case result of
      -- The run threw: it takes precedence; finalizers were still drained.
      Left (e :: SomeException) -> throwIO $ NoBacktrace e
      Right status -> case failures of
        [] -> pure status
        -- A captured finalizer failure aborts the run; 'drainFinalizers'
        -- captures every finalizer exception, so nothing escapes uncaught.
        es -> do
          origin <- readIORef caseOrigin
          throwIO $ FinalizerFailed origin es
  where
    run finalizers forks caseOrigin = do
      tc <- mkTestCase Tick.Silent Handle {ctx, ptr = tcPtr}
      status <-
        -- 'catchControl' catches only Hegel's async control signals via base
        -- 'E.catches'; 'catchAny' (unliftio) then catches all remaining
        -- synchronous exceptions and marks them as failures /except/ for
        -- 'MalformedTest', which is re-thrown so 'check' can abort the run.
        (action finalizers forks tc $> Valid)
          `catchControl` \case
            Assume -> pure Invalid
            -- @libhegel@ owns the choice budget but does not observe that we
            -- stopped; report Overrun explicitly to let the engine shrink
            Stop -> pure Overrun
          `catchAny` \e -> case fromException e of
            Just malformed -> throwIO (malformed :: MalformedTest)
            Nothing -> pure . Interesting $ originOf e
      -- Must settle every fork before markComplete: one still drawing
      -- against its clone when the family completes fails with an engine
      -- error of its own, rather than the well-formed 'MalformedTest'
      -- 'closeOpenForks' produces here.
      closeOpenForks forks
      case status of
        Interesting origin -> writeIORef caseOrigin (Just origin)
        _ -> pure ()
      markComplete tc status
      pure status
