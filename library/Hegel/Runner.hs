-- | @libhegel@ property runner.
module Hegel.Runner
  ( check,
  )
where

import Control.Concurrent.Async (wait, withAsyncBound)
import Control.Exception (fromException, toException)
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
import Hegel.HealthCheck (HealthCheck)
import Hegel.Internal.CString qualified as CString
import Hegel.Internal.Control (ControlSignal (..), MalformedTest, catchControl, isControlSignal)
import Hegel.Internal.FFI
import Hegel.Internal.TestCase (Handle (..), Status (..), TestCase, markComplete, mkTestCase)
import Hegel.Internal.Tick qualified as Tick
import Hegel.Phase (Phase)
import Hegel.Property.Internal (Property, failureDetails, observeProperty, propertyAction)
import Hegel.Report (Abort (..), Report (..), Result (..), Stats (..), aborted)
import Hegel.Settings (Finalizer (..), Settings (..))
import UnliftIO.Exception (catch, catchAny, finally, throwIO)
import Witch qualified

-- | Run a 'Property' through @libhegel@.
--
-- The engine's run result is the authority for the verdict: a failure with
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
    `catch` (\(e :: MalformedTest) -> pure (aborted (Errored (toException e))))
    `catch` (\(e :: HegelError) -> pure (aborted (Errored (toException e))))
  where
    go = withContext \ctx ->
      withSettings ctx \s -> do
        applySettings ctx settings s
        -- Read and copy everything out of the run handle before withRun frees
        -- it on bracket exit (see 'readRunOutcome').
        (nValid, nInvalid, outcome) <- withRun ctx s \run -> do
          (nv, ni) <- driveLoop ctx settings (propertyAction prop) run
          o <- readRunOutcome ctx run
          pure (nv, ni, o)
        result <- case outcome.status of
          RunPassed
            | nValid == 0 -> pure (GaveUp "no valid examples found")
            | otherwise -> pure Ok
          RunFailed -> case outcome.failure of
            Just f
              | Just blob <- f.reproductionBlob -> reconstructProperty ctx prop s blob
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

-- * Settings

-- | Map a 'Settings' value onto the corresponding @libhegel@ settings setters.
applySettings :: Ptr HegelContext -> Settings -> Ptr HegelSettings -> IO ()
applySettings ctx s ptr = do
  chk $ hegel_settings_set_mode ctx ptr HEGEL_MODE_TEST_RUN
  chk $ hegel_settings_set_backend ctx ptr (Witch.into @CInt s.backend)
  chk $ hegel_settings_set_test_cases ctx ptr (fromIntegral s.testCases)
  chk $ hegel_settings_set_verbosity ctx ptr (Witch.into @CInt s.verbosity)

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

-- | Decode the @hegel_run_status_t@ wire code; an unrecognised code is treated
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
--
-- Must be called before 'hegel_run_free' frees the borrowed result.
readRunOutcome :: Ptr HegelContext -> Ptr HegelRun -> IO RunOutcome
readRunOutcome ctx run = do
  res <- outWith (hegel_run_result ctx run)
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
    else do
      f <- alloca \out -> do
        throwOnError ctx =<< hegel_run_result_failure ctx res 0 out
        peek out
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
reconstructProperty :: Ptr HegelContext -> Property () -> Ptr HegelSettings -> ByteString -> IO Result
reconstructProperty ctx prop s blob =
  withTestCaseFromBlob ctx s blob \tcPtr -> do
    recording <- Tick.newRecording
    tc <- mkTestCase recording Handle {ctx, ptr = tcPtr}
    (eRes, notes, events) <- observeProperty tc prop
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
  Settings ->
  (TestCase -> IO ()) ->
  Ptr HegelRun ->
  IO (Int, Int)
driveLoop ctx settings action run = loop 0 0
  where
    loop !nValid !nInvalid = do
      tcPtr <- alloca \out -> do
        throwOnError ctx =<< hegel_next_test_case ctx run out
        peek out
      if tcPtr == nullPtr
        then pure (nValid, nInvalid)
        else
          runTestCase ctx settings action tcPtr >>= \case
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
-- point is honoured — this is what lets a property body interleave its own
-- draws with test logic.
--
-- The handlers only classify; 'markComplete' runs once, outside the 'catches'
-- scope, so an engine error raised while reporting (a 'HegelError') propagates
-- to 'check''s outer handler instead of being misread as a test failure.
runTestCase ::
  Ptr HegelContext ->
  Settings ->
  (TestCase -> IO ()) ->
  Ptr HegelTestCase ->
  IO Status
runTestCase ctx settings action tcPtr =
  run `finally` finalizer
  where
    Finalizer finalizer = settings.perCaseFinalizer
    run = do
      tc <- mkTestCase Tick.Silent Handle {ctx, ptr = tcPtr}
      status <-
        -- 'catchControl' catches only Hegel's async control signals via base
        -- 'E.catches'; 'catchAny' (unliftio) then catches all remaining
        -- synchronous exceptions and marks them as failures /except/ for
        -- 'MalformedTest', which is re-thrown so 'check' can abort the run.
        (action tc $> Valid)
          `catchControl` \case
            Assume -> pure Invalid
            -- @libhegel@ owns the choice budget but does not observe that we
            -- stopped; report Overrun explicitly to let the engine shrink
            Stop -> pure Overrun
          `catchAny` \e -> case fromException e of
            Just malformed -> throwIO (malformed :: MalformedTest)
            Nothing -> pure . Interesting $ originOf e
      markComplete tc status
      pure status
