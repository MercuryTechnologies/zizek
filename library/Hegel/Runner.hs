-- | @libhegel@ property runner.
module Hegel.Runner
  ( check,
  )
where

import Control.Concurrent.Async (wait, withAsyncBound)
import Control.Exception (toException)
import Control.Exception qualified as E
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word32)
import Foreign (Ptr, Storable, alloca, nullPtr, peek)
import Foreign.C.String (withCString)
import Foreign.C.Types (CBool (..), CInt, CSize)
import Hegel.Assertion (originOf)
import Hegel.Backend (Backend (..))
import Hegel.Database (Database (..))
import Hegel.HealthCheck (HealthCheck (..))
import Hegel.Internal.Control (AssumeRejected (..), TestStopped (..), isControlSignal)
import Hegel.Internal.FFI
import Hegel.Internal.TestCase (Status (..), TestCase, markComplete, mkTestCase)
import Hegel.Phase (Phase (..))
import Hegel.Property.Internal (Property, failureDetails, observeProperty, propertyAction)
import Hegel.Report (Abort (..), Report (..), Result (..), Stats (..), aborted)
import Hegel.Settings (Settings (..))
import Hegel.Verbosity (Verbosity (..))
import UnliftIO.Exception (Handler (..), catchAny, catches, finally)

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
  withAsyncBound go wait
    `catches` [ Handler \(e :: HegelStartupError) -> pure (aborted (Errored (toException e))),
                -- A libhegel call outside the per-case try (e.g. markComplete)
                -- can fail with a HegelError; surface it as Errored rather than
                -- letting it escape the runner.
                Handler \(e :: HegelError) -> pure (aborted (Errored (toException e)))
              ]
  where
    go = withContext \ctx ->
      withSettings ctx \s -> do
        applySettings ctx settings s
        -- The run result, its failures, and their blobs are all borrowed from
        -- the run handle and only valid until hegel_run_free (called by withRun
        -- on bracket exit), so read and copy everything out before returning
        -- from the lambda.
        (nValid, nInvalid, outcome) <- withRun ctx s \run -> do
          (nv, ni) <- driveLoop ctx settings (propertyAction prop) run
          o <- readRunOutcome ctx run
          pure (nv, ni, o)
        result <- case outcome.status of
          HEGEL_RUN_STATUS_PASSED
            | nValid == 0 -> pure (GaveUp "no valid examples found")
            | otherwise -> pure Ok
          HEGEL_RUN_STATUS_FAILED -> case outcome.failure of
            Just f
              | Just blob <- f.reproductionBlob -> reconstructProperty ctx prop s blob
              | otherwise -> pure (Aborted (UnhealthyInput f.origin))
            Nothing ->
              pure (Aborted (Errored (toException (userError "run reported a failure but exposed no counterexample"))))
          -- HEGEL_RUN_STATUS_ERROR (and any unexpected status): the run itself
          -- failed (a health check, a nondeterministic test, an engine panic)
          -- and produced no verdict on the property.
          _ -> pure (Aborted (UnhealthyInput (fromMaybe "the run failed" outcome.runError)))
        pure Report {result, stats = Stats {valid = nValid, invalid = nInvalid}}

-- * Settings

-- | Map a 'Settings' value onto the corresponding @libhegel@ settings setters.
applySettings :: Ptr HegelContext -> Settings -> Ptr HegelSettings -> IO ()
applySettings ctx s ptr = do
  chk (hegel_settings_set_mode ctx ptr HEGEL_MODE_TEST_RUN)
  chk (hegel_settings_set_backend ctx ptr (backendC s.backend))
  chk (hegel_settings_set_test_cases ctx ptr (fromIntegral s.testCases))
  chk (hegel_settings_set_verbosity ctx ptr (verbosityC s.verbosity))
  case s.seed of
    Just seed -> chk (hegel_settings_set_seed ctx ptr seed (CBool 1))
    Nothing -> chk (hegel_settings_set_seed ctx ptr 0 (CBool 0))
  chk (hegel_settings_set_derandomize ctx ptr (boolC s.derandomize))
  chk (hegel_settings_set_report_multiple_failures ctx ptr (boolC s.reportMultipleFailures))
  chk (hegel_settings_set_phases ctx ptr (phasesBitmask s.phases))
  chk (hegel_settings_set_suppress_health_check ctx ptr (hcBitmask s.suppressHealthCheck))
  -- "" disables the store; skipping the call leaves the engine default
  -- (.hegel/ under the cwd).
  case s.database of
    DatabaseDefault -> pure ()
    DatabaseDisabled -> withCString "" \p -> chk (hegel_settings_set_database ctx ptr p)
    DatabaseDirectory p -> withCString p \cp -> chk (hegel_settings_set_database ctx ptr cp)
  for_ s.databaseKey \key ->
    BS.useAsCString (encodeUtf8 key) \p -> chk (hegel_settings_set_database_key ctx ptr p)
  where
    chk io = io >>= throwOnError ctx

boolC :: Bool -> CBool
boolC b = CBool (if b then 1 else 0)

backendC :: Backend -> CInt
backendC Auto = HEGEL_BACKEND_AUTO
backendC Default = HEGEL_BACKEND_DEFAULT
backendC Urandom = HEGEL_BACKEND_URANDOM

verbosityC :: Verbosity -> CInt
verbosityC Quiet = HEGEL_VERBOSITY_QUIET
verbosityC Normal = HEGEL_VERBOSITY_NORMAL
verbosityC Verbose = HEGEL_VERBOSITY_VERBOSE
verbosityC Debug = HEGEL_VERBOSITY_DEBUG

-- | OR the per-phase flags into a bitmask.
--
-- An empty list yields @0@ (no phases enabled).
phasesBitmask :: [Phase] -> Word32
phasesBitmask = foldl' (\acc p -> acc .|. phaseFlag p) 0

phaseFlag :: Phase -> Word32
phaseFlag Explicit = HEGEL_PHASE_EXPLICIT
phaseFlag Reuse = HEGEL_PHASE_REUSE
phaseFlag Generate = HEGEL_PHASE_GENERATE
phaseFlag Target = HEGEL_PHASE_TARGET
phaseFlag Shrink = HEGEL_PHASE_SHRINK

hcBitmask :: [HealthCheck] -> Word32
hcBitmask = foldl' (\acc hc -> acc .|. healthCheckFlag hc) 0

healthCheckFlag :: HealthCheck -> Word32
healthCheckFlag FilterTooMuch = HEGEL_HC_FILTER_TOO_MUCH
healthCheckFlag TooSlow = HEGEL_HC_TOO_SLOW
healthCheckFlag TestCasesTooLarge = HEGEL_HC_TEST_CASES_TOO_LARGE
healthCheckFlag LargeInitialTestCase = HEGEL_HC_LARGE_INITIAL_TEST_CASE

-- * Failures

-- | A single failure copied out of the run result.
data Failure = Failure
  { -- | Stable, draw-independent deduplication key (e.g. @\"file:line\"@).
    origin :: !Text,
    -- | Base64 reproduction blob, or 'Nothing' for failures that carry none
    -- (e.g. a health-check failure).
    reproductionBlob :: !(Maybe ByteString)
  }

-- | The aggregated verdict of a finished run, copied out of the borrowed
-- result before 'hegel_run_free' invalidates it.
data RunOutcome = RunOutcome
  { -- | One of the @HEGEL_RUN_STATUS_*@ codes.
    status :: !CInt,
    -- | The first distinct failure, when the run failed. 'Report' carries a
    -- single counterexample, so any additional distinct failures are not
    -- surfaced.
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
  status <- outWith (hegel_run_result_status ctx res)
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
    (eRes, notes) <- observeProperty (mkTestCase ctx tcPtr) prop
    pure case eRes of
      Left e
        -- A discard or budget stop during replay means the engine's failure
        -- did not recur.
        | isControlSignal e -> diverged
        | otherwise ->
            let (message, loc, diff) = failureDetails e
             in Counterexample {message, notes, loc, diff}
      Right () -> diverged
  where
    diverged =
      Aborted (Errored (toException (userError "failure reported but the property did not reproduce it on replay")))

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
            -- 'Invalid' is reserved for assume\/filter rejections; it feeds
            -- 'Stats.invalid'.
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
  run `finally` settings.perCaseFinalizer
  where
    tc = mkTestCase ctx tcPtr
    run = do
      status <-
        -- The control signals are thrown as asynchronous exceptions, so
        -- they must be caught with base 'E.catches'.
        --
        -- All synchronous exceptions are caught by `catchAny` so they can be
        -- marked as indicative of test failure.
        (action tc $> Valid)
          `E.catches` [ E.Handler \AssumeRejected -> pure Invalid,
                        -- libhegel owns the choice budget but does not observe
                        -- that we stopped, so report Overrun explicitly to let
                        -- the engine shrink.
                        E.Handler \TestStopped -> pure Overrun
                      ]
          `catchAny` \e -> pure (Interesting (originOf e))
      markComplete tc status
      pure status
