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
import Control.Exception qualified as E
import Control.Monad (unless, void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Foreign (Ptr, Storable, alloca, nullPtr, peek)
import Foreign.C.Types (CBool (..), CInt, CSize)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hegel.Assertion (originOf)
import Hegel.Database (Database (..))
import Hegel.Gen.Internal (Gen, draw)
import Hegel.Internal.Control (ControlSignal (..), FinalizerFailed (..), NoBacktrace (..), catchControl, isAborting)
import Hegel.Internal.Foreign.Raw
import Hegel.Internal.Replay qualified as Replay
import Hegel.Internal.Settings (Resolved (..), withResolvedSettings)
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
import Hegel.Report
  ( Abort (..),
    Event,
    FailureEvidence (..),
    FailureEvidenceStatus (..),
    FailureOutcome (..),
    Note,
    ReplayDivergence (..),
    ReplayReason (..),
    Report (..),
    Reproduction (..),
    Result (..),
    Stats (..),
    aborted,
  )
import Hegel.Settings (Settings (..))
import Hegel.Settings qualified as Settings
import UnliftIO.Exception (catchAny, throwIO)
import UnliftIO.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Witch qualified

-- | Run a 'Property' through @libhegel@.
check :: (HasCallStack) => Settings -> Property () -> IO Report
check = withFrozenCallStack $ checkWithProgress (\_ -> pure ())

-- | Run a property and report cumulative completed engine cases after cleanup.
-- Every case the engine runs counts, including shrink probes and the engine's
-- final replay of each failure.
checkWithProgress :: (HasCallStack) => (Int -> IO ()) -> Settings -> Property () -> IO Report
checkWithProgress progress settings prop =
  abortFramework (withAsyncBound go wait)
  where
    go = either throwIO pure (withFrozenCallStack (Settings.validate settings)) *> execute
    execute
      | Just 0 <- settings.testCases, Just DatabaseDisabled <- settings.database = pure (Report (GaveUp "no valid examples found") (Stats 0 0 0) Unstored [])
      | otherwise = withContext \ctx ->
          withResolvedSettings ctx settings \resolved s -> do
            captures <- newIORef Map.empty
            (sink, readOutput) <- newOutputBuffer
            -- Read and copy everything out of the run handle before withRun frees
            -- it on bracket exit (see 'readRunOutcome').
            (stats, outcome) <- withRun ctx s (Just sink) (driveRun ctx settings prop captures progress)
            result <- case outcome.status of
              RunPassed
                | stats.valid == 0 -> pure (GaveUp "no valid examples found")
                | otherwise -> pure Ok
              RunFailed -> case outcome.failures of
                [] -> pure noCounterexample
                failure : failures -> do
                  version <- engineVersion ctx
                  captured <- readIORef captures
                  pure (Failures (fmap (capturedOutcome resolved.printBlob version captured) (failure :| failures)))
              RunErrored -> pure (runErrored outcome)
            engineOutput <- readOutput
            pure
              Report
                { result,
                  stats,
                  reproduction = case result of
                    Failures {} -> failureReproduction outcome.failures resolved settings.databaseKey
                    _ -> Unstored,
                  engineOutput
                }

-- | Pump every case out of a started run, then copy its outcome.
driveRun :: Ptr HegelContext -> Settings -> Property () -> IORef (Map Text Capture) -> (Int -> IO ()) -> Ptr HegelRun -> IO (Stats, RunOutcome)
driveRun ctx settings prop captures progress run = do
  stats <- driveLoop ctx (\journal -> propertyAction journal (fromMaybe Settings.defaultMaxCloneDepth settings.maxCloneDepth) prop) captures run progress
  outcome <- readRunOutcome ctx run
  pure (stats, outcome)

-- | The result for a run the engine marked failed without exposing a failure.
noCounterexample :: Result
noCounterexample = Aborted (Errored (toException (userError "run reported a failure but exposed no counterexample")))

-- | The result for a run that itself failed, such as on a health check or an
-- engine panic, and produced no verdict on the property.
runErrored :: RunOutcome -> Result
runErrored outcome
  | Just msg <- outcome.runError, isUnsatisfiable msg = GaveUp msg
  | otherwise = Aborted (UnhealthyInput (fromMaybe "the run failed" outcome.runError))

-- | Whether a run error is the engine's verdict that no case satisfied the
-- property's assumptions, which this library reports as giving up.
--
-- @libhegel@ signals this only through the error text, which its own tests
-- require to contain @Unsatisfiable@.
isUnsatisfiable :: Text -> Bool
isUnsatisfiable = T.isInfixOf "Unsatisfiable"

-- | Pair an engine failure with the capture recorded for its origin, and with
-- its replay token when @printBlob@ asks for one.
capturedOutcome :: Bool -> Text -> Map Text Capture -> Failure -> FailureOutcome
capturedOutcome printBlob version captured failure =
  FailureOutcome
    { failureOrigin = failure.origin,
      failureReplayToken =
        if printBlob
          then Replay.makeReplayToken version failure.origin <$> failure.reproductionBlob
          else Nothing,
      failureCaveat = failure.caveat,
      failureEvidence = maybe Uncaptured (Captured . captureEvidence) (Map.lookup failure.origin captured)
    }

-- | Where a failing run's counterexample can be found again. A primary
-- failure without a reproduce blob has nothing to replay, and nothing is filed
-- without both a database and a key.
failureReproduction :: [Failure] -> Resolved -> Maybe Text -> Reproduction
failureReproduction failures resolved databaseKey = case (failures, databaseKey) of
  (Failure {reproductionBlob = Nothing} : _, _) -> Unreproducible
  (_, Just key) | resolved.persists -> Stored key
  _ -> Unstored

-- | Replay one failure token against a property without using the example
-- database.
--
-- The engine replays the token's blob until a replay fails, within a bounded
-- budget, so a nondeterministic failure gets several chances to recur.
replay :: (HasCallStack) => Settings -> ReplayToken -> Property () -> IO Report
replay settings token prop =
  abortFramework (withAsyncBound go wait)
  where
    expected = Replay.tokenOriginOf token
    go =
      either throwIO pure (withFrozenCallStack (Settings.validate settings)) *> withContext \ctx ->
        withResolvedSettings ctx settings {database = Just DatabaseDisabled, databaseKey = Nothing} \_ s -> do
          version <- engineVersion ctx
          if version /= Replay.tokenVersionOf token
            then pure (diverged (Stats 0 0 0) (IncompatibleVersions (Replay.tokenVersionOf token) version))
            else do
              captures <- newIORef Map.empty
              (sink, readOutput) <- newOutputBuffer
              report <-
                withBlobRun ctx s (Replay.tokenBlobOf token) (Just sink) (driveRun ctx settings prop captures (\_ -> pure ())) >>= \case
                  Left e
                    | e.code == HEGEL_E_INVALID_ARG ->
                        pure (diverged (Stats 0 0 0) (InvalidReplayBlob (fromMaybe "invalid replay blob" e.message)))
                    | otherwise -> throwIO e
                  Right (stats, outcome) -> do
                    captured <- readIORef captures
                    pure case outcome.status of
                      RunPassed -> diverged stats DidNotReproduce
                      RunFailed -> case outcome.failures of
                        [] -> Report noCounterexample stats Unstored []
                        failure : failures -> Report (Failures (replayedOutcomes captured (failure :| failures))) stats Unstored []
                      RunErrored -> Report (runErrored outcome) stats Unstored []
              engineOutput <- readOutput
              pure report {engineOutput}

    diverged stats reason =
      Report (Failures (FailureOutcome expected (Just token) Nothing (Diverged (ReplayDivergence reason)) :| [])) stats Unstored []

    -- The engine attaches no blob to a replay's failure, since the token
    -- already holds it. When no failure has the token's origin, a divergence
    -- leads the report and every actual failure follows with its own evidence.
    replayedOutcomes captured failures
      | any ((== expected) . (.origin)) failures = fmap (actualOutcome captured) failures
      | failure :| _ <- failures =
          FailureOutcome expected (Just token) Nothing (Diverged (ReplayDivergence (ChangedOrigin failure.origin)))
            NE.<| fmap (actualOutcome captured) failures

    actualOutcome captured failure =
      FailureOutcome
        { failureOrigin = failure.origin,
          failureReplayToken = if failure.origin == expected then Just token else Nothing,
          failureCaveat = failure.caveat,
          failureEvidence = maybe Uncaptured (Captured . captureEvidence) (Map.lookup failure.origin captured)
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
-- A shrink probe or the engine's final replay of a failure then draws a
-- different value than the live case did. The enclosing run treats this as
-- nondeterminism, as its 'Hegel.Settings.nondeterminism' setting directs,
-- once it replays that case's prefix.
sample :: (HasCallStack) => Settings -> Gen a -> IO a
sample settings gen =
  withAsyncBound go wait
  where
    go =
      either throwIO pure (withFrozenCallStack (Settings.validate settings)) *> withContext \ctx ->
        withResolvedSettings ctx (sampling 1 settings) \_ s ->
          withRun ctx s Nothing (drawOneCase ctx gen)

-- | Settings for a 'sample' or 'samples' run of @n@ generated cases, which
-- persists nothing and draws from a fresh seed per call unless @settings@
-- derandomizes.
sampling :: Int -> Settings -> Settings
sampling n settings =
  settings
    { testCases = Just n,
      phases = Just [Generate],
      database = Just DatabaseDisabled,
      databaseKey = Nothing,
      derandomize = Just (fromMaybe False settings.derandomize)
    }

-- | Pull the first test case a one-case run offers, draw @gen@
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
    go = either throwIO pure (withFrozenCallStack (Settings.validate settings {testCases = Just n})) *> execute
    execute
      | n == 0 = pure []
      | otherwise = withContext \ctx ->
          withResolvedSettings ctx (sampling n settings) \_ s -> do
            acc <- newIORef []
            outcome <- withRun ctx s Nothing \run -> do
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

-- * Failures

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

-- | An engine failure copied before its handle is freed.
data Failure = Failure
  { origin :: !Text,
    reproductionBlob :: !(Maybe ByteString),
    caveat :: !(Maybe Text)
  }

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
      then pure Failure {origin = "<missing failure>", reproductionBlob = Nothing, caveat = Nothing}
      else do
        org <- alloca \out -> do
          throwOnError ctx =<< hegel_failure_origin ctx f out
          peekUtf8 =<< peek out
        blob <- failureReproductionBlob ctx f
        caveat <- failureCaveat ctx f
        pure Failure {origin = org, reproductionBlob = blob, caveat}

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

-- * Captures

-- | What a failing case left behind for its origin's report.
data Capture = Capture
  { -- | Whether the engine stamped the case for capture, which makes its
    -- journal and pool events part of the evidence.
    stamped :: !Bool,
    failure :: !SomeException,
    notes :: ![Note],
    events :: ![Event]
  }

-- | Record a failing case's capture under its origin.
--
-- A stamped capture always replaces an unstamped one. Between two captures of
-- the same kind the newer wins, so the engine's final replay of a failure,
-- which runs after every other case with its origin, supplies the report.
recordCapture :: IORef (Map Text Capture) -> Text -> Capture -> IO ()
recordCapture captures origin capture = modifyIORef' captures (Map.alter keep origin)
  where
    keep = \case
      Just prior | prior.stamped, not capture.stamped -> Just prior
      _ -> Just capture

captureEvidence :: Capture -> FailureEvidence
captureEvidence capture =
  let (message, loc, diff) = failureDetails capture.failure
   in FailureEvidence {message, notes = capture.notes, events = capture.events, loc, diff}

-- * Per-case loop

driveLoop ::
  Ptr HegelContext ->
  (Journal -> Finalizers -> OpenForks -> TestCase -> IO ()) ->
  IORef (Map Text Capture) ->
  Ptr HegelRun ->
  (Int -> IO ()) ->
  IO Stats
driveLoop ctx action captures run progress = loop (Stats 0 0 0) 0
  where
    loop !stats !completed = do
      tcPtr <- alloca \out -> do
        throwOnError ctx =<< hegel_next_test_case ctx run out
        peek out
      if tcPtr == nullPtr
        then pure stats
        else do
          status <- runTestCase ctx action captures tcPtr `finally` void (hegel_test_case_free ctx tcPtr)
          progress (completed + 1)
          case status of
            Valid -> loop stats {valid = stats.valid + 1} (completed + 1)
            Invalid -> loop stats {invalid = stats.invalid + 1} (completed + 1)
            Interesting _ -> loop stats {failing = stats.failing + 1} (completed + 1)
            Overrun -> loop stats (completed + 1)

-- | Run one engine-produced test case, recording a capture when it fails.
--
-- A case the engine stamped for capture runs with a recording journal and
-- pool-event stream; every other case runs silently.
runTestCase ::
  Ptr HegelContext ->
  (Journal -> Finalizers -> OpenForks -> TestCase -> IO ()) ->
  IORef (Map Text Capture) ->
  Ptr HegelTestCase ->
  IO Status
runTestCase ctx action captures tcPtr = do
  stamped <- alloca \out -> do
    throwOnError ctx =<< hegel_test_case_should_capture ctx tcPtr out
    (/= CBool 0) <$> peek out
  (recording, journal, drainNotes) <-
    if stamped
      then do
        (journal, drainNotes) <- newRecordingJournal
        recording <- Tick.newRecording
        pure (recording, journal, drainNotes)
      else pure (Tick.Silent, Silent, pure [])
  tc <- mkTestCase recording Handle {ctx, ptr = tcPtr}
  finalizers <- newFinalizers
  forks <- newOpenForks
  -- The body exception is retained separately from the engine's deduplication
  -- origin so a teardown abort still explains the original failure.
  caseFailure <- newIORef Nothing
  -- Every exit drains cleanup, retaining a runner error alongside any
  -- cleanup diagnostics.
  mask \restore -> do
    result <- try $ restore $ run tc journal finalizers forks caseFailure
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
        -- Base 'E.throwIO', because unliftio's would wrap an asynchronous
        -- exception such as a cancellation as a synchronous one, and callers
        -- could no longer catch it as what it is.
        | otherwise -> E.throwIO $ NoBacktrace e
      Right status -> case failures of
        [] -> do
          case status of
            Interesting origin ->
              readIORef caseFailure >>= traverse_ \failure -> do
                notes <- drainNotes
                events <- Tick.drain tc.events
                recordCapture captures origin Capture {stamped, failure, notes, events}
            _ -> pure ()
          pure status
        -- A captured finalizer failure aborts the run; 'drainFinalizers'
        -- captures every finalizer exception, so nothing escapes uncaught.
        es -> do
          bodyFailure <- readIORef caseFailure
          throwIO $ FinalizerFailed bodyFailure es
  where
    run tc journal finalizers forks caseFailure = do
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
              pure . Interesting $ originOf e
      -- Must settle every fork before markComplete: one still drawing
      -- against its clone when the family completes fails with an engine
      -- error of its own, rather than the well-formed 'MalformedTest'
      -- 'closeOpenForks' produces here.
      closeOpenForks forks
      markComplete tc status
      pure status

-- | Report framework aborts while preserving unrelated exceptions and cancellation.
abortFramework :: IO Report -> IO Report
abortFramework action =
  action `catchAny` \e ->
    if isAborting e
      then pure (aborted (Errored e))
      else throwIO (NoBacktrace e)

-- | A sink collecting a run's engine output, and an action reading back every
-- line it received so far in order.
newOutputBuffer :: IO (OutputSink, IO [Text])
newOutputBuffer = do
  buffer <- newIORef []
  let sink _ ptr len = do
        bytes <- BS.packCStringLen (ptr, fromIntegral len)
        modifyIORef' buffer (TE.decodeUtf8Lenient bytes :)
  pure (sink, reverse <$> readIORef buffer)
