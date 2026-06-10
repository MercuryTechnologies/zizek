-- | Native @libhegel@ property runner.
module Hegel.Runner
  ( runProperty,
    runPropertyWith,
    check,
  )
where

import Control.Concurrent.Async (wait, withAsyncBound)
import Control.Exception (SomeException, fromException, toException)
import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word32)
import Foreign (Ptr, nullPtr)
import Foreign.C.String (withCString)
import Foreign.C.Types (CBool (..))
import Hegel.Assertion (originOf)
import Hegel.Database (Database (..))
import Hegel.FFI
import Hegel.Gen.Internal (Gen)
import Hegel.HealthCheck (HealthCheck (..))
import Hegel.Phase (Phase (..))
import Hegel.Property.Internal (Property, failureDetails, forAllWith, observeProperty, propertyAction)
import Hegel.Report (Abort (..), Report (..), Result (..), Stats (..), aborted, renderValue)
import Hegel.Settings (Settings (..))
import Hegel.TestCase (AssumeRejected (..), Status (..), TestCase, TestStopped (..), markComplete, mkReplayTestCase, mkTestCase)
import UnliftIO.Exception (Handler (..), catches, finally)

-- | Run a generator-plus-body property: 'runPropertyWith' rendering drawn
-- values via 'renderValue'.
runProperty ::
  forall a.
  (Show a) =>
  Settings ->
  Gen a ->
  (a -> IO ()) ->
  IO Report
runProperty = runPropertyWith renderValue

-- | 'runProperty' with an explicit renderer, for values without a 'Show'
-- instance (or with an unhelpful one): sugar for 'check' over
-- @'forAllWith' render gen '>>=' 'liftIO' . body@.
runPropertyWith ::
  forall a.
  (a -> Text) ->
  Settings ->
  Gen a ->
  (a -> IO ()) ->
  IO Report
runPropertyWith render settings gen body =
  check settings (forAllWith render gen >>= liftIO . body)

-- | Run a 'Property' using the native @libhegel@ backend.
--
-- The engine's run result is the authority for the verdict: a failure with
-- a reproduction blob is replayed through the property to describe the
-- counterexample ('reconstructProperty'); one without is a health-check
-- abort; otherwise the tally decides between 'GaveUp' and 'Ok'.
check :: Settings -> Property () -> IO Report
check settings prop =
  -- 'withAsyncBound' rather than 'runInBoundThread': both fork a bound OS
  -- thread (required because 'hegel_last_error_message' is thread-local and
  -- 'throwOnError' must read it on the same OS thread that made the failing
  -- call), but 'withAsyncBound' cancels the worker when the caller is
  -- interrupted. An async exception to the 'check' caller is forwarded into
  -- 'go', which unwinds the loop and runs 'hegel_run_free' promptly (it
  -- drains any in-flight case, sets the abort flag, and joins the worker --
  -- see refs/hegel-rust.md). 'runInBoundThread' has no such cancellation
  -- path and would orphan the worker.
  --
  -- Note: 'safe' blocking FFI calls (notably 'hegel_next_test_case') cannot
  -- be interrupted by async exceptions; a cancellation signal is deferred
  -- until that call returns. Teardown latency is therefore bounded by one
  -- engine step. A cancellation hook in libhegel (e.g.
  -- 'hegel_run_request_abort') is the only way to remove this bound; tracked
  -- as a follow-up.
  withAsyncBound go wait
    `catches` [ Handler \(e :: HegelStartupError) -> pure (aborted (Errored (toException e))),
                -- A libhegel call outside the per-case try (e.g. markComplete)
                -- can fail with a HegelError; surface it as Errored rather than
                -- letting it escape the runner.
                Handler \(e :: HegelError) -> pure (aborted (Errored (toException e)))
              ]
  where
    go = withSettings \s -> do
      applySettings settings s
      (nValid, nInvalid, mFailure) <- withRun s \run -> do
        (nv, ni) <- driveLoop settings (propertyAction prop) run
        -- The failure record is borrowed from the run handle and only valid
        -- until hegel_run_free (called by withRun on bracket exit), so read
        -- and copy it out before returning from the lambda.
        f <- readPrimaryFailure run
        pure (nv, ni, f)
      result <- case mFailure of
        Just f
          | Just blob <- f.reproductionBlob -> reconstructProperty prop s blob (failureMessage f)
          | otherwise -> pure (Aborted (UnhealthyInput (failureMessage f)))
        Nothing
          | nValid == 0 -> pure (GaveUp "no valid examples found")
          | otherwise -> pure Ok
      pure Report {result, stats = Stats {valid = nValid, invalid = nInvalid}}

-- * Settings

-- | Map a 'Settings' value onto the corresponding @libhegel@ settings setters.
applySettings :: Settings -> Ptr HegelSettings -> IO ()
applySettings s ptr = do
  hegel_settings_mode ptr HEGEL_MODE_TEST_RUN
  hegel_settings_test_cases ptr (fromIntegral s.testCases)
  hegel_settings_verbosity ptr HEGEL_VERBOSITY_QUIET
  case s.seed of
    Just seed -> hegel_settings_seed ptr seed (CBool 1)
    Nothing -> hegel_settings_seed ptr 0 (CBool 0)
  hegel_settings_derandomize ptr (boolC s.derandomize)
  hegel_settings_report_multiple_failures ptr (boolC s.reportMultipleFailures)
  hegel_settings_phases ptr (phasesBitmask s.phases)
  hegel_settings_suppress_health_check ptr (hcBitmask s.suppressHealthCheck)
  -- "" disables the store; skipping the call leaves the engine default
  -- (.hegel/ under the cwd).
  case s.database of
    DatabaseDefault -> pure ()
    DatabaseDisabled -> withCString "" (hegel_settings_database ptr)
    DatabaseDirectory p -> withCString p (hegel_settings_database ptr)
  for_ s.databaseKey \key ->
    BS.useAsCString (encodeUtf8 key) (hegel_settings_database_key ptr)

boolC :: Bool -> CBool
boolC b = CBool (if b then 1 else 0)

-- | OR the per-phase flags into a bitmask. An empty list yields @0@ (no phases
-- enabled).
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
  { -- | Stable, draw-independent dedup key (e.g. @\"file:line\"@).
    origin :: !Text,
    -- | Engine diagnostic, if any.
    diagnostic :: !Text,
    -- | Base64 reproduction blob, or 'Nothing' for failures that carry none
    -- (e.g. a health-check failure).
    reproductionBlob :: !(Maybe ByteString)
  }

-- | Read and copy the first failure from the engine's run result, if any.
-- 'Report' carries a single counterexample, so any additional distinct
-- failures are not surfaced. Must be called before 'hegel_run_free' frees the
-- borrowed result.
readPrimaryFailure :: Ptr HegelRun -> IO (Maybe Failure)
readPrimaryFailure run = do
  res <- hegel_run_result run
  count <- fromIntegral <$> hegel_run_result_failure_count res
  if count <= (0 :: Int)
    then pure Nothing
    else do
      f <- hegel_run_result_failure res 0
      if f == nullPtr
        then pure Nothing
        else do
          org <- peekUtf8 =<< hegel_failure_origin f
          diag <- peekUtf8 =<< hegel_failure_diagnostic f
          blob <- failureReproductionBlob f
          pure (Just Failure {origin = org, diagnostic = diag, reproductionBlob = blob})

-- * Counterexample reconstruction

-- | Prefer the engine's diagnostic; fall back to the (stable) origin string.
failureMessage :: Failure -> Text
failureMessage f
  | T.null f.diagnostic = f.origin
  | otherwise = f.diagnostic

-- | Replay a reproduction blob through the property to harvest its journal.
--
-- The replay handle is caller-owned and standalone, so 'mkReplayTestCase'
-- gives it a no-op 'markComplete' (calling @hegel_mark_complete@ on it would
-- abort the process).
--
-- The failure is expected to recur; its notes become the counterexample
-- description, and its exception supplies the message and source location
-- (via 'failureDetails'). A replay that passes, discards, or runs out of
-- choices did not reproduce the engine's failure — surface the mismatch
-- rather than fabricating a report from a non-failing run.
reconstructProperty :: Property () -> Ptr HegelSettings -> ByteString -> Text -> IO Result
reconstructProperty prop s blob msg =
  withTestCaseFromBlob s blob \tcPtr -> do
    (eRes, notes) <- observeProperty (mkReplayTestCase tcPtr) prop
    pure case eRes of
      Left e
        | isDivergence e -> diverged
        | otherwise ->
            let (message, loc, diff) = failureDetails msg e
             in Counterexample {message, notes, loc, diff}
      Right () -> diverged
  where
    diverged =
      Aborted (Errored (toException (userError "failure reported but the property did not reproduce it on replay")))
    isDivergence e =
      isJust (fromException @AssumeRejected e) || isJust (fromException @TestStopped e)

-- * Per-case loop

driveLoop ::
  Settings ->
  (TestCase -> IO ()) ->
  Ptr HegelRun ->
  IO (Int, Int)
driveLoop settings action run = loop 0 0
  where
    loop !nValid !nInvalid = do
      tcPtr <- hegel_next_test_case run
      if tcPtr == nullPtr
        then pure (nValid, nInvalid)
        else
          runCase settings action tcPtr >>= \case
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
-- to 'runProperty''s outer handler instead of being misread as a test failure.
runCase ::
  Settings ->
  (TestCase -> IO ()) ->
  Ptr HegelTestCase ->
  IO Status
runCase settings action tcPtr =
  run `finally` settings.perCaseFinalizer
  where
    tc = mkTestCase tcPtr
    run = do
      status <-
        (action tc $> Valid)
          `catches` [ Handler \AssumeRejected -> pure Invalid,
                      -- libhegel owns the choice budget but does not observe
                      -- that we stopped, so report Overrun explicitly to let
                      -- the engine shrink.
                      Handler \TestStopped -> pure Overrun,
                      Handler \(e :: SomeException) -> pure (Interesting (originOf e))
                    ]
      markComplete tc status
      pure status
