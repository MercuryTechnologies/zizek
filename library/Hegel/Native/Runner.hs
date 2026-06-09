-- | Native @libhegel@ property runner.
module Hegel.Native.Runner
  ( runProperty,
  )
where

import Control.Concurrent (runInBoundThread)
import Control.Exception (SomeException, toException)
import Data.ByteString (ByteString)
import Data.Functor (($>))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Foreign (Ptr, nullPtr)
import Hegel.Assertion (originOf)
import Hegel.Gen.Internal (AssumeRejected (..), Gen, draw)
import Hegel.Native.FFI
import Hegel.Native.Settings (applySettings)
import Hegel.Native.TestCase (mkReplayTestCase, mkTestCase)
import Hegel.Outcome (Outcome (..), Stats (..))
import Hegel.Settings (Settings (..))
import Hegel.TestCase (Status (..), TestStopped (..), markComplete)
import UnliftIO.Exception (Handler (..), catches, finally, tryAny)

-- | Run a property using the native @libhegel@ backend.
--
-- The engine's run result is the authority for the verdict: a genuine failure
-- carries a reproduction blob, a health-check abort does not. The per-case
-- loop only drives generation and tallies valid\/invalid cases; the
-- counterexample /value/ is reconstructed afterwards by replaying the blob,
-- since only the Haskell side can rebuild the typed value from the engine's
-- choice sequence.
runProperty ::
  forall a.
  Settings ->
  Gen a ->
  (a -> IO ()) ->
  IO (Outcome a)
runProperty settings gen body =
  runInBoundThread go
    `catches` [ Handler \(e :: HegelStartupError) -> pure (Errored (toException e)),
                -- A libhegel call outside the per-case try (e.g. markComplete)
                -- can fail with a HegelError; surface it as Errored rather than
                -- letting it escape the runner.
                Handler \(e :: HegelError) -> pure (Errored (toException e))
              ]
  where
    go = withSettings \s -> do
      applySettings settings s
      (nValid, nInvalid, mFailure) <- withRun s \run -> do
        (nv, ni) <- driveLoop settings gen body run
        -- The failure record is borrowed from the run handle and only valid
        -- until hegel_run_free (called by withRun on bracket exit), so read
        -- and copy it out before returning from the lambda.
        f <- readPrimaryFailure run
        pure (nv, ni, f)
      deriveOutcome s gen nValid nInvalid mFailure

-- ---------------------------------------------------------------------------
-- Failures
-- ---------------------------------------------------------------------------

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
-- 'Outcome' carries a single counterexample, so any additional distinct
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

-- ---------------------------------------------------------------------------
-- Outcome
-- ---------------------------------------------------------------------------

-- | Derive the 'Outcome' from the engine's run result. A failure carrying a
-- reproduction blob is replayed to rebuild the typed counterexample; a failure
-- without one is a health-check abort. With no failure, the per-case tally
-- decides between 'Rejected' (nothing valid) and 'Passed'.
deriveOutcome ::
  Ptr HegelSettings ->
  Gen a ->
  Int ->
  Int ->
  Maybe Failure ->
  IO (Outcome a)
deriveOutcome s gen nValid nInvalid = \case
  Just f
    | Just blob <- f.reproductionBlob -> reconstruct s gen blob (failureMessage f)
    | otherwise -> pure (UnhealthyInput (failureMessage f))
  Nothing
    | nValid == 0 -> pure (Rejected "no valid examples found")
    | otherwise -> pure (Passed Stats {testsRun = nValid, invalid = nInvalid})

-- | Prefer the engine's diagnostic; fall back to the (stable) origin string.
failureMessage :: Failure -> Text
failureMessage f
  | T.null f.diagnostic = f.origin
  | otherwise = f.diagnostic

-- | Replay a reproduction blob to rebuild the typed counterexample.
--
-- The replay handle is caller-owned and standalone, so 'mkReplayTestCase'
-- gives it a no-op 'markComplete' (calling @hegel_mark_complete@ on it would
-- abort the process). If the blob no longer matches the generators the draw
-- throws, which we surface as 'Errored' since there is no value to report.
reconstruct :: Ptr HegelSettings -> Gen a -> ByteString -> Text -> IO (Outcome a)
reconstruct s gen blob msg =
  withTestCaseFromBlob s blob \tcPtr -> do
    let tc = mkReplayTestCase tcPtr
    eVal <- tryAny (draw tc gen)
    pure $ case eVal of
      Right val -> Failed {counterexample = val, message = msg, notes = []}
      Left e -> Errored e

-- ---------------------------------------------------------------------------
-- Per-case loop
-- ---------------------------------------------------------------------------

driveLoop ::
  Settings ->
  Gen a ->
  (a -> IO ()) ->
  Ptr HegelRun ->
  IO (Int, Int)
driveLoop settings gen body run = do
  nValidRef <- newIORef (0 :: Int)
  nInvalidRef <- newIORef (0 :: Int)
  let loop = do
        tcPtr <- hegel_next_test_case run
        if tcPtr == nullPtr
          then pure ()
          else do
            result <- runCase settings gen body tcPtr
            case result of
              CaseValid -> modifyIORef' nValidRef (+ 1)
              CaseInvalid -> modifyIORef' nInvalidRef (+ 1)
              -- A failure (counted via the run result) or an overrun (a
              -- budget-exhausted shrink probe) — neither is a valid example
              -- nor an assume\/filter rejection, so it affects neither tally.
              CaseInteresting -> pure ()
              CaseOverrun -> pure ()
            loop
  loop
  (,) <$> readIORef nValidRef <*> readIORef nInvalidRef

-- | Outcome of running one engine-produced test case. 'CaseInvalid' is
-- reserved for assume\/filter rejections (it feeds 'Stats.invalid');
-- 'CaseOverrun' is a separate budget-exhaustion signal that is /not/ counted
-- as invalid.
data CaseResult = CaseValid | CaseInvalid | CaseInteresting | CaseOverrun

runCase ::
  Settings ->
  Gen a ->
  (a -> IO ()) ->
  Ptr HegelTestCase ->
  IO CaseResult
runCase settings gen body tcPtr =
  run `finally` settings.perCaseFinalizer
  where
    tc = mkTestCase tcPtr
    run = do
      eVal <-
        (Right <$> draw tc gen)
          `catches` [ Handler \AssumeRejected ->
                        markComplete tc Invalid $> Left CaseInvalid,
                      -- libhegel owns the choice budget but does not observe
                      -- that we stopped, so report Overrun explicitly to let
                      -- the engine shrink.
                      Handler \TestStopped ->
                        markComplete tc Overrun $> Left CaseOverrun,
                      Handler \(e :: SomeException) ->
                        markComplete tc (Interesting (originOf e)) $> Left CaseInteresting
                    ]
      case eVal of
        Left r -> pure r
        Right val -> do
          eRes <- tryAny (body val)
          case eRes of
            Right () -> markComplete tc Valid $> CaseValid
            Left exc -> markComplete tc (Interesting (originOf exc)) $> CaseInteresting
