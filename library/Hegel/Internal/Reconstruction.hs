-- | Reconstruct copied engine failures and account for replay execution.
module Hegel.Internal.Reconstruction
  ( Failure (..),
    ReplayResult (..),
    reconstructFailures,
    replayOne,
  )
where

import Control.Exception (SomeException, bracket, displayException, fromException)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Foreign (Ptr)
import Hegel.Assertion (originOf)
import Hegel.Internal.Control (AssumeRejected, isAborting, isControlSignal)
import Hegel.Internal.Foreign.Raw
import Hegel.Internal.Replay (ReplayToken)
import Hegel.Internal.Replay qualified as Replay
import Hegel.Internal.TestCase (Handle (..), mkTestCase)
import Hegel.Internal.Tick qualified as Tick
import Hegel.Property.Internal (Property, failureDetails, observeProperty)
import Hegel.Report
import UnliftIO.Exception (catch)

-- | An engine failure copied before its handle is freed.
data Failure = Failure
  { origin :: !Text,
    reproductionBlob :: !(Maybe ByteString)
  }

-- | One replay's diagnostic and execution accounting.
data ReplayResult = ReplayResult
  { outcome :: !FailureOutcome,
    accounting :: !ReplayStats
  }

reconstructFailures :: Ptr HegelContext -> Property () -> Ptr HegelSettings -> Int -> Text -> NonEmpty Failure -> IO (NonEmpty FailureOutcome)
reconstructFailures ctx prop settings depth version = go
  where
    go (failure :| rest) = do
      result <- case tokenFor failure of
        Nothing -> pure (notRun failure.origin Nothing MissingReplayData)
        Just token -> replayOne ctx settings depth version token prop
      later <- case rest of
        [] -> pure []
        next : remaining
          | null result.outcome.cleanupDiagnostics,
            not (reconstructionAborted result.outcome) -> do
              first :| others <- go (next :| remaining)
              pure (first : others)
          | otherwise -> pure (map (skip (if null result.outcome.cleanupDiagnostics then SkippedAfterReconstructionAbort else SkippedAfterCleanupFailure)) rest)
      pure (result.outcome :| later)
    tokenFor failure = Replay.makeReplayToken version failure.origin <$> failure.reproductionBlob
    skip reason failure = FailureOutcome failure.origin (tokenFor failure) (Skipped reason) []
    reconstructionAborted :: FailureOutcome -> Bool
    reconstructionAborted outcome = case outcome.failureEvidence of
      Diverged (ReplayDivergence (ReconstructionAborted _)) -> True
      _ -> False

notRun :: Text -> Maybe ReplayToken -> ReplayReason -> ReplayResult
notRun origin token reason =
  ReplayResult (FailureOutcome origin token (Diverged (ReplayDivergence reason)) []) (ReplayStats 0 0 0 0 0)

replayOne :: Ptr HegelContext -> Ptr HegelSettings -> Int -> Text -> ReplayToken -> Property () -> IO ReplayResult
replayOne ctx settings depth version token prop
  | version /= Replay.tokenVersionOf token =
      pure (notRun expected (Just token) (IncompatibleVersions (Replay.tokenVersionOf token) version))
  | otherwise = bracket acquire release \case
      Left reason -> pure (notRun expected (Just token) reason)
      Right ptr -> do
        recording <- Tick.newRecording
        tc <- mkTestCase recording Handle {ctx, ptr}
        (body, notes, events, cleanup) <- observeProperty depth tc prop
        let (status, accounting) = classify body notes events
            outcome = FailureOutcome expected (Just token) status (map (CleanupDiagnostic . T.pack . displayException) cleanup)
        pure ReplayResult {outcome, accounting}
  where
    expected = Replay.tokenOriginOf token
    acquire =
      (Right <$> testCaseFromBlob ctx settings (Replay.tokenBlobOf token))
        `catch` \(e :: HegelError) -> pure (Left (InvalidReplayBlob (fromMaybe "invalid replay blob" e.message)))
    release = either (const (pure ())) (void . hegel_test_case_free ctx)

    classify :: Either SomeException () -> [Note] -> [Event] -> (FailureEvidenceStatus, ReplayStats)
    classify body notes events = case body of
      Right () -> diverged UnexpectedSuccess (ReplayStats 1 1 0 0 0)
      Left exception
        | Just (_ :: AssumeRejected) <- fromException exception -> diverged UnexpectedDiscard (ReplayStats 1 0 1 0 0)
        | isControlSignal exception -> diverged ExhaustedChoices (ReplayStats 1 0 0 1 0)
        | isAborting exception -> diverged (ReconstructionAborted (T.pack (displayException exception))) (ReplayStats 1 0 0 0 0)
        | otherwise ->
            let actual = originOf exception
                (message, loc, diff) = failureDetails exception
             in if actual == expected
                  then (Reconstructed FailureEvidence {message, notes, events, loc, diff}, ReplayStats 1 0 0 0 1)
                  else diverged (ChangedOrigin actual) (ReplayStats 1 0 0 0 0)
    diverged :: ReplayReason -> ReplayStats -> (FailureEvidenceStatus, ReplayStats)
    diverged reason accounting = (Diverged (ReplayDivergence reason), accounting)
