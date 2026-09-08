module TestSupport
  ( allFailureOutcomes,
    failureEvidenceStatuses,
    failureRecordOf,
    singleReconstructedEvidence,
    singleObservedEvidence,
    failureMessages,
    failureNotes,
    failureEvents,
    noteText,
    noteKind,
    failureResult,
    hasFailures,
    expectReconstructed,
    expectObserved,
    expectToken,
    forRenderers,
  )
where

import Data.Foldable (toList, traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Hegel.Replay (ReplayToken)
import Hegel.Report (Event, FailureEvidence (..), FailureEvidenceStatus (..), FailureOutcome (..), Note (..), NoteKind, Result (..))
import Hegel.Report qualified as Report
import Test.Hspec (Expectation, expectationFailure)

allFailureOutcomes :: Result -> [FailureOutcome]
allFailureOutcomes (Failures outcomes) = toList outcomes
allFailureOutcomes _ = []

failureEvidenceStatuses :: Result -> [FailureEvidenceStatus]
failureEvidenceStatuses = map (.failureEvidence) . allFailureOutcomes

hasFailures :: Result -> Bool
hasFailures (Failures _) = True
hasFailures _ = False

failureResult :: FailureEvidence -> Result
failureResult evidence = Failures (fixtureOutcome (Reconstructed evidence) :| [])

fixtureOutcome :: FailureEvidenceStatus -> FailureOutcome
fixtureOutcome evidence =
  FailureOutcome
    { failureOrigin = "test fixture",
      failureReplayToken = Nothing,
      failureEvidence = evidence,
      cleanupDiagnostics = []
    }

failureRecordOf :: FailureOutcome -> Maybe FailureEvidence
failureRecordOf outcome = case outcome.failureEvidence of
  Reconstructed evidence -> Just evidence
  Observed evidence -> Just evidence
  Diverged _ -> Nothing
  Skipped _ -> Nothing

singleReconstructedEvidence :: Result -> Maybe FailureEvidence
singleReconstructedEvidence result = case failureEvidenceStatuses result of
  [Reconstructed evidence] -> Just evidence
  _ -> Nothing

singleObservedEvidence :: Result -> Maybe FailureEvidence
singleObservedEvidence result = case failureEvidenceStatuses result of
  [Observed evidence] -> Just evidence
  _ -> Nothing

expectReconstructed :: Result -> IO FailureEvidence
expectReconstructed result = require "exactly one reconstructed failure" result (singleReconstructedEvidence result)

expectObserved :: Result -> IO FailureEvidence
expectObserved result = require "exactly one observed failure" result (singleObservedEvidence result)

expectToken :: FailureOutcome -> IO ReplayToken
expectToken outcome = require "a replay token" outcome outcome.failureReplayToken

require :: (Show a) => String -> a -> Maybe b -> IO b
require label actual = maybe (expectationFailure ("expected " <> label <> ", got: " <> show actual) >> fail label) pure

forRenderers :: Report.Report -> (Text -> Expectation) -> Expectation
forRenderers report assertion = do
  rich <- Report.renderReportRich report
  richAnsi <- Report.renderReportRichAnsi report
  traverse_ assertion [Report.renderReport report, Report.renderReportAnsi report, rich, richAnsi]

failureMessages :: Result -> [Text]
failureMessages = mapMaybe (fmap (.message) . failureRecordOf) . allFailureOutcomes

failureNotes :: Result -> [Note]
failureNotes = foldMap notesOf . mapMaybe failureRecordOf . allFailureOutcomes

failureEvents :: Result -> [Event]
failureEvents = foldMap eventsOf . mapMaybe failureRecordOf . allFailureOutcomes

notesOf :: FailureEvidence -> [Note]
notesOf FailureEvidence {notes} = notes

eventsOf :: FailureEvidence -> [Event]
eventsOf FailureEvidence {events} = events

noteText :: Note -> Text
noteText Note {text} = text

noteKind :: Note -> NoteKind
noteKind Note {kind} = kind
