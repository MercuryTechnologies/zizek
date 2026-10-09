-- | The viability bar every demo's failure has to clear.
module Support
  ( viable,
    isOk,
  )
where

import Control.Monad (when)
import Data.Default.Class (def)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Hegel.Property (Property, check)
import Hegel.Report (FailureEvidence (..), FailureEvidenceStatus (..), FailureOutcome (..), Note (..), Report (..), Result (..), isDrawn, renderReportRich)
import Hegel.Runner (replay)
import System.Environment (lookupEnv)
import Test.Hspec (expectationFailure, shouldBe)

-- | Check that the engine finds the property's failure with no
-- nondeterminism caveat, and that replaying its token draws the same values,
-- returning the failing case's drawn notes.
viable :: Property () -> IO [Text]
viable prop = do
  report <- check def prop
  show' <- lookupEnv "SUPPLY_SPIKE_SHOW"
  when (show' == Just "1") (renderReportRich report >>= TIO.putStrLn)
  outcome <- firstFailure report.result
  outcome.failureCaveat `shouldBe` Nothing
  drawn <- drawnOf outcome
  token <- maybe (failWith "a replay token" outcome) pure outcome.failureReplayToken
  replayed <- replay def token prop
  replayedOutcome <- firstFailure replayed.result
  drawnOf replayedOutcome >>= (`shouldBe` drawn)
  pure drawn

isOk :: Result -> Bool
isOk = \case
  Ok -> True
  _ -> False

firstFailure :: Result -> IO FailureOutcome
firstFailure = \case
  Failures (outcome :| _) -> pure outcome
  other -> failWith "a failure" other

drawnOf :: FailureOutcome -> IO [Text]
drawnOf outcome = case outcome.failureEvidence of
  Captured evidence -> pure [n.text | n <- evidence.notes, isDrawn n.kind]
  _ -> failWith "captured evidence" outcome

failWith :: (Show a) => String -> a -> IO b
failWith what actual = do
  expectationFailure ("expected " <> what <> ", got: " <> show actual)
  fail what
