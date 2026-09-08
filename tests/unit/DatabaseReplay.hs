-- | End-to-end example-database replay and derandomization.
module DatabaseReplay (spec) where

import Control.Monad (void, when)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Function ((&))
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as T
import Hegel (Gen)
import Hegel.Database (Database (..))
import Hegel.Gen qualified as Gen
import Hegel.Internal.Foreign.Raw
import Hegel.Internal.Reconstruction qualified as Reconstruction
import Hegel.Internal.Replay qualified as InternalReplay
import Hegel.Phase (Phase (..))
import Hegel.Property (Property, assert, assume, forAll)
import Hegel.Property.Internal (Env (..), Journal (..), askEnv)
import Hegel.Replay (ReplayError (..), ReplayToken, decodeReplayToken, encodeReplayToken, replayTokenOrigin, replayTokenVersion)
import Hegel.Report (FailureEvidence (..), FailureEvidenceStatus (..), FailureOutcome (..), ReplayDivergence (..), ReplayReason (..), ReplayStats (..), Report (..), Reproduction (..), Result (..), Stats (..))
import Hegel.Runner (check, replay)
import Hegel.Settings (Settings (..), defaultSettings)
import System.FilePath ((</>))
import Test.Hspec
import TestSupport (allFailureOutcomes, expectReconstructed, expectToken, failureEvidenceStatuses)
import UnliftIO.Directory (doesDirectoryExist, listDirectory)
import UnliftIO.Exception (throwIO)
import UnliftIO.IORef (newIORef, readIORef, writeIORef)
import UnliftIO.Temporary (withSystemTempDirectory)

intR :: (Int, Int) -> Gen Int
intR (lo, hi) = Gen.integral & Gen.min lo & Gen.max hi & Gen.build

zeroFailure :: Property ()
zeroFailure = assert False "zero failure"

oneFailure :: Property ()
oneFailure = assert False "one failure"

spec :: Spec
spec = do
  it "reports and replays multiple distinct deterministic failures" $ do
    let settings = defaultSettings {reportMultipleFailures = True, testCases = 200}
        failing :: Property ()
        failing = do
          x <- forAll (intR (0, 1))
          if x == 0
            then zeroFailure
            else oneFailure
    report <- check settings failing
    case allFailureOutcomes report.result of
      [first, second] -> do
        firstToken <- expectToken first
        secondToken <- expectToken second
        replayTokenOrigin firstToken `shouldNotBe` replayTokenOrigin secondToken
        for_ [first, second] \outcome -> do
          token <- expectToken outcome
          evidence <- expectReconstructed (Failures (outcome :| []))
          decodeReplayToken (encodeReplayToken token) `shouldBe` Right token
          replayed <- replay settings token failing
          actual <- expectReconstructed replayed.result
          actual.message `shouldBe` evidence.message
          map (.failureReplayToken) (allFailureOutcomes replayed.result) `shouldBe` [Just token]
          replayed.stats.replayStats `shouldBe` Just (ReplayStats 1 0 0 0 1)
      other -> expectationFailure (show other)

  it "reports distinct token envelope errors" do
    for_
      [ ("hegel-replay:v:41", ReplayTokenMalformedEnvelope),
        ("other:v:41:AAAA", ReplayTokenUnsupportedFormat "other"),
        ("hegel-replay:v:41:", ReplayTokenEmptyField),
        ("hegel-replay:v:not-hex:AAAA", ReplayTokenInvalidOriginHex),
        ("hegel-replay:v:gg:AAAA", ReplayTokenInvalidOriginHex),
        ("hegel-replay:v:ff:AAAA", ReplayTokenOriginNotUtf8)
      ]
      \(encoded, expected) ->
        decodeReplayToken encoded `shouldBe` Left expected

  for_
    [ ("passing", pure (), UnexpectedSuccess, ReplayStats 1 1 0 0 0),
      ("discarded", assume False, UnexpectedDiscard, ReplayStats 1 0 1 0 0),
      ("exhausted", void (forAll (intR (0, 100))), ExhaustedChoices, ReplayStats 1 0 0 1 0)
    ]
    \(name, body, reason, accounting) ->
      it ("accounts for a " <> name <> " replay body") do
        token <- singletonToken =<< check defaultSettings zeroFailure
        replayed <- replay defaultSettings token body
        assertDivergence reason replayed
        assertAccounting accounting replayed

  it "rejects invalid blobs without executing the body" do
    token <- singletonToken =<< check defaultSettings zeroFailure
    let invalid = InternalReplay.makeReplayToken (replayTokenVersion token) (replayTokenOrigin token) "AAAA"
    ran <- newIORef False
    replayed <- replay defaultSettings invalid (writeIORef ran True)
    case failureEvidenceStatuses replayed.result of
      [Diverged (ReplayDivergence (InvalidReplayBlob _))] -> pure ()
      other -> expectationFailure (show other)
    assertAccounting (ReplayStats 0 0 0 0 0) replayed
    readIORef ran `shouldReturn` False

  it "retains expected and changed origins during public replay" do
    token <- singletonToken =<< check defaultSettings zeroFailure
    expectedActual <- singletonToken =<< check defaultSettings oneFailure
    replayed <- replay defaultSettings token oneFailure
    assertDivergence (ChangedOrigin (replayTokenOrigin expectedActual)) replayed
    map (.failureOrigin) (allFailureOutcomes replayed.result) `shouldBe` [replayTokenOrigin token]
    assertAccounting (ReplayStats 1 0 0 0 0) replayed

  it "leaves a copied failure without replay data tokenless and unexecuted" do
    ran <- newIORef False
    withContext \ctx -> withSettings ctx \settings -> do
      outcomes <-
        Reconstruction.reconstructFailures
          ctx
          (writeIORef ran True)
          settings
          10
          "test-version"
          (Reconstruction.Failure "missing-origin" Nothing :| [])
      let report = Report (Failures outcomes) (Stats 4 2 Nothing) Unstored
      assertDivergence MissingReplayData report
      map (.failureReplayToken) (allFailureOutcomes report.result) `shouldBe` [Nothing]
      readIORef ran `shouldReturn` False

  it "counts an execution-time engine exception as an attempted changed failure" do
    token <- singletonToken =<< check defaultSettings zeroFailure
    replayed <- replay defaultSettings token (throwIO HegelError {code = HEGEL_E_INVALID_ARG, message = Just "body engine error"})
    case failureEvidenceStatuses replayed.result of
      [Diverged (ReplayDivergence (ChangedOrigin _))] -> pure ()
      other -> expectationFailure (show other)
    assertAccounting (ReplayStats 1 0 0 0 0) replayed

  it "replays stored failures via the Reuse phase" $
    withSystemTempDirectory "zizek-replay" \dbDir -> do
      let settings =
            defaultSettings
              { database = DatabaseDirectory dbDir,
                databaseKey = Just "database-replay-spec"
              }
          failing :: Property ()
          failing = do
            x <- forAll (intR (0, 1000))
            assert (x < 100) "stays small"
      r1 <- check settings failing
      void (expectReconstructed r1.result)
      -- With generation disabled, only the stored example can fail it again.
      r2 <- check settings {phases = [Explicit, Reuse, Shrink]} failing
      void (expectReconstructed r2.result)

  it "reports unexpected success during reconstruction" $ do
    -- Fails exactly once. With shrinking enabled the engine's own replays
    -- would observe the disagreement and flag a flaky test (UnhealthyInput);
    -- with the Shrink phase off, the only re-execution is zizek's final
    -- reconstruction replay — which the engine cannot see — and it passes.
    flag <- newIORef False
    let nondeterministic :: Property ()
        nondeterministic = do
          _ <- forAll (intR (0, 10))
          fired <- readIORef flag
          if fired
            then pure ()
            else do
              writeIORef flag True
              assert False "fails exactly once (nondeterministic)"
    r <- check defaultSettings {phases = [Generate]} nondeterministic
    case failureEvidenceStatuses r.result of
      [Diverged (ReplayDivergence UnexpectedSuccess)] -> pure ()
      other -> expectationFailure ("expected a structured replay divergence, got: " <> show other)

  it "a passing run reports Unstored even with persistence configured" $
    withSystemTempDirectory "zizek-replay-passing" \dbDir -> do
      let settings =
            defaultSettings
              { database = DatabaseDirectory dbDir,
                databaseKey = Just "database-replay-passing-spec"
              }
          passing :: Property ()
          passing = do
            x <- forAll (intR (0, 10))
            assert (x >= 0) "non-negative"
      r <- check settings passing
      r.result `shouldSatisfy` \case Ok -> True; _ -> False
      r.reproduction `shouldBe` Unstored

  it "derandomize makes keyed runs deterministic" $ do
    let settings =
          defaultSettings
            { derandomize = True,
              databaseKey = Just "derandomize-spec"
            }
        go = check settings do
          x <- forAll (intR (0, 1000000))
          assume (even x)
          assert (x >= 0) "non-negative"
    ra <- go
    rb <- go
    ra.stats.valid `shouldBe` rb.stats.valid
    ra.stats.invalid `shouldBe` rb.stats.invalid

  it "keeps successful, divergent, and successful reconstructions in engine order" $ do
    let settings = defaultSettings {reportMultipleFailures = True, testCases = 300, derandomize = True, databaseKey = Just "ordered-replay"}
    baseline <- check settings (orderedProperty Nothing)
    case failureEvidenceStatuses baseline.result of
      [Reconstructed first, Reconstructed second, Reconstructed third] -> do
        report <- check settings (orderedProperty (Just second.message))
        case failureEvidenceStatuses report.result of
          [Reconstructed actualFirst, Diverged divergence, Reconstructed actualThird] -> do
            actualFirst.message `shouldBe` first.message
            actualThird.message `shouldBe` third.message
            map (.failureReplayToken) (allFailureOutcomes report.result) `shouldBe` map (.failureReplayToken) (allFailureOutcomes baseline.result)
            report.stats.valid `shouldBe` baseline.stats.valid
            report.stats.invalid `shouldBe` baseline.stats.invalid
            divergence.replayReason `shouldSatisfy` \case ChangedOrigin _ -> True; _ -> False
          other -> expectationFailure ("expected success/divergence/success: " <> show other)
      other -> expectationFailure ("expected three baseline failures: " <> show other)

  it "rejects incompatible versions with zero execution and both versions" do
    ran <- newIORef False
    token <- singletonToken =<< check defaultSettings zeroFailure
    let mismatched = InternalReplay.makeReplayToken "different-engine" (replayTokenOrigin token) (InternalReplay.tokenBlobOf token)
    replayed <- replay defaultSettings mismatched (writeIORef ran True)
    assertDivergence (IncompatibleVersions "different-engine" (replayTokenVersion token)) replayed
    assertAccounting (ReplayStats 0 0 0 0 0) replayed
    readIORef ran `shouldReturn` False

  it "explicit replay ignores populated and unusable database paths" $
    withSystemTempDirectory "replay-persistence" \dir -> do
      let settings = defaultSettings {database = DatabaseDirectory dir, databaseKey = Just "persisted"}
      void (check settings zeroFailure)
      contentsBefore <- databaseContents dir
      contentsBefore `shouldNotSatisfy` null
      token <- singletonToken =<< check defaultSettings oneFailure
      for_ [dir, dir </> "blocked"] \path -> do
        when (path /= dir) (writeFile path "a file cannot contain a database")
        report <- replay settings {database = DatabaseDirectory path} token oneFailure
        evidence <- expectReconstructed report.result
        evidence.message `shouldBe` "one failure"
        report.reproduction `shouldBe` Unstored
      contentsAfter <- databaseContents dir
      filter ((/= "blocked") . fst) contentsAfter `shouldBe` contentsBefore

singletonToken :: Report -> IO ReplayToken
singletonToken report = do
  void (expectReconstructed report.result)
  case allFailureOutcomes report.result of
    [outcome] -> expectToken outcome
    _ -> fail "expected one outcome"

assertDivergence :: ReplayReason -> Report -> Expectation
assertDivergence reason report = case failureEvidenceStatuses report.result of
  [Diverged divergence] -> divergence.replayReason `shouldBe` reason
  other -> expectationFailure (show other)

assertAccounting :: ReplayStats -> Report -> Expectation
assertAccounting expected report = do
  report.stats.replayStats `shouldBe` Just expected
  report.stats.valid `shouldBe` expected.replayValid
  report.stats.invalid `shouldBe` expected.replayInvalid

databaseContents :: FilePath -> IO [(FilePath, BS.ByteString)]
databaseContents root = go ""
  where
    go relative = do
      entries <- sort <$> listDirectory (root </> relative)
      concat
        <$> mapM
          ( \entry -> do
              let path = relative </> entry
              directory <- doesDirectoryExist (root </> path)
              if directory
                then go path
                else do
                  bytes <- BS.readFile (root </> path)
                  pure [(path, bytes)]
          )
          entries

orderedProperty :: Maybe T.Text -> Property ()
orderedProperty changed = do
  x <- forAll (intR (0, 2))
  let label = case x of 0 -> "ordered A"; 1 -> "ordered B"; _ -> "ordered C"
  env <- askEnv
  case env.journal of
    Recording _ -> when (changed == Just label) (assert False "changed assertion")
    Silent -> pure ()
  case x of
    0 -> assert False "ordered A"
    1 -> assert False "ordered B"
    _ -> assert False "ordered C"
