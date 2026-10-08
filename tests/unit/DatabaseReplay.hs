-- | End-to-end example-database replay and derandomization.
module DatabaseReplay (spec) where

import Control.Monad (void, when)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Function ((&))
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust)
import Data.Text qualified as T
import Hegel (Gen)
import Hegel.Database (Database (..))
import Hegel.Gen qualified as Gen
import Hegel.Internal.Foreign.Raw
import Hegel.Internal.Replay qualified as InternalReplay
import Hegel.Nondeterminism (Nondeterminism (..))
import Hegel.Phase (Phase (..))
import Hegel.Property (Property, assert, assume, forAll)
import Hegel.Property.Internal (Env (..), Journal (..), askEnv)
import Hegel.Replay (ReplayError (..), ReplayToken, decodeReplayToken, encodeReplayToken, replayTokenOrigin, replayTokenVersion)
import Hegel.Report (Abort (..), FailureEvidence (..), FailureEvidenceStatus (..), FailureOutcome (..), ReplayDivergence (..), ReplayReason (..), Report (..), Reproduction (..), Result (..), Stats (..))
import Hegel.Runner (check, replay)
import Hegel.Settings (Settings (..), defaultSettings)
import System.FilePath ((</>))
import Test.Hspec
import TestSupport (allFailureOutcomes, expectCaptured, expectToken, failureEvidenceStatuses, forRenderers)
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
          evidence <- expectCaptured (Failures (outcome :| []))
          decodeReplayToken (encodeReplayToken token) `shouldBe` Right token
          replayed <- replay settings token failing
          actual <- expectCaptured replayed.result
          actual.message `shouldBe` evidence.message
          map (.failureReplayToken) (allFailureOutcomes replayed.result) `shouldBe` [Just token]
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
    [ ("passing", pure ()),
      ("discarding", assume False),
      ("overdrawing", void (forAll (intR (0, 100))))
    ]
    \(name, body) ->
      it ("reports a stale token replayed against a body that is " <> name <> " as not reproducing") do
        token <- singletonToken =<< check defaultSettings zeroFailure
        replayed <- replay defaultSettings token body
        assertDivergence DidNotReproduce replayed
        map (.failureReplayToken) (allFailureOutcomes replayed.result) `shouldBe` [Just token]

  it "rejects invalid blobs without executing the body" do
    token <- singletonToken =<< check defaultSettings zeroFailure
    let invalid = InternalReplay.makeReplayToken (replayTokenVersion token) (replayTokenOrigin token) "AAAA"
    ran <- newIORef False
    replayed <- replay defaultSettings invalid (writeIORef ran True)
    case failureEvidenceStatuses replayed.result of
      [Diverged (ReplayDivergence (InvalidReplayBlob _))] -> pure ()
      other -> expectationFailure (show other)
    assertNothingRan replayed
    readIORef ran `shouldReturn` False

  it "reports a changed-origin replay as a divergence followed by the actual failure" do
    token <- singletonToken =<< check defaultSettings zeroFailure
    actualOrigin <- replayTokenOrigin <$> (singletonToken =<< check defaultSettings oneFailure)
    replayed <- replay defaultSettings token oneFailure
    case allFailureOutcomes replayed.result of
      [divergent, actual] -> do
        divergent.failureOrigin `shouldBe` replayTokenOrigin token
        divergent.failureReplayToken `shouldBe` Just token
        case divergent.failureEvidence of
          Diverged d -> d.replayReason `shouldBe` ChangedOrigin actualOrigin
          other -> expectationFailure (show other)
        actual.failureOrigin `shouldBe` actualOrigin
        actual.failureReplayToken `shouldBe` Nothing
        evidence <- expectCaptured (Failures (actual :| []))
        evidence.message `shouldBe` "one failure"
      other -> expectationFailure (show other)
    forRenderers replayed \rendered -> do
      rendered `shouldSatisfy` T.isInfixOf "failure 1 (replay diverged)"
      rendered `shouldSatisfy` T.isInfixOf "one failure"

  it "aborts a replay whose body raises an engine exception" do
    token <- singletonToken =<< check defaultSettings zeroFailure
    replayed <- replay defaultSettings token (throwIO HegelError {code = HEGEL_E_INVALID_ARG, message = Just "body engine error"})
    case replayed.result of
      Aborted (Errored _) -> pure ()
      other -> expectationFailure (show other)

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
      void (expectCaptured r1.result)
      -- With generation disabled, only the stored example can fail it again.
      r2 <- check settings {phases = [Explicit, Reuse, Shrink]} failing
      void (expectCaptured r2.result)

  it "reports a fail-once flake with a caveat and no reproducer" $ do
    -- Fails exactly once. The engine's own replays of the failure pass, so
    -- it reports the failure unconfirmed, with no reproducer to replay.
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
    evidence <- expectCaptured r.result
    evidence.message `shouldBe` "fails exactly once (nondeterministic)"
    case allFailureOutcomes r.result of
      [outcome] -> do
        outcome.failureCaveat `shouldSatisfy` isJust
        outcome.failureReplayToken `shouldBe` Nothing
      other -> expectationFailure (show other)
    r.reproduction `shouldBe` Unreproducible
    forRenderers r \rendered -> rendered `shouldSatisfy` T.isInfixOf "note: "

  it "forbidden nondeterminism aborts the run" $ do
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
    r <- check defaultSettings {phases = [Generate], nondeterminism = Forbid} nondeterministic
    case r.result of
      Aborted (UnhealthyInput _) -> pure ()
      other -> expectationFailure (show other)

  it "reports a stamped capture over unstamped cases with the same origin" $ do
    let failing :: Property ()
        failing = do
          x <- forAll (intR (0, 1000))
          env <- askEnv
          let recorded = case env.journal of
                Recording _ -> "recorded"
                Silent -> "silent"
          assert (x < 100) recorded
    report <- check defaultSettings failing
    evidence <- expectCaptured report.result
    evidence.message `shouldBe` "recorded"
    evidence.notes `shouldNotSatisfy` null

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

  it "rejects incompatible versions with zero execution and both versions" do
    ran <- newIORef False
    token <- singletonToken =<< check defaultSettings zeroFailure
    let mismatched = InternalReplay.makeReplayToken "different-engine" (replayTokenOrigin token) (InternalReplay.tokenBlobOf token)
    replayed <- replay defaultSettings mismatched (writeIORef ran True)
    assertDivergence (IncompatibleVersions "different-engine" (replayTokenVersion token)) replayed
    assertNothingRan replayed
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
        evidence <- expectCaptured report.result
        evidence.message `shouldBe` "one failure"
        report.reproduction `shouldBe` Unstored
      contentsAfter <- databaseContents dir
      filter ((/= "blocked") . fst) contentsAfter `shouldBe` contentsBefore

singletonToken :: Report -> IO ReplayToken
singletonToken report = do
  void (expectCaptured report.result)
  case allFailureOutcomes report.result of
    [outcome] -> expectToken outcome
    _ -> fail "expected one outcome"

assertDivergence :: ReplayReason -> Report -> Expectation
assertDivergence reason report = case failureEvidenceStatuses report.result of
  [Diverged divergence] -> divergence.replayReason `shouldBe` reason
  other -> expectationFailure (show other)

assertNothingRan :: Report -> Expectation
assertNothingRan report = do
  report.stats.valid `shouldBe` 0
  report.stats.invalid `shouldBe` 0

databaseContents :: FilePath -> IO [(FilePath, BS.ByteString)]
databaseContents root = go ""
  where
    go relative = do
      entries <- sort <$> listDirectory (root </> relative)
      concat
        <$> traverse
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
