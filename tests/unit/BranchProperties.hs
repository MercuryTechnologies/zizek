-- | Unit tests for the 'Hegel.Property.Branch' combinators.
module BranchProperties (spec) where

import Control.Concurrent (threadDelay)
import Control.Monad (join, replicateM, replicateM_)
import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (def)
import Data.Foldable (for_, traverse_)
import Data.Function ((&))
import Data.List (sort)
import Data.Text qualified as T
import Hegel (Gen)
import Hegel.Gen qualified as Gen
import Hegel.HealthCheck (HealthCheck (..))
import Hegel.Internal.TestCase (TestCase (..))
import Hegel.Pool qualified as Pool
import Hegel.Property
  ( Property,
    annotate,
    annotateShow,
    assert,
    check,
    discard,
    forAll,
    forAllWithLabel,
    (===),
  )
import Hegel.Property.Branch qualified as Branch
import Hegel.Property.Internal (Env (..), askEnv)
import Hegel.Report (FailureEvidence (..), FailureOutcome (..), Note (..), Report (..), Result (..), isBranchFailure, isBranchHeader, renderReport, renderReportRich)
import Hegel.Runner (replay)
import Hegel.Seed (Seed (..))
import Hegel.Settings (Settings (..), defaultSettings)
import Test.Hspec
import TestSupport (allFailureOutcomes, expectCaptured, expectToken, singleCapturedEvidence)
import UnliftIO.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)

intR :: (Int, Int) -> Gen Int
intR (lo, hi) = Gen.integral & Gen.min lo & Gen.max hi & Gen.build

isOk :: Result -> Bool
isOk = \case
  Ok -> True
  _ -> False

spec :: Spec
spec = describe "concurrent combinators" do
  describe "Branch.concurrently" do
    it "runs both branches and combines their results" do
      report <- check def do
        (x, y) <- Branch.concurrently (pure (1 :: Int)) (pure (2 :: Int))
        assert (x == 1 && y == 2) "both branch results survive"
      report.result `shouldSatisfy` isOk

    it "lets each branch draw independently" do
      report <- check def do
        (x, y) <- Branch.concurrently (forAll (intR (0, 100))) (forAll (intR (0, 100)))
        annotateShow (x :: Int, y :: Int)
        assert (x >= 0 && y >= 0) "draws succeed"
      report.result `shouldSatisfy` isOk

    it "reports a branch assertion failure as a shrinkable counterexample, not Errored" do
      report <- check def do
        _ <- Branch.concurrently (assert False "left branch always fails") (pure ())
        pure ()
      case singleCapturedEvidence report.result of
        Just FailureEvidence {message} -> message `shouldBe` "left branch always fails"
        other -> expectationFailure ("expected failure, got: " <> show other)

    it "attributes a two-branch failure to the lower-indexed branch, deterministically" do
      -- Both branches fail unconditionally; the left one must win on every
      -- invocation, not whichever thread happens to finish first.
      let oneRun = do
            report <-
              check def do
                _ <- Branch.concurrently (assert False "left") (assert False "right")
                pure ()
            pure case singleCapturedEvidence report.result of
              Just FailureEvidence {message} -> Just message
              _ -> Nothing
      results <- replicateM 20 oneRun
      results `shouldSatisfy` all (== Just "left")

    it "nests each branch's notes under its own Branch N header" do
      report <- check def do
        _ <- Branch.concurrently (annotateShow (1 :: Int)) (annotateShow (2 :: Int))
        assert False "force a counterexample so the journal renders"
      case singleCapturedEvidence report.result of
        Just FailureEvidence {notes} -> do
          let headers = [n.text | n <- notes, isBranchHeader n]
          headers `shouldBe` ["Branch 1", "Branch 2"]
        other -> expectationFailure ("expected failure, got: " <> show other)

    it "journals every failing branch's own message in-band, not only the shrink-target branch's" do
      -- Both branches fail independently with distinct messages; the losing
      -- branch's failure must still be visible in the journal, not silently
      -- dropped in favor of the winning ("left") branch's headline.
      report <- check def do
        _ <- Branch.concurrently (assert False "left branch always fails") (assert False "right branch always fails")
        pure ()
      case singleCapturedEvidence report.result of
        Just FailureEvidence {message, notes} -> do
          message `shouldBe` "left branch always fails"
          let branchFailures = [n.text | n <- notes, isBranchFailure n]
          branchFailures `shouldBe` ["left branch always fails", "right branch always fails"]
        other -> expectationFailure ("expected failure, got: " <> show other)

    it "suppresses the redundant top-level headline once a branch fails in-band" do
      -- 'renderReport' drops the top headline/loc block when the journal
      -- already carries an in-band failure; both branches' messages must
      -- still show up somewhere in the rendered body.
      report <- check def do
        _ <- Branch.concurrently (assert False "left branch always fails") (assert False "right branch always fails")
        pure ()
      let rendered = renderReport report
      ("left branch always fails" `T.isInfixOf` rendered) `shouldBe` True
      ("right branch always fails" `T.isInfixOf` rendered) `shouldBe` True

    it "keeps the top-level headline when no branch failed in-band" do
      -- A later, unrelated top-level assertion after both branches succeed
      -- has no in-band failure to anchor the reason, so the headline must
      -- survive.
      report <- check def do
        _ <- Branch.concurrently (annotateShow (1 :: Int)) (annotateShow (2 :: Int))
        assert False "unrelated top-level assertion"
      case singleCapturedEvidence report.result of
        Just FailureEvidence {notes} -> [n.text | n <- notes, isBranchFailure n] `shouldBe` []
        other -> expectationFailure ("expected failure, got: " <> show other)
      ("unrelated top-level assertion" `T.isInfixOf` renderReport report) `shouldBe` True

    it "splices every branch's source into the rich report" do
      -- End-to-end through 'renderReportRich' (requires cwd = repo root, as
      -- under `just test`): this is the regression test for the render-path
      -- misclassification bug — a Branch.concurrently failure must route to the
      -- concurrent splice, not silently degrade or misrender as a stateful
      -- composed report.
      report <- check def do
        _ <- Branch.concurrently (assert False "left branch always fails") (assert False "right branch always fails")
        pure ()
      rich <- renderReportRich report
      ("┏━━ tests/unit/BranchProperties.hs" `T.isInfixOf` rich) `shouldBe` True
      ("left branch always fails" `T.isInfixOf` rich) `shouldBe` True
      ("right branch always fails" `T.isInfixOf` rich) `shouldBe` True

    it "shares a Pool across branches with no duplicate or lost value" do
      report <- check def do
        pool <- Pool.new
        Pool.add pool (1 :: Int)
        Pool.add pool (2 :: Int)
        (x, y) <- Branch.concurrently (forAll (Pool.consume pool)) (forAll (Pool.consume pool))
        remaining <- liftIO (Pool.size pool)
        annotateShow (sort [x, y], remaining)
        assert (sort [x, y] == [1, 2] && remaining == 0) "each value consumed exactly once"
      report.result `shouldSatisfy` isOk

  describe "branch splice threshold and merge" do
    it "merges branches sharing an enclosing declaration into one labeled block" do
      report <- check def do
        _ <- Branch.concurrently (assert False "left branch always fails") (assert False "right branch always fails")
        pure ()
      rich <- renderReportRich report
      T.count "┏━━" rich `shouldBe` 1
      ("Branch 1:" `T.isInfixOf` rich) `shouldBe` True
      ("Branch 2:" `T.isInfixOf` rich) `shouldBe` True

    it "stacks failures sharing a source line under one caret, each diff beneath its label" do
      report <- check def do
        Branch.forConcurrently_ [1, 2 :: Int] \i -> i === 0
      rich <- renderReportRich report
      let caretOnly l = case T.strip (snd (T.breakOnEnd "┃" l)) of
            "" -> False
            rest -> T.all (== '^') rest
      length (filter caretOnly (T.lines rich)) `shouldBe` 1
      T.count "at tests/unit/BranchProperties.hs:" rich `shouldBe` 1
      ("Branch 1: === failed" `T.isInfixOf` rich) `shouldBe` True
      ("Branch 2: === failed" `T.isInfixOf` rich) `shouldBe` True
      T.count "│   (- lhs) (+ rhs)" rich `shouldBe` 2

    it "does not show a redundant bare header once a branch's content is fully spliced" do
      -- Regression test: an earlier version of this renderer always emitted
      -- a "Branch N" header line even when that branch's content already
      -- merged into the labeled listing below, which read as if the listing
      -- belonged only to whichever header happened to sit closest to it.
      report <- check def do
        _ <- Branch.concurrently (assert False "left branch always fails") (assert False "right branch always fails")
        pure ()
      rich <- renderReportRich report
      T.count "\n  Branch " ("\n" <> rich) `shouldBe` 0

    it "collapses passing branches into a summary once past the splice threshold" do
      report <- check def do
        _ <- Branch.mapConcurrently (\i -> assert (i /= (7 :: Int)) "branch seven fails") [1 .. 10]
        pure ()
      rich <- renderReportRich report
      ("9 branches passed" `T.isInfixOf` rich) `shouldBe` True
      -- Only branch 7's label appears; no other branch number shows up
      -- anywhere in the report.
      let branch7Labels = T.count "Branch 7:" rich
      branch7Labels `shouldBe` 1
      T.count "Branch " rich `shouldBe` branch7Labels

    it "shows a summary with no branch dump when every branch passed" do
      report <- check def do
        _ <- Branch.replicateConcurrently 10 (pure ())
        assert False "unrelated top-level assertion"
      rich <- renderReportRich report
      ("10 branches passed" `T.isInfixOf` rich) `shouldBe` True
      ("unrelated top-level assertion" `T.isInfixOf` rich) `shouldBe` True
      T.count "Branch " rich `shouldBe` 0

    it "keeps top-level activity outside the branch count" do
      report <- check def do
        traverse_ (\i -> annotate ("setup step " <> T.pack (show (i :: Int)))) [1 .. 5]
        Branch.concurrently_ (pure ()) (assert False "right branch always fails")
      rich <- renderReportRich report
      for_ [1 .. 5 :: Int] \i ->
        ("setup step " <> T.pack (show i)) `shouldSatisfy` (`T.isInfixOf` rich)
      ("Branch 2:" `T.isInfixOf` rich) `shouldBe` True
      ("passed" `T.isInfixOf` rich) `shouldBe` False

    it "counts only branches in the summary past the splice threshold" do
      report <- check def do
        traverse_ (\i -> annotate ("setup step " <> T.pack (show (i :: Int)))) [1 .. 5]
        Branch.mapConcurrently_ (\i -> assert (i /= (7 :: Int)) "branch seven fails") [1 .. 10]
      rich <- renderReportRich report
      for_ [1 .. 5 :: Int] \i ->
        ("setup step " <> T.pack (show i)) `shouldSatisfy` (`T.isInfixOf` rich)
      ("9 branches passed" `T.isInfixOf` rich) `shouldBe` True

    it "still shows every branch's data below the splice threshold" do
      report <- check def do
        rs <- Branch.replicateConcurrently 3 (forAll (intR (0, 1000)))
        assert (all (< 42) rs) "every branch stays small"
      rich <- renderReportRich report
      ("Branch 1:" `T.isInfixOf` rich) `shouldBe` True
      ("Branch 2:" `T.isInfixOf` rich) `shouldBe` True
      ("Branch 3:" `T.isInfixOf` rich) `shouldBe` True

  describe "Branch.concurrently_" do
    it "runs both branches, discarding results" do
      report <- check def (Branch.concurrently_ (pure ()) (pure ()))
      report.result `shouldSatisfy` isOk

  describe "Branch.mapConcurrently / Branch.replicateConcurrently" do
    it "returns one result per input, in order" do
      report <- check def do
        rs <- Branch.mapConcurrently (\i -> pure (i * 2)) [1 .. 5 :: Int]
        assert (rs == [2, 4 .. 10]) "results preserve input order"
      report.result `shouldSatisfy` isOk

    it "fans out to n branches, each drawing independently" do
      report <- check def do
        rs <- Branch.replicateConcurrently 5 (forAll (intR (0, 1000)))
        assert (length rs == 5) "one result per branch"
      report.result `shouldSatisfy` isOk

    it "a failing branch among many shrinks like any other counterexample" do
      report <- check def do
        rs <- Branch.replicateConcurrently 3 (forAll (intR (0, 1000)))
        assert (all (< 42) rs) "every branch stays small"
      evidence <- expectCaptured report.result
      evidence.message `shouldBe` "every branch stays small"

  describe "Pool concurrency safety" do
    it "shares a Pool across n concurrent consuming branches with no duplicate or lost value" do
      report <- check def do
        n <- forAllWithLabel "branchCount" (intR (1, 8))
        pool <- Pool.new
        values <-
          forAllWithLabel
            "values"
            ( Gen.list (intR (-1000, 1000))
                & Gen.unique (==)
                & Gen.minSize n
                & Gen.maxSize n
                & Gen.build
            )
        traverse_ (Pool.add pool) values
        results <- Branch.replicateConcurrently n (forAll (Pool.consume pool))
        remaining <- liftIO (Pool.size pool)
        annotateShow (n, sort values, sort results, remaining)
        assert
          (sort results == sort values && remaining == 0)
          "each preloaded value is consumed exactly once, across n branches"
      report.result `shouldSatisfy` isOk

    it "loses no entries when Pool.add is called concurrently from many branches" do
      report <- check def do
        n <- forAllWithLabel "branchCount" (intR (1, 8))
        pool <- Pool.new
        added <- Branch.replicateConcurrently n do
          v <- forAll (intR (-1000, 1000))
          Pool.add pool v
          pure v
        remaining <- liftIO (Pool.size pool)
        consumed <- replicateM remaining (forAll (Pool.consume pool))
        annotateShow (n, sort added, remaining, sort consumed)
        assert
          (remaining == n && sort consumed == sort added)
          "every concurrently-added value survives, none lost or duplicated"
      report.result `shouldSatisfy` isOk

    it "transfers n values between pools under concurrent access with no loss or duplication" do
      report <- check def do
        n <- forAllWithLabel "branchCount" (intR (1, 8))
        srcValues <-
          forAllWithLabel
            "srcValues"
            ( Gen.list (intR (-1000, 1000))
                & Gen.unique (==)
                & Gen.minSize n
                & Gen.maxSize n
                & Gen.build
            )
        dstValues <-
          forAllWithLabel
            "dstValues"
            (Gen.list (intR (-1000, 1000)) & Gen.unique (==) & Gen.maxSize 5 & Gen.build)
        src <- Pool.new
        dst <- Pool.new
        traverse_ (Pool.add src) srcValues
        traverse_ (Pool.add dst) dstValues
        transferred <- Branch.replicateConcurrently n (forAll (Pool.transfer src dst))
        srcRemaining <- liftIO (Pool.size src)
        dstFinal <- liftIO (Pool.size dst)
        annotateShow (n, sort srcValues, sort transferred, srcRemaining, dstFinal)
        assert
          ( sort transferred == sort srcValues
              && srcRemaining == 0
              && dstFinal == length dstValues + n
          )
          "every src value transfers exactly once; dst gains n entries with none of its own lost"
      report.result `shouldSatisfy` isOk

  describe "Branch.replicateConcurrentlyBounded" do
    it "still returns n results when capped below n" do
      report <- check def do
        rs <- Branch.replicateConcurrentlyBounded 2 6 (pure ())
        assert (length rs == 6) "cap limits concurrency, not the result count"
      report.result `shouldSatisfy` isOk

    it "never holds more than cap clones live at once" do
      peakRef <- newIORef (0 :: Int)
      report <- check def do
        live <- (.testCase.liveClones) <$> askEnv
        _ <-
          Branch.replicateConcurrentlyBounded
            2
            8
            ( liftIO do
                cur <- readIORef live
                atomicModifyIORef' peakRef \m -> (max m cur, ())
                threadDelay 2000
            )
        pure ()
      report.result `shouldSatisfy` isOk
      peak <- readIORef peakRef
      peak `shouldSatisfy` (<= 2)
      peak `shouldSatisfy` (>= 1)

    it "frees every clone when a branch fails" do
      liveRef <- newIORef (pure 0)
      report <- check def do
        live <- (.testCase.liveClones) <$> askEnv
        liftIO (writeIORef liveRef (readIORef live))
        _ <- Branch.replicateConcurrentlyBounded 2 6 (assert False "branch fails")
        pure ()
      case singleCapturedEvidence report.result of
        Just FailureEvidence {message} -> message `shouldBe` "branch fails"
        other -> expectationFailure ("expected failure, got: " <> show other)
      join (readIORef liveRef) >>= (`shouldBe` 0)

    it "returns results in input order whatever the cap or scheduling" do
      let settings = defaultSettings {seed = Just (SeedFixed 7), testCases = Just 1, database = Nothing}
          runWith cap = do
            ref <- newIORef []
            report <-
              check settings do
                rs <- Branch.replicateConcurrentlyBounded cap 6 (forAll (intR (0, 1000)) <* liftIO (threadDelay 1000))
                liftIO (writeIORef ref rs)
            report.result `shouldSatisfy` isOk
            readIORef ref
      sequential <- runWith 1
      length sequential `shouldBe` 6
      for_ [2, 3, 6, 9] \cap -> replicateM_ 5 (runWith cap >>= (`shouldBe` sequential))

    it "replays a failing bounded fan-out from its token to the same failure" do
      let settings = defaultSettings {testCases = Just 200}
          failing :: Property ()
          failing = do
            _ <- Branch.replicateConcurrentlyBounded 2 6 do
              x <- forAll (intR (0, 20))
              assert (x < 15) ("drew " <> T.pack (show x))
            pure ()
      report <- check settings failing
      outcome <- case allFailureOutcomes report.result of
        [o] -> pure o
        other -> expectationFailure (show other) >> fail "no single failure"
      token <- expectToken outcome
      evidence <- expectCaptured report.result
      replayed <- replay settings token failing
      actual <- expectCaptured replayed.result
      actual.message `shouldBe` evidence.message
      map (.failureReplayToken) (allFailureOutcomes replayed.result) `shouldBe` [Just token]

  describe "control signals" do
    it "a discarding branch discards the whole case" do
      -- Every case discards unconditionally, which would otherwise trip the
      -- engine's FilterTooMuch health check before GaveUp classification.
      let settings = defaultSettings {suppressHealthCheck = Just [FilterTooMuch]}
      report <- check settings do
        _ <- Branch.concurrently discard (pure ())
        pure ()
      case report.result of
        GaveUp _ -> pure ()
        other -> expectationFailure ("expected GaveUp, got: " <> show other)
