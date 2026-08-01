-- | Unit tests for the 'Hegel.Property.Branch' combinators.
module BranchProperties (spec) where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (def)
import Data.Function ((&))
import Data.List (sort)
import Data.Text qualified as T
import Hegel (Gen)
import Hegel.Gen qualified as Gen
import Hegel.HealthCheck (HealthCheck (..))
import Hegel.Pool qualified as Pool
import Hegel.Property
  ( annotateShow,
    assert,
    check,
    discard,
    forAll,
    forAllWithLabel,
  )
import Hegel.Property.Branch qualified as Branch
import Hegel.Report (Note (..), Report (..), Result (..), isBranchFailure, isBranchHeader, renderReport, renderReportRich)
import Hegel.Settings (Settings (..), defaultSettings)
import Test.Hspec
import UnliftIO.IORef (atomicModifyIORef', newIORef, readIORef)

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
      case report.result of
        Counterexample {message} -> message `shouldBe` "left branch always fails"
        other -> expectationFailure ("expected Counterexample, got: " <> show other)

    it "attributes a two-branch failure to the lower-indexed branch, deterministically" do
      -- Both branches fail unconditionally; the left one must win on every
      -- invocation, not whichever thread happens to finish first.
      let oneRun = do
            report <-
              check def do
                _ <- Branch.concurrently (assert False "left") (assert False "right")
                pure ()
            pure case report.result of
              Counterexample {message} -> Just message
              _ -> Nothing
      results <- replicateM 20 oneRun
      results `shouldSatisfy` all (== Just "left")

    it "nests each branch's notes under its own Branch N header" do
      report <- check def do
        _ <- Branch.concurrently (annotateShow (1 :: Int)) (annotateShow (2 :: Int))
        assert False "force a counterexample so the journal renders"
      case report.result of
        Counterexample {notes} -> do
          let headers = [n.text | n <- notes, isBranchHeader n]
          headers `shouldBe` ["Branch 1", "Branch 2"]
        other -> expectationFailure ("expected Counterexample, got: " <> show other)

    it "journals every failing branch's own message in-band, not only the shrink-target branch's" do
      -- Both branches fail independently with distinct messages; the losing
      -- branch's failure must still be visible in the journal, not silently
      -- dropped in favor of the winning ("left") branch's headline.
      report <- check def do
        _ <- Branch.concurrently (assert False "left branch always fails") (assert False "right branch always fails")
        pure ()
      case report.result of
        Counterexample {message, notes} -> do
          message `shouldBe` "left branch always fails"
          let branchFailures = [n.text | n <- notes, isBranchFailure n]
          branchFailures `shouldBe` ["left branch always fails", "right branch always fails"]
        other -> expectationFailure ("expected Counterexample, got: " <> show other)

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
      case report.result of
        Counterexample {notes} -> [n.text | n <- notes, isBranchFailure n] `shouldBe` []
        other -> expectationFailure ("expected Counterexample, got: " <> show other)
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
      case report.result of
        Counterexample {} -> pure ()
        other -> expectationFailure ("expected Counterexample, got: " <> show other)

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
        mapM_ (Pool.add pool) values
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
        mapM_ (Pool.add src) srcValues
        mapM_ (Pool.add dst) dstValues
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

    it "never runs more than cap branches live at once" do
      liveRef <- newIORef (0 :: Int)
      maxLiveRef <- newIORef (0 :: Int)
      report <- check def do
        _ <-
          Branch.replicateConcurrentlyBounded
            2
            8
            ( liftIO do
                atomicModifyIORef' liveRef \n -> (n + 1, ())
                cur <- readIORef liveRef
                atomicModifyIORef' maxLiveRef \m -> (max m cur, ())
                atomicModifyIORef' liveRef \n -> (n - 1, ())
            )
        pure ()
      report.result `shouldSatisfy` isOk
      maxLive <- readIORef maxLiveRef
      maxLive `shouldSatisfy` (<= 2)

  describe "control signals" do
    it "a discarding branch discards the whole case" do
      -- Every case discards unconditionally, which would otherwise trip the
      -- engine's FilterTooMuch health check before GaveUp classification.
      let settings = defaultSettings {suppressHealthCheck = [FilterTooMuch]}
      report <- check settings do
        _ <- Branch.concurrently discard (pure ())
        pure ()
      case report.result of
        GaveUp _ -> pure ()
        other -> expectationFailure ("expected GaveUp, got: " <> show other)
