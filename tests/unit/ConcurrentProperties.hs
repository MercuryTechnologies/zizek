-- | Unit tests for the 'Hegel.Property.Concurrent' combinators.
module ConcurrentProperties (spec) where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
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
    concurrently,
    concurrently_,
    discard,
    forAll,
    mapConcurrently,
    replicateConcurrently,
    replicateConcurrentlyBounded,
  )
import Hegel.Report (Note (..), NoteKind (..), Report (..), Result (..))
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
  describe "concurrently" do
    it "runs both branches and combines their results" do
      report <- check defaultSettings do
        (x, y) <- concurrently (pure (1 :: Int)) (pure (2 :: Int))
        assert (x == 1 && y == 2) "both branch results survive"
      report.result `shouldSatisfy` isOk

    it "lets each branch draw independently" do
      report <- check defaultSettings do
        (x, y) <- concurrently (forAll (intR (0, 100))) (forAll (intR (0, 100)))
        annotateShow (x :: Int, y :: Int)
        assert (x >= 0 && y >= 0) "draws succeed"
      report.result `shouldSatisfy` isOk

    it "reports a branch assertion failure as a shrinkable counterexample, not Errored" do
      report <- check defaultSettings do
        _ <- concurrently (assert False "left branch always fails") (pure ())
        pure ()
      case report.result of
        Counterexample {message} -> message `shouldBe` "left branch always fails"
        other -> expectationFailure ("expected Counterexample, got: " <> show other)

    it "attributes a two-branch failure to the lower-indexed branch, deterministically" do
      -- Both branches fail unconditionally; the left one must win on every
      -- invocation, not whichever thread happens to finish first.
      let oneRun = do
            report <-
              check defaultSettings do
                _ <- concurrently (assert False "left") (assert False "right")
                pure ()
            pure case report.result of
              Counterexample {message} -> Just message
              _ -> Nothing
      results <- replicateM 20 oneRun
      results `shouldSatisfy` all (== Just "left")

    it "nests each branch's notes under its own Branch N header" do
      report <- check defaultSettings do
        _ <- concurrently (annotateShow (1 :: Int)) (annotateShow (2 :: Int))
        assert False "force a counterexample so the journal renders"
      case report.result of
        Counterexample {notes} -> do
          let headers = [n.text | n <- notes, n.kind == Annotation, "Branch " `T.isPrefixOf` n.text]
          headers `shouldBe` ["Branch 1", "Branch 2"]
        other -> expectationFailure ("expected Counterexample, got: " <> show other)

    it "shares a Pool across branches with no duplicate or lost value" do
      report <- check defaultSettings do
        pool <- Pool.new
        Pool.add pool (1 :: Int)
        Pool.add pool (2 :: Int)
        (x, y) <- concurrently (forAll (Pool.consume pool)) (forAll (Pool.consume pool))
        remaining <- liftIO (Pool.size pool)
        annotateShow (sort [x, y], remaining)
        assert (sort [x, y] == [1, 2] && remaining == 0) "each value consumed exactly once"
      report.result `shouldSatisfy` isOk

  describe "concurrently_" do
    it "runs both branches, discarding results" do
      report <- check defaultSettings (concurrently_ (pure ()) (pure ()))
      report.result `shouldSatisfy` isOk

  describe "mapConcurrently / replicateConcurrently" do
    it "returns one result per input, in order" do
      report <- check defaultSettings do
        rs <- mapConcurrently (\i -> pure (i * 2)) [1 .. 5 :: Int]
        assert (rs == [2, 4 .. 10]) "results preserve input order"
      report.result `shouldSatisfy` isOk

    it "fans out to n branches, each drawing independently" do
      report <- check defaultSettings do
        rs <- replicateConcurrently 5 (forAll (intR (0, 1000)))
        assert (length rs == 5) "one result per branch"
      report.result `shouldSatisfy` isOk

    it "a failing branch among many shrinks like any other counterexample" do
      report <- check defaultSettings do
        rs <- replicateConcurrently 3 (forAll (intR (0, 1000)))
        assert (all (< 42) rs) "every branch stays small"
      case report.result of
        Counterexample {} -> pure ()
        other -> expectationFailure ("expected Counterexample, got: " <> show other)

  describe "replicateConcurrentlyBounded" do
    it "still returns n results when capped below n" do
      report <- check defaultSettings do
        rs <- replicateConcurrentlyBounded 2 6 (pure ())
        assert (length rs == 6) "cap limits concurrency, not the result count"
      report.result `shouldSatisfy` isOk

    it "never runs more than cap branches live at once" do
      liveRef <- newIORef (0 :: Int)
      maxLiveRef <- newIORef (0 :: Int)
      report <- check defaultSettings do
        _ <-
          replicateConcurrentlyBounded
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
        _ <- concurrently discard (pure ())
        pure ()
      case report.result of
        GaveUp _ -> pure ()
        other -> expectationFailure ("expected GaveUp, got: " <> show other)
