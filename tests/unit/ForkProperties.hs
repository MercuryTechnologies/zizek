-- | Unit tests for 'Hegel.Property.Fork' ('Fork.spawn', 'Fork.join',
-- 'Fork.cancel', 'Fork.poll', 'Fork.scoped').
module ForkProperties (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, displayException)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.List (sort)
import Data.Text qualified as T
import Hegel (Gen)
import Hegel.Gen qualified as Gen
import Hegel.Pool qualified as Pool
import Hegel.Property
  ( Fork,
    Property,
    annotateShow,
    assert,
    check,
    forAll,
  )
import Hegel.Property.Branch qualified as Branch
import Hegel.Property.Fork qualified as Fork
import Hegel.Report (Abort (..), Note (..), Report (..), Result (..), isBranchHeader, renderReportRich)
import Hegel.Settings (Settings (..), defaultSettings)
import Test.Hspec
import UnliftIO.IORef (newIORef, readIORef, writeIORef)

intR :: (Int, Int) -> Gen Int
intR (lo, hi) = Gen.integral & Gen.min lo & Gen.max hi & Gen.build

isOk :: Result -> Bool
isOk = \case
  Ok -> True
  _ -> False

-- | Spin on 'Fork.poll' until a fork has settled, without joining or
-- cancelling it. Used to make a fork's completion deterministic for a test
-- without relying on wall-clock timing, while still leaving it registered
-- as an open, unjoined fork.
pollUntilSettled :: Fork a -> Property (Maybe (Either SomeException a))
pollUntilSettled f = do
  mr <- Fork.poll f
  case mr of
    Nothing -> pollUntilSettled f
    Just _ -> pure mr

-- | A fork body that never completes on its own, so a caller can only
-- observe a settled outcome for it if something cancelled it first.
--
-- Blocks on real time rather than looping on a draw. An unbounded draw loop
-- inflates the engine's initial-size estimate for the case, by however many
-- iterations run before cancellation lands, and trips its
-- @LargeInitialTestCase@ health check for reasons unrelated to what this
-- tests.
spinForever :: Property ()
spinForever = liftIO (threadDelay maxBound)

spec :: Spec
spec = describe "Hegel.Property.Fork" do
  describe "Fork.spawn / Fork.join" do
    it "returns the fork's result on join" do
      report <- check defaultSettings do
        f <- Fork.spawn (pure (42 :: Int))
        v <- Fork.join f
        assert (v == 42) "fork result observed"
      report.result `shouldSatisfy` isOk

    it "lets a fork draw independently" do
      report <- check defaultSettings do
        f <- Fork.spawn (forAll (intR (0, 100)))
        v <- Fork.join f
        assert (v >= 0) "fork drew a value"
      report.result `shouldSatisfy` isOk

    it "reports a joined fork's failure as a shrinkable counterexample, not Errored" do
      report <- check defaultSettings do
        f <- Fork.spawn (assert False "fork failed")
        Fork.join f
      case report.result of
        Counterexample {message} -> message `shouldBe` "fork failed"
        other -> expectationFailure ("expected Counterexample, got: " <> show other)

    it "surfaces the first-joined failing fork's message deterministically" do
      -- Unlike Branch.concurrently's two-branch race, join order here is
      -- ordinary sequential code: the first Fork.join throws before the
      -- second one ever runs, so this is deterministic by construction, not
      -- by luck.
      --
      -- Built from Fork.scoped rather than raw Fork.spawn/Fork.join pairs:
      -- when Fork.join f1 throws, plain sequential code would skip past
      -- Fork.join f2 entirely, leaking it. Fork.scoped cancels f2 on that
      -- unwind instead, so the test observes "first" rather than a
      -- leaked-fork malformed-test abort.
      let oneRun = do
            report <- check defaultSettings do
              Fork.scoped (assert False "first") \f1 ->
                Fork.scoped (assert False "second") \f2 -> do
                  _ <- Fork.join f1
                  _ <- Fork.join f2
                  pure ()
            pure case report.result of
              Counterexample {message} -> Just message
              _ -> Nothing
      results <- replicateM 20 oneRun
      results `shouldSatisfy` all (== Just "first")

    it "folds a joined fork's notes under a Fork N header" do
      report <- check defaultSettings do
        f <- Fork.spawn (annotateShow (1 :: Int))
        _ <- Fork.join f
        assert False "force a counterexample so the journal renders"
      case report.result of
        Counterexample {notes} -> do
          let headers = [n.text | n <- notes, isBranchHeader n]
          headers `shouldBe` ["Fork 1"]
        other -> expectationFailure ("expected Counterexample, got: " <> show other)

    it "splices a fork's source into the rich report under Fork N:, distinct from Branch N:" do
      report <- check defaultSettings do
        f <- Fork.spawn (assert False "fork failed")
        Fork.join f
      rich <- renderReportRich report
      ("Fork 1:" `T.isInfixOf` rich) `shouldBe` True
      ("Branch 1:" `T.isInfixOf` rich) `shouldBe` False

    it "renders genuine top-level notes plainly, with no spurious self-referential label" do
      -- Regression test: a top-level note alongside a fork forms its own
      -- depth-0 group rooted at an ordinary note, not a BranchHeader. That
      -- root note's own text must not be read back as a group label, which
      -- would double the rendered value up on itself. The value is a
      -- distinctive number, not e.g. "99", so it can't collide with the
      -- source splice's own context lines, including this comment.
      report <- check defaultSettings do
        annotateShow (424242 :: Int)
        f <- Fork.spawn (assert False "fork failed")
        Fork.join f
      rich <- renderReportRich report
      ("424242" `T.isInfixOf` rich) `shouldBe` True
      ("424242: 424242" `T.isInfixOf` rich) `shouldBe` False

    it "Fork.poll observes a fork's outcome without consuming it" do
      report <- check defaultSettings do
        f <- Fork.spawn (pure (7 :: Int))
        mr <- pollUntilSettled f
        assert (case mr of Just (Right 7) -> True; _ -> False) "poll observed the fork's successful result"
        v <- Fork.join f
        assert (v == 7) "join still delivers the result after polling"
      report.result `shouldSatisfy` isOk

    it "shares a Pool across two forked branches with no duplicate or lost value" do
      -- Mirrors Branch.concurrently's own Pool precedent: both racy consumes
      -- stay inside cloned branches. Consuming once on the parent's own
      -- stream and once inside a fork, racing the same pool, would make how
      -- much is left for the parent's draw depend on real-time scheduling
      -- against the fork. From the engine's perspective that is a generator
      -- depending on global mutable state, and it trips the non-determinism
      -- health check. Keeping every racy access inside its own clone avoids
      -- that.
      report <- check defaultSettings do
        pool <- Pool.new
        Pool.add pool (1 :: Int)
        Pool.add pool (2 :: Int)
        fx <- Fork.spawn (forAll (Pool.consume pool))
        fy <- Fork.spawn (forAll (Pool.consume pool))
        x <- Fork.join fx
        y <- Fork.join fy
        remaining <- liftIO (Pool.size pool)
        annotateShow (sort [x, y], remaining)
        assert (sort [x, y] == [1, 2] && remaining == 0) "each value consumed exactly once"
      report.result `shouldSatisfy` isOk

    it "supports a fork whose own body forks and joins another fork" do
      report <- check defaultSettings do
        outer <- Fork.spawn do
          inner <- Fork.spawn (pure (41 :: Int))
          v <- Fork.join inner
          pure (v + 1)
        v <- Fork.join outer
        assert (v == (42 :: Int)) "nested fork result observed"
      report.result `shouldSatisfy` isOk

  describe "leaked forks" do
    it "fails the case as a malformed test when a fork is never joined or cancelled" do
      report <- check defaultSettings do
        _ <- Fork.spawn (pure ())
        pure ()
      case report.result of
        Aborted (Errored e) -> T.pack (displayException e) `shouldSatisfy` T.isInfixOf "1 fork"
        other -> expectationFailure ("expected Aborted Errored, got: " <> show other)

    it "folds a leaked fork's own failure text into the malformed-test message" do
      report <- check defaultSettings do
        f <- Fork.spawn (assert False "fork failed before anyone joined it")
        -- Deterministically wait for the leaked fork to actually finish
        -- before letting the case end, without joining or cancelling it, so
        -- the leak-detection path sees a settled failure rather than racing
        -- a still-running thread.
        _ <- pollUntilSettled f
        pure ()
      case report.result of
        Aborted (Errored e) ->
          T.pack (displayException e) `shouldSatisfy` T.isInfixOf "fork failed before anyone joined it"
        other -> expectationFailure ("expected Aborted Errored, got: " <> show other)

  describe "clone-depth guard" do
    it "fails the case when Fork.spawn nests past maxCloneDepth" do
      let settings = defaultSettings {maxCloneDepth = 2}
          recurse :: Int -> Property ()
          recurse 0 = pure ()
          recurse n = do
            f <- Fork.spawn (recurse (n - 1))
            Fork.join f
      report <- check settings (recurse 5)
      case report.result of
        Aborted (Errored e) -> T.pack (displayException e) `shouldSatisfy` T.isInfixOf "maxCloneDepth"
        other -> expectationFailure ("expected Aborted Errored, got: " <> show other)

    it "succeeds when nesting stays within maxCloneDepth" do
      let settings = defaultSettings {maxCloneDepth = 8}
          recurse :: Int -> Property ()
          recurse 0 = pure ()
          recurse n = do
            f <- Fork.spawn (recurse (n - 1))
            Fork.join f
      report <- check settings (recurse 3)
      report.result `shouldSatisfy` isOk

    it "trips the same guard for deeply nested Branch.concurrently" do
      let settings = defaultSettings {maxCloneDepth = 2}
          recurse :: Int -> Property ()
          recurse 0 = pure ()
          recurse n = Branch.concurrently_ (recurse (n - 1)) (pure ())
      report <- check settings (recurse 5)
      case report.result of
        Aborted (Errored e) -> T.pack (displayException e) `shouldSatisfy` T.isInfixOf "maxCloneDepth"
        other -> expectationFailure ("expected Aborted Errored, got: " <> show other)

  describe "Fork.scoped" do
    it "cancels a still-running fork when the scope exits, without waiting for it" do
      reachedRef <- newIORef False
      report <- check defaultSettings do
        Fork.scoped
          (liftIO (threadDelay 200000) *> liftIO (writeIORef reachedRef True))
          (\_ -> pure ())
      report.result `shouldSatisfy` isOk
      reached <- readIORef reachedRef
      reached `shouldBe` False

    it "propagates a body failure when joined inside use" do
      report <- check defaultSettings do
        Fork.scoped (assert False "scoped body failed") Fork.join
      case report.result of
        Counterexample {message} -> message `shouldBe` "scoped body failed"
        other -> expectationFailure ("expected Counterexample, got: " <> show other)

    it "yields the cancelled outcome when polled after Fork.scoped already released it" do
      report <- check defaultSettings do
        stash <- liftIO (newIORef Nothing)
        Fork.scoped spinForever (\f -> liftIO (writeIORef stash (Just f)))
        Just f <- liftIO (readIORef stash)
        mr <- Fork.poll f
        assert (case mr of Just (Left _) -> True; _ -> False) "a fork retained past Fork.scoped is already settled"
      report.result `shouldSatisfy` isOk

    it "does not leak when the caller never explicitly joins or cancels inside use" do
      report <- check defaultSettings do
        Fork.scoped (pure (1 :: Int)) (\_ -> pure ())
      report.result `shouldSatisfy` isOk
