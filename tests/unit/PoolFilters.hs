-- | Pool draws under 'Gen.filtered' and 'Gen.mapMaybe': a filter consumes only
-- the value it keeps, and the values its rejected attempts took go back.
module PoolFilters (spec) where

import Control.Exception (fromException)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (def)
import Data.Foldable (for_)
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sort)
import Data.Text (Text)
import Hegel.Exception (Diagnostic (..), MalformedTest (..))
import Hegel.Gen qualified as Gen
import Hegel.Gen.Internal (Gen (Draw))
import Hegel.Internal.DataSource (Label (LabelJust), spanLabel)
import Hegel.Pool (Pool)
import Hegel.Pool qualified as Pool
import Hegel.Property (PropertyT, assert, forAll, (===))
import Hegel.Report (Abort (..), Event (..), FailureEvidence (..), Operation (..), Report (..), Result (..), Stats (..))
import Hegel.Runner (check)
import Hegel.Settings (Settings (..))
import Hegel.Stateful qualified as Stateful
import Test.Hspec
import TestSupport (expectCaptured)

expectOk :: Report -> Expectation
expectOk report = case report.result of
  Ok -> pure ()
  other -> expectationFailure ("expected Ok, got: " <> show other)

-- | Two pools, the first holding only odd values and the second empty, so
-- 'rejectingMachine''s filtering rules always reject their step.
data Pools = Pools {odds :: Pool Int, empty :: Pool Int}

-- | A machine whose filtering rules always reject, and every rule of which
-- first checks that no earlier rejection left a value consumed.
rejectingMachine :: Stateful.Machine Pools IO
rejectingMachine =
  Stateful.Machine
    { initial = do
        odds <- Pool.new
        for_ [1, 3, 5] (Pool.add odds)
        empty <- Pool.new
        pure Pools {odds, empty},
      rules =
        [ checked "exhaust_filter" \ps -> () <$ forAll (Gen.filtered even (Pool.consume ps.odds)),
          checked "draw_from_empty" \ps ->
            () <$ forAll (Gen.filtered (const True) ((,) <$> Pool.consume ps.odds <*> Pool.consume ps.empty)),
          checked "noop" \_ -> pure ()
        ],
      stepCount = Stateful.defaultStepCount,
      invariants = []
    }
  where
    checked :: Text -> (Pools -> PropertyT IO ()) -> Stateful.Rule Pools IO
    checked name body = Stateful.rule name \ps -> do
      remaining <- liftIO (Pool.size ps.odds)
      remaining === 3
      body ps
      pure ps

spec :: Spec
spec = describe "pool draws under a filter" do
  it "consume only the value the filter keeps" do
    report <- check def {testCases = Just 50} do
      pool <- Pool.new
      for_ [1 .. 5 :: Int] (Pool.add pool)
      kept <- forAll (Gen.filtered even (Pool.consume pool))
      remaining <- liftIO (Pool.size pool)
      remaining === 4
      rest <- replicateM 4 (forAll (Pool.consume pool))
      sort (kept : rest) === [1 .. 5]
    expectOk report

  -- With two odd values and one even value, three attempts that never redraw
  -- a rejected value always reach the even one.
  it "never redraw a value an earlier attempt rejected" do
    report <- check def {testCases = Just 50} do
      pool <- Pool.new
      for_ [1, 3, 6 :: Int] (Pool.add pool)
      kept <- forAll (Gen.filtered even (Pool.consume pool))
      kept === 6
    expectOk report
    report.stats.invalid `shouldBe` 0

  it "put back every value of a rejected attempt that drew several" do
    report <- check def {testCases = Just 50} do
      pool <- Pool.new
      for_ [1 .. 6 :: Int] (Pool.add pool)
      (a, b) <- forAll (Gen.filtered (\(a, b) -> even (a + b)) ((,) <$> Pool.consume pool <*> Pool.consume pool))
      assert (even (a + b)) "the kept pair has an even sum"
      remaining <- liftIO (Pool.size pool)
      remaining === 4
    expectOk report

  it "put back what an outer filter rejects after an inner filter kept it" do
    report <- check def {testCases = Just 50} do
      pool <- Pool.new
      for_ [1 .. 6 :: Int] (Pool.add pool)
      kept <- forAll (Gen.filtered (> 2) (Gen.filtered even (Pool.consume pool)))
      assert (kept `elem` [4, 6]) "the kept value passes both filters"
      remaining <- liftIO (Pool.size pool)
      remaining === 5
    expectOk report

  it "put back every value when the filter runs out of attempts or a draw rejects" do
    report <- check def (Stateful.run rejectingMachine)
    expectOk report

  -- Each attempt bumps a counter, so only a case with a rejected attempt fails
  -- and supplies the captured event stream.
  it "record a Restored event after each rejected value's Consumed event" do
    tries <- newIORef (0 :: Int)
    let attempt = Draw (spanLabel LabelJust) \_ -> atomicModifyIORef' tries \n -> (n + 1, ())
    report <- check def do
      liftIO (writeIORef tries 0)
      pool <- Pool.new
      for_ [1 .. 6 :: Int] (Pool.add pool)
      _ <- forAll (Gen.filtered even (Pool.consume pool <* attempt))
      n <- liftIO (readIORef tries)
      assert (n == 1) "the filter kept its first attempt"
    evidence <- expectCaptured report.result
    let restored = [e | e <- evidence.events, e.kind == Restored]
        consumedBefore r = any (\e -> e.kind == Consumed && e.var == r.var && e.clock < r.clock) evidence.events
    restored `shouldSatisfy` (not . null)
    for_ restored \r -> r `shouldSatisfy` consumedBefore
    length [() | e <- evidence.events, e.kind == Consumed] `shouldBe` length restored + 1

  it "refuse a transfer drawn inside a filter attempt" do
    report <- check def do
      src <- Pool.new
      dst <- Pool.new
      Pool.add src (1 :: Int)
      _ <- forAll (Gen.filtered (const True) (Pool.transfer src dst))
      pure ()
    case report.result of
      Aborted (Errored e)
        | Just (MalformedTest d) <- fromException e -> d.context `shouldBe` "Hegel.Pool.transfer"
      other -> expectationFailure ("expected a MalformedTest abort, got: " <> show other)
