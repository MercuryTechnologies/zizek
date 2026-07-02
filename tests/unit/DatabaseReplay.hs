-- | End-to-end example-database replay and derandomization.
module DatabaseReplay (spec) where

import Data.Function ((&))
import Hegel (Gen)
import Hegel.Database (Database (..))
import Hegel.Gen qualified as Gen
import Hegel.Phase (Phase (..))
import Hegel.Property (Property, assert, assume, forAll)
import Hegel.Report (Abort (..), Report (..), Result (..), Stats (..))
import Hegel.Runner (check)
import Hegel.Settings (Settings (..), defaultSettings)
import Test.Hspec
import UnliftIO.IORef (newIORef, readIORef, writeIORef)
import UnliftIO.Temporary (withSystemTempDirectory)

intR :: (Int, Int) -> Gen Int
intR (lo, hi) = Gen.integral & Gen.min lo & Gen.max hi & Gen.build

spec :: Spec
spec = do
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
      r1.result `shouldSatisfy` isCounterexample
      -- With generation disabled, only the stored example can fail it again.
      r2 <- check settings {phases = [Explicit, Reuse, Shrink]} failing
      r2.result `shouldSatisfy` isCounterexample

  it "a failure the reconstruction replay cannot reproduce surfaces as ReplayDiverged" $ do
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
    case r.result of
      Aborted (ReplayDiverged _) -> pure ()
      other -> expectationFailure ("expected ReplayDiverged, got: " <> show other)

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

isCounterexample :: Result -> Bool
isCounterexample = \case
  Counterexample {} -> True
  _ -> False
