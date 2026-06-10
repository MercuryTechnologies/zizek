-- | End-to-end example-database replay and derandomization.
module DatabaseReplay (spec) where

import Data.Function ((&))
import Hegel (Gen)
import Hegel.Database (Database (..))
import Hegel.Gen qualified as Gen
import Hegel.Phase (Phase (..))
import Hegel.Property (assert, assume, forAll)
import Hegel.Report (Report (..), Result (..), Stats (..))
import Hegel.Settings (Settings (..), defaultSettings)
import Test.Hspec
import TestRunner (Checker, checkWith)
import UnliftIO.Temporary (withSystemTempDirectory)

intR :: (Int, Int) -> Gen Int
intR (lo, hi) = Gen.integral & Gen.min lo & Gen.max hi & Gen.build

spec :: String -> Checker -> Spec
spec backend checker = do
  it "replays stored failures via the Reuse phase" $
    withSystemTempDirectory ("zizek-replay-" <> backend) \dbDir -> do
      let settings =
            defaultSettings
              { database = DatabaseDirectory dbDir,
                databaseKey = Just "database-replay-spec"
              }
          failing = do
            x <- forAll (intR (0, 1000))
            assert (x < 100) "stays small"
      r1 <- checkWith checker settings failing
      r1.result `shouldSatisfy` isCounterexample
      -- With generation disabled, only the stored example can fail it again.
      r2 <- checkWith checker settings {phases = [Explicit, Reuse, Shrink]} failing
      r2.result `shouldSatisfy` isCounterexample

  it "derandomize makes keyed runs deterministic" $ do
    let settings =
          defaultSettings
            { derandomize = True,
              databaseKey = Just "derandomize-spec"
            }
        go = checkWith checker settings do
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
