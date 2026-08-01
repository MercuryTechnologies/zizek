-- | Unit tests for 'Hegel.Internal.TestCase.withClone'.
--
-- Deliberately does not attempt to trigger 'HEGEL_E_CONCURRENT_USE' or
-- 'HEGEL_E_ALREADY_COMPLETE' from Haskell: both are genuine misuse/race
-- paths that would need dedicated machinery (a controlled race, or
-- completing the family mid-property) this pass doesn't need to build.
-- 'throwOnError'\'s existing, already-shared fallback path handles them
-- correctly regardless.
module TestCaseClone (spec) where

import Control.Concurrent.Async (concurrently_)
import Control.Monad (replicateM_)
import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (def)
import Data.Function ((&))
import Data.Text (Text)
import Hegel (Gen)
import Hegel.Database (Database (..))
import Hegel.Gen qualified as Gen
import Hegel.Gen.Internal (draw)
import Hegel.Internal.TestCase (TestCase (..))
import Hegel.Internal.TestCase qualified as TestCase
import Hegel.Phase (Phase (..))
import Hegel.Property (Property, annotateShow, assert, check)
import Hegel.Property.Internal (Env (..), askEnv)
import Hegel.Report (Note (..), NoteKind (..), Report (..), Result (..))
import Hegel.Settings (Settings (..), defaultSettings)
import Test.Hspec
import UnliftIO.Temporary (withSystemTempDirectory)

intGen :: Gen Int
intGen = Gen.int & Gen.min 0 & Gen.max 1000 & Gen.build

isOk :: Result -> Bool
isOk = \case
  Ok -> True
  _ -> False

-- | The rendered text of every 'Annotation' note in a counterexample's
-- journal, in this test's case the value drawn from a clone via
-- 'annotateShow'. Empty for any other 'Result'.
annotatedValues :: Result -> [Text]
annotatedValues = \case
  Counterexample {notes} -> [n.text | n <- notes, n.kind == Annotation]
  _ -> []

spec :: Spec
spec = describe "TestCase.withClone" do
  it "gives the clone its own event and draw buffers, not the source's" do
    report <- check def do
      env <- askEnv
      liftIO $ TestCase.withClone env.testCase \cloned -> do
        (cloned.events == env.testCase.events) `shouldBe` False
        (cloned.draws == env.testCase.draws) `shouldBe` False
    report.result `shouldSatisfy` isOk

  it "draws independently from the clone" do
    report <- check def do
      env <- askEnv
      liftIO $ TestCase.withClone env.testCase \cloned -> do
        _ <- draw cloned intGen
        pure ()
    report.result `shouldSatisfy` isOk

  it "drives the source and its clone concurrently with no HEGEL_E_CONCURRENT_USE" do
    report <- check def do
      env <- askEnv
      liftIO $ TestCase.withClone env.testCase \cloned ->
        concurrently_
          (replicateM_ 200 (draw env.testCase intGen))
          (replicateM_ 200 (draw cloned intGen))
    report.result `shouldSatisfy` isOk

  it "repeated clone/free cycles against one source keep working" do
    report <- check def do
      env <- askEnv
      liftIO $
        replicateM_ 20 $
          TestCase.withClone env.testCase \cloned -> do
            _ <- draw cloned intGen
            pure ()
    report.result `shouldSatisfy` isOk

  it "shrinks and replays deterministically through a cloned draw" $
    withSystemTempDirectory "zizek-clone-replay" \dbDir -> do
      let settings =
            defaultSettings
              { database = DatabaseDirectory dbDir,
                databaseKey = Just "test-case-clone-replay-spec"
              }
          failing :: Property ()
          failing = do
            env <- askEnv
            v <- liftIO $ TestCase.withClone env.testCase \cloned -> draw cloned intGen
            annotateShow v
            assert (v < 42) "clone-drawn value stays small"
      r1 <- check settings failing
      -- The boundary shrinks to the smallest failing value, exactly as an
      -- ordinary forAll-drawn value would (see BasicProperties.hs).
      annotatedValues r1.result `shouldBe` ["42"]
      -- With generation disabled, only the stored blob can fail it again, and
      -- only by reconstructing the same cloned choice sequence. Comparing
      -- against r1's own value directly, rather than a second hardcoded
      -- "42", is the sharper claim: replay must reproduce THIS run's value,
      -- not merely land on the same boundary independently.
      r2 <- check settings {phases = [Explicit, Reuse, Shrink]} failing
      annotatedValues r2.result `shouldBe` annotatedValues r1.result
