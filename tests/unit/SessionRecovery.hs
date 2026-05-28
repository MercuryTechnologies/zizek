module SessionRecovery (spec) where

import Data.Function ((&))
import Hegel.Gen qualified as Gen
import Hegel.Outcome (Outcome (..))
import Hegel.Runner (Settings (..), defaultSettings, runPropertyOn)
import Hegel.Session (defaultSessionConfig, withSession)
import Hegel.Session.Internal (liveProcess)
import System.Process.Typed (stopProcess)
import Test.Hspec

spec :: Spec
spec =
  it "returns Errored when child process is killed mid-run, then auto-recovers" $
    withSession defaultSessionConfig \ses -> do
      proc <- liveProcess ses
      outcome <-
        runPropertyOn
          ses
          defaultSettings {testCases = 1}
          (Gen.int & Gen.min 0 & Gen.max 100 & Gen.build)
          \_ -> stopProcess proc
      outcome `shouldSatisfy` \case
        Errored _ -> True
        _ -> False
      outcome2 <-
        runPropertyOn
          ses
          defaultSettings
          (Gen.int & Gen.min 0 & Gen.max 100 & Gen.build)
          \_ -> pure ()
      outcome2 `shouldSatisfy` \case
        Passed _ -> True
        _ -> False
