module SessionRecovery (spec) where

import Data.Function ((&))
import Hegel.Gen qualified as Gen
import Hegel.Outcome (Outcome (..))
import Hegel.Server.Runner (runPropertyOn)
import Hegel.Server.Session (defaultSessionConfig, withSession)
import Hegel.Server.Session.Internal (liveProcess)
import Hegel.Settings (Settings (..), defaultSettings)
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
