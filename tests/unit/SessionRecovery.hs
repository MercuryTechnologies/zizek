module SessionRecovery (spec) where

import Control.Concurrent.Async (async)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, tryPutMVar)
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
      started <- newEmptyMVar
      _ <- async do
        takeMVar started
        stopProcess proc
      outcome <-
        runPropertyOn
          ses
          defaultSettings {testCases = 10_000}
          (Gen.integer @Int & Gen.min 0 & Gen.max 100 & Gen.build)
          \_ -> tryPutMVar started () >> pure ()
      outcome `shouldSatisfy` \case
        Errored _ -> True
        _ -> False
      outcome2 <-
        runPropertyOn
          ses
          defaultSettings
          (Gen.integer @Int & Gen.min 0 & Gen.max 100 & Gen.build)
          \_ -> pure ()
      outcome2 `shouldSatisfy` \case
        Passed _ -> True
        _ -> False
