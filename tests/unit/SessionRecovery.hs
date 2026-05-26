module SessionRecovery (spec) where

import Control.Concurrent.Async (async)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, tryPutMVar)
import Data.Function ((&))
import Hegel (runProperty)
import Hegel.Generators.Integer qualified as Integer
import Hegel.Outcome (Outcome (..))
import Hegel.Runner (Settings (..), defaultSettings)
import Hegel.Session (Session (..), getOrInitSession, invalidateSession)
import System.Process.Typed (stopProcess)
import Test.Hspec

-- | Killing the server mid-run should return 'Errored' rather than hanging.
-- The first test-case body signals a MVar; the killer async waits for that
-- signal so the kill is guaranteed to arrive while the run is active.
spec :: Spec
spec = before_ invalidateSession $
  it "returns Errored when child process is killed mid-run" $ do
    ses <- getOrInitSession
    started <- newEmptyMVar
    _ <- async do
      takeMVar started
      stopProcess ses.process
    outcome <-
      runProperty
        defaultSettings {testCases = 10_000}
        (Integer.gen $ Integer.integers @Int & Integer.withRange (0, 100))
        \_ -> tryPutMVar started () >> pure ()
    outcome `shouldSatisfy` \case
      Errored _ -> True
      _ -> False
