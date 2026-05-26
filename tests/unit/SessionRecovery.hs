module SessionRecovery (sessionRecoveryTest) where

import Control.Concurrent.Async (async)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, tryPutMVar)
import Data.Function ((&))
import Hegel (runProperty)
import Hegel.Generators.Integer qualified as Integer
import Hegel.Outcome (Outcome (..))
import Hegel.Runner (Settings (..), defaultSettings)
import Hegel.Session (Session (..), getOrInitSession)
import System.Process.Typed (stopProcess)

-- | Verify that killing the server mid-run returns 'Errored' rather than
-- hanging. The first test-case body signals a MVar; the killer async waits
-- for that signal so the kill is guaranteed to arrive while the run is
-- active, regardless of machine speed.
sessionRecoveryTest :: IO ()
sessionRecoveryTest = do
  putStrLn "Running session recovery test (process killed mid-run)..."
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
  case outcome of
    Errored _ -> putStrLn "PASSED"
    other -> error $ "sessionRecoveryTest: expected Errored, got " <> show other
