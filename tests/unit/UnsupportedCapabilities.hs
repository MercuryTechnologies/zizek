module UnsupportedCapabilities (spec) where

import Data.Function ((&))
import Hegel.Gen qualified as Gen
import Hegel.Gen.Internal (Gen (Draw))
import Hegel.Report (Abort (..), Report (..), Result (..))
import Hegel.Server.Runner (runPropertyOn)
import Hegel.Server.Session (defaultSessionConfig, withSession)
import Hegel.Settings (defaultSettings)
import Hegel.TestCase (UnsupportedCapability (..))
import Test.Hspec
import UnliftIO.Exception (throwIO)

spec :: Spec
spec =
  it "maps UnsupportedCapability to Errored, then the session recovers" $
    withSession defaultSessionConfig \ses -> do
      -- A generator that uses a primitive the backend does not implement
      -- surfaces the typed exception during `draw`.
      let unsupported :: Gen ()
          unsupported = Draw \_ -> throwIO (UnsupportedCapability "test")
      report <-
        runPropertyOn ses defaultSettings unsupported \_ -> pure ()
      report.result `shouldSatisfy` \case
        Aborted (Errored _) -> True
        _ -> False
      -- The handler invalidates the session (the case aborted mid-protocol), so
      -- confirm a subsequent run auto-recovers and passes.
      report2 <-
        runPropertyOn
          ses
          defaultSettings
          (Gen.int & Gen.min 0 & Gen.max 100 & Gen.build)
          \_ -> pure ()
      report2.result `shouldSatisfy` \case
        Ok -> True
        _ -> False
