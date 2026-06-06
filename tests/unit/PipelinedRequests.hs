module PipelinedRequests (spec) where

import Control.Concurrent.Async (concurrently)
import Data.Function ((&))
import Hegel.Gen qualified as Gen
import Hegel.Outcome (Outcome (..))
import Hegel.Server.Runner (runPropertyOn)
import Hegel.Server.Session (defaultSessionConfig, withSession)
import Hegel.Settings (defaultSettings)
import Test.Hspec

spec :: Spec
spec = do
  it "two concurrent runPropertyOn calls on the same session both succeed" $
    -- Both tests issue run_test requests on the shared control stream via
    -- requestCborPending. With the old MVar-per-stream design this could
    -- serialize arbitrarily; with per-field TVars they pipeline freely.
    withSession defaultSessionConfig \ses -> do
      let go = runPropertyOn ses defaultSettings (Gen.int & Gen.min 0 & Gen.max 99 & Gen.build) $
            \_ -> pure ()
      (r1, r2) <- concurrently go go
      r1 `shouldSatisfy` \case Passed _ -> True; _ -> False
      r2 `shouldSatisfy` \case Passed _ -> True; _ -> False
