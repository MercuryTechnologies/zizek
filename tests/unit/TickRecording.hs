-- | Unit tests for 'Hegel.Internal.Tick' recording into a shared buffer.
module TickRecording (spec) where

import Control.Concurrent.Async (forConcurrently_)
import Control.Monad (replicateM_)
import Data.List (sort)
import Data.Sequence qualified as Seq
import Hegel.Internal.Tick qualified as Tick
import Test.Hspec
import UnliftIO.IORef (newIORef)

spec :: Spec
spec = describe "Tick.record" do
  it "loses no entries when many threads record into one buffer" do
    let threads = 16
        perThread = 5000 :: Int
    rec <- Tick.newRecording
    buffer <- newIORef Seq.empty
    forConcurrently_ [1 .. threads] \t ->
      replicateM_ perThread (Tick.record rec buffer \tick -> (t :: Int, tick))
    entries <- Tick.drain buffer
    length entries `shouldBe` threads * perThread
    sort (map snd entries) `shouldBe` map Tick.Tick [0 .. threads * perThread - 1]
