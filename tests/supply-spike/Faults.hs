-- | A key-value store whose writes fail as the 'Supply' decides, under a
-- client with retry logic.
module Faults (spec) where

import Control.Monad (when)
import Data.Foldable (for_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Gen qualified as Gen
import Hegel.Property (assert)
import Hegel.Supply (Supply)
import Hegel.Supply qualified as Supply
import Support (viable)
import Test.Hspec
import UnliftIO.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)

data Outcome = Success | Timeout | Conflict
  deriving stock (Eq, Show)

data Store = Store
  { put :: Text -> Int -> IO Outcome,
    contents :: IORef (Map Text Int)
  }

-- | A store where a write usually succeeds, and a timed-out or conflicting
-- write is dropped.
--
-- Faults come up often enough that random generation reaches a specific
-- sequence of them within a few dozen cases.
newStore :: Supply -> IO Store
newStore supply = do
  contents <- newIORef Map.empty
  counter <- newIORef (0 :: Int)
  let put k v = do
        n <- atomicModifyIORef' counter \c -> (c + 1, c + 1)
        outcome <- Supply.draw supply ("put#" <> T.pack (show n)) (Gen.frequency [(3, pure Success), (1, pure Timeout), (1, pure Conflict)])
        when (outcome == Success) (modifyIORef' contents (Map.insert k v))
        pure outcome
  pure Store {put, contents}

-- | Write a key, retrying a timeout up to twice more.
--
-- The bug: a conflict right after a timeout is taken to mean the timed-out
-- write landed after all.
putReliably :: Store -> Text -> Int -> IO Bool
putReliably store k v = go False (3 :: Int)
  where
    go timedOut attempts =
      store.put k v >>= \case
        Success -> pure True
        Timeout | attempts > 1 -> go True (attempts - 1)
        Conflict | timedOut -> pure True
        _ -> pure False

spec :: Spec
spec = describe "a fault-injecting store under a retrying client" do
  it "finds and shrinks the bug to a timeout followed by a conflict" do
    drawn <- viable do
      (acks, stored) <- Supply.withSupply \supply -> do
        store <- newStore supply
        acks <- traverse (\(k, v) -> (,) k <$> putReliably store k v) (zip ["a", "b", "c"] [1 ..])
        (,) acks <$> readIORef store.contents
      for_ acks \(k, acked) ->
        when acked (assert (Map.member k stored) ("the client acknowledged a write to " <> k <> " that never landed"))
    drawn `shouldBe` ["put#1=Success", "put#2=Success", "put#3=Timeout", "put#4=Conflict"]
