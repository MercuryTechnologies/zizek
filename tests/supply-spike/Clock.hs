-- | A TTL cache driven by a fake clock that advances by a drawn step on every
-- read.
module Clock (spec) where

import Control.Monad (when)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime (..), addUTCTime, diffUTCTime, fromGregorian)
import Hegel.Gen qualified as Gen
import Hegel.Property (assert)
import Hegel.Supply (Supply)
import Hegel.Supply qualified as Supply
import Support (viable)
import Test.Hspec
import UnliftIO.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)

-- | A clock that moves forward by a drawn multiple of 15 seconds, never
-- backwards, on every read.
--
-- A coarse step keeps exact boundaries like the TTL within reach of random
-- generation, which a fine-grained duration would almost never land on.
newFakeClock :: Supply -> IO (IO UTCTime)
newFakeClock supply = do
  ref <- newIORef (UTCTime (fromGregorian 2026 1 1) 0)
  pure do
    step <- Supply.draw supply "tick" ((* 15) <$> (Gen.int & Gen.min 0 & Gen.max 8 & Gen.build))
    atomicModifyIORef' ref \t -> let t' = addUTCTime (fromIntegral step) t in (t', t')

data Cache = Cache
  { now :: IO UTCTime,
    entries :: IORef (Map Text (UTCTime, Int)),
    misses :: IORef [UTCTime]
  }

ttl :: NominalDiffTime
ttl = 60

insert :: Cache -> Text -> Int -> IO ()
insert cache k v = do
  t <- cache.now
  modifyIORef' cache.entries (Map.insert k (t, v))

-- | Look a key up, returning the time of the lookup alongside the result.
--
-- The bug: an entry exactly 'ttl' old should have expired, but still hits.
-- An expired lookup also logs the miss with a second clock read, so the
-- number of reads depends on the drawn steps.
lookup' :: Cache -> Text -> IO (NominalDiffTime, Maybe Int)
lookup' cache k = do
  t <- cache.now
  Map.lookup k <$> readIORef cache.entries >>= \case
    Just (at, v)
      | diffUTCTime t at <= ttl -> pure (diffUTCTime t at, Just v)
      | otherwise -> do
          logged <- cache.now
          modifyIORef' cache.misses (logged :)
          pure (diffUTCTime t at, Nothing)
    Nothing -> pure (0, Nothing)

spec :: Spec
spec = describe "a TTL cache on a fake clock" do
  it "finds and shrinks the expiry off-by-one to a step of exactly the TTL" do
    drawn <- viable do
      lookups <- Supply.withSupply \supply -> do
        clock <- newFakeClock supply
        cache <- Cache clock <$> newIORef Map.empty <*> newIORef []
        insert cache "k" 1
        traverse (const (lookup' cache "k")) [1 :: Int .. 3]
      for_ lookups \(age, result) ->
        when (age >= ttl) (assert (not (isJust result)) "an entry at least ttl old was still served")
    drawn `shouldBe` ["tick=0", "tick=0", "tick=0", "tick=60"]
