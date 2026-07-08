-- | Engine-managed pools of values for stateful tests.
--
-- A 'Pool' holds a set of previously generated values that rules can draw from
-- during a stateful run. The engine picks which value to hand out on each draw,
-- so pool references shrink like any other choice: a minimal counterexample
-- shows the smallest set of values that triggers the failure.
--
-- Usage:
--
-- > pool <- Pool.new
-- > Pool.add pool someValue
-- > Pool.add pool anotherValue
-- >
-- > -- In a rule body:
-- > v <- forAll (Pool.reuse pool)     -- does not remove from pool
-- > v <- forAll (Pool.consume pool)   -- removes from pool
--
-- Drawing from an empty pool discards the current test case (equivalent to
-- @assume False@); the run is tallied as 'Invalid', not a failure.
module Hegel.Pool
  ( -- * Handle
    Pool,

    -- * Construction
    new,
    named,

    -- * Mutation
    add,

    -- * Queries
    size,
    isEmpty,

    -- * Generators
    reuse,
    consume,
    transfer,
  )
where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Hegel.Gen.Internal (Gen (..))
import Hegel.Internal.Control (AssumeRejected (..))
import Hegel.Internal.DataSource (labelPool, newPool, poolAdd, poolAddFrom, poolGenerate)
import Hegel.Internal.Event (Var (..))
import Hegel.Internal.TestCase (TestCase)
import Hegel.Property.Internal (Env (..), PropertyT, askEnv)
import UnliftIO.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)

-- | Opaque handle to a @libhegel@-managed pool of values of type @a@.
--
-- Holds a reference to the live 'TestCase', the engine-assigned pool id, and
-- a local mirror of the currently-live values (keyed by engine variable id).
data Pool a = Pool
  { tc :: !TestCase,
    poolId :: !Int,
    values :: !(IORef (IntMap a))
  }

-- | Create a new pool against the running property's test case. Allocates a
-- pool id from the engine immediately.
--
-- The failure report auto-names the pool's values (@v₁, w₁, ...@ by birth
-- order); use 'named' when a semantic letter (@h₁@ for handles) reads
-- better.
new :: (MonadIO m) => PropertyT m (Pool a)
new = do
  env <- askEnv
  let tc = env.testCase
  liftIO do
    pid <- newPool tc
    ref <- newIORef IntMap.empty
    pure Pool {tc, poolId = pid, values = ref}

-- | 'new' with a display label for the failure report: values of a pool
-- named @"h"@ render as @h₁, h₂, ...@ in the event log.
named :: (MonadIO m) => Text -> PropertyT m (Pool a)
named label = do
  pool <- new
  liftIO (labelPool pool.tc pool.poolId label)
  pure pool

-- | Add a value to the pool. The engine assigns the variable id.
add :: Pool a -> a -> IO ()
add pool v = do
  vid <- poolAdd pool.tc pool.poolId
  modifyIORef' pool.values (IntMap.insert vid v)

-- | Number of values currently in the pool.
size :: Pool a -> IO Int
size pool = IntMap.size <$> readIORef pool.values

-- | Is the pool currently empty?
isEmpty :: Pool a -> IO Bool
isEmpty pool = IntMap.null <$> readIORef pool.values

-- | A generator over values in the pool that does not remove them.
--
-- The engine picks the variable id so the choice shrinks like any other draw.
-- Drawing from an empty pool discards the current test case.
reuse :: Pool a -> Gen a
reuse pool = Draw \tc -> do
  vals <- readIORef pool.values
  if IntMap.null vals
    then throwIO AssumeRejected
    else do
      vid <- poolGenerate tc pool.poolId False
      case IntMap.lookup vid vals of
        Just v -> pure v
        Nothing ->
          -- Engine returned a variable id that was never added — engine-contract
          -- violation, not a user error.
          error ("Hegel.Pool.reuse: unknown variable id " <> show vid)

-- | A generator that consumes values from the pool, removing each yielded
-- value so it is never drawn again.
--
-- Drawing from an empty pool discards the current test case.
consume :: Pool a -> Gen a
consume pool = Draw \tc -> snd <$> drawConsuming "consume" pool tc

-- | The consuming draw shared by 'consume' and 'transfer': draw a
-- vid from the engine (removing it there) and pop the mirrored value.
--
-- Throws 'AssumeRejected' when the pool is empty, discarding the test case.
drawConsuming :: String -> Pool a -> TestCase -> IO (Int, a)
drawConsuming caller pool tc = do
  empty <- IntMap.null <$> readIORef pool.values
  if empty
    then throwIO AssumeRejected
    else do
      vid <- poolGenerate tc pool.poolId True
      v <- atomicModifyIORef' pool.values \m ->
        case IntMap.updateLookupWithKey (\_ _ -> Nothing) vid m of
          (Just v, m') -> (m', v)
          (Nothing, _) ->
            -- Engine returned a variable id that was never added —
            -- engine-contract violation, not a user error.
            error ("Hegel.Pool." <> caller <> ": unknown variable id " <> show vid)
      pure (vid, v)

-- | A generator that moves a value from one pool to another: a consuming
-- draw from @src@ whose value is immediately registered in @dst@, with the
-- identity link /declared/ in the event stream — the failure report renders
-- the value as one continuous lifeline across both pools rather than two
-- unrelated ones.
--
-- This is the honest way to model state changes like closing a handle
-- (consume from the open pool, transfer into the closed pool): a manual
-- @'consume' ... 'add'@ pair works but severs the value's story.
--
-- No engine primitive is involved beyond the same draw + add; the link is
-- zizek-side bookkeeping. Drawing from an empty @src@ discards the test
-- case.
transfer :: Pool a -> Pool a -> Gen a
transfer src dst = Draw \tc -> do
  (vid, v) <- drawConsuming "transfer" src tc
  vid' <- poolAddFrom tc dst.poolId Var {pool = src.poolId, id = vid}
  modifyIORef' dst.values (IntMap.insert vid' v)
  pure v
