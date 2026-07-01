-- | Engine-managed pools of values for stateful tests.
--
-- A 'Pool' holds a set of previously generated values that rules can draw from
-- during a stateful run. The engine picks which value to hand out on each draw,
-- so pool references shrink like any other choice: a minimal counterexample
-- shows the smallest set of values that triggers the failure.
--
-- Usage:
--
-- > pool <- Pool.new tc
-- > Pool.add pool someValue
-- > Pool.add pool anotherValue
-- >
-- > -- In a rule body:
-- > v <- forAll (Pool.valuesReusable pool)   -- does not remove from pool
-- > v <- forAll (Pool.valuesConsumed pool)   -- removes from pool
--
-- Drawing from an empty pool discards the current test case (equivalent to
-- @assume False@); the run is tallied as 'Invalid', not a failure.
module Hegel.Pool
  ( -- * Handle
    Pool,

    -- * Construction
    new,

    -- * Mutation
    add,

    -- * Queries
    size,
    isEmpty,

    -- * Generators
    valuesReusable,
    valuesConsumed,
  )
where

import Control.Exception (throwIO)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Hegel.Gen.Internal (Gen (..))
import Hegel.Internal.Control (AssumeRejected (..))
import Hegel.Internal.DataSource (newPool, poolAdd, poolGenerate)
import Hegel.Internal.TestCase (TestCase)
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

-- | Create a new pool. Allocates a pool id from the engine immediately.
new :: TestCase -> IO (Pool a)
new tc = do
  pid <- newPool tc
  ref <- newIORef IntMap.empty
  pure Pool {tc, poolId = pid, values = ref}

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
valuesReusable :: Pool a -> Gen a
valuesReusable pool = Draw \tc -> do
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
          error ("Hegel.Pool.valuesReusable: unknown variable id " <> show vid)

-- | A generator that consumes values from the pool, removing each yielded
-- value so it is never drawn again.
--
-- Drawing from an empty pool discards the current test case.
valuesConsumed :: Pool a -> Gen a
valuesConsumed pool = Draw \tc -> do
  empty <- IntMap.null <$> readIORef pool.values
  if empty
    then throwIO AssumeRejected
    else do
      vid <- poolGenerate tc pool.poolId True
      atomicModifyIORef' pool.values \m ->
        case IntMap.updateLookupWithKey (\_ _ -> Nothing) vid m of
          (Just v, m') -> (m', v)
          (Nothing, _) ->
            error ("Hegel.Pool.valuesConsumed: unknown variable id " <> show vid)
