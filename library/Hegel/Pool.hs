-- | Engine-managed pools of values for stateful tests.
--
-- A 'Pool' holds a set of previously generated values that rules can draw from
-- during a stateful run. The engine picks which value to hand out on each draw,
-- so pool references shrink like any other choice.
--
-- Usage:
--
-- > pool <- Pool.new
-- > Pool.add pool someValue
-- > Pool.add pool anotherValue
-- >
-- > -- In a rule body:
-- > x <- forAll (Pool.reuse pool)     -- does not remove from pool
-- > y <- forAll (Pool.consume pool)   -- removes from pool
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

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar, withMVar)
import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Foreign (Ptr)
import Hegel.Gen.Internal (Gen (..))
import Hegel.Internal.Control (AssumeRejected (..))
import Hegel.Internal.DataSource (HegelPool, freePool, freshPoolIdentity, labelPool, newPool, poolAdd, poolAddFrom, poolGenerate)
import Hegel.Internal.Event (Var (..))
import Hegel.Internal.TestCase (TestCase)
import Hegel.Property.Internal (Env (..), PropertyT, askEnv, resource)

-- | Opaque handle to a @libhegel@-managed pool of values of type @a@.
data Pool a = Pool
  { handle :: !(Ptr HegelPool),
    identity :: !Int,
    values :: !(MVar (IntMap a))
  }

-- | Create a new pool against the running property's test case.
--
-- Like 'resource', this throws 'Hegel.Internal.Control.MalformedTest' if called
-- from within a stateful rule's @apply@ or an invariant's @check@.
--
-- Pools should be created in a 'Hegel.Stateful.Machine'\'s @initial@ or in a
-- plain property body instead, and rules should only move values into and out
-- of them.
--
-- The failure report automatically names the pool's values @v₁, w₁, ...@ based
-- on their birth order; use 'named' to assign a name directly upon creation.
new :: (MonadIO m) => PropertyT m (Pool a)
new = do
  env <- askEnv
  resource
    ( do
        handle <- newPool env.testCase
        identity <- freshPoolIdentity
        values <- newMVar IntMap.empty
        pure Pool {handle, identity, values}
    )
    (\pool -> freePool env.testCase pool.handle)

-- | 'new' with a display label for the failure report: values of a pool
-- named @"h"@ render as @h₁, h₂, ...@ in the event log.
named :: (MonadIO m) => Text -> PropertyT m (Pool a)
named label = do
  pool <- new
  env <- askEnv
  liftIO (labelPool env.testCase pool.identity label)
  pure pool

-- | Add a value to the pool. The engine assigns the variable id.
add :: (MonadIO m) => Pool a -> a -> PropertyT m ()
add pool v = do
  env <- askEnv
  liftIO $ modifyMVar_ pool.values \m -> do
    vid <- poolAdd env.testCase pool.handle pool.identity
    pure (IntMap.insert vid v m)

-- | Number of values currently in the pool.
size :: Pool a -> IO Int
size pool = IntMap.size <$> readMVar pool.values

-- | Is the pool currently empty?
isEmpty :: Pool a -> IO Bool
isEmpty pool = IntMap.null <$> readMVar pool.values

-- | A generator over values in the pool that does not remove them.
--
-- The engine picks the variable id so the choice shrinks like any other draw.
-- Drawing from an empty pool discards the current test case.
reuse :: Pool a -> Gen a
reuse pool = Draw \tc ->
  withMVar pool.values \vals ->
    if IntMap.null vals
      then throwIO AssumeRejected
      else do
        vid <- poolGenerate tc pool.handle pool.identity False
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
drawConsuming caller pool tc =
  modifyMVar pool.values \m ->
    if IntMap.null m
      then throwIO AssumeRejected
      else do
        vid <- poolGenerate tc pool.handle pool.identity True
        case IntMap.updateLookupWithKey (\_ _ -> Nothing) vid m of
          (Just v, m') -> pure (m', (vid, v))
          (Nothing, _) ->
            -- Engine returned a variable id that was never added —
            -- engine-contract violation, not a user error.
            error ("Hegel.Pool." <> caller <> ": unknown variable id " <> show vid)

-- | A generator that moves a value from one pool to another: a consuming
-- draw from @src@ whose value is immediately registered in @dst@, with the
-- identity link /declared/ in the event stream.
--
-- Use this this when modeling state changes, such as closing a handle to some
-- resource.
transfer :: Pool a -> Pool a -> Gen a
transfer src dst = Draw \tc -> do
  (vid, v) <- drawConsuming "transfer" src tc
  modifyMVar_ dst.values \m -> do
    vid' <- poolAddFrom tc dst.handle dst.identity Var {pool = src.identity, id = vid}
    pure (IntMap.insert vid' v m)
  pure v
