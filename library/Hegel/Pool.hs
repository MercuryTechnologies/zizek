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
-- Drawing from an empty pool is equivalent to @assume False@: inside a
-- stateful rule's body it skips just that step, without discarding the
-- case; anywhere else it discards the whole test case, tallied as
-- 'Invalid' rather than a failure.
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
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Data.Text qualified as T
import Foreign (Ptr)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hegel.Exception (InvariantViolation (..))
import Hegel.Gen.Internal (Gen (..))
import Hegel.Internal.Control (AssumeRejected (..), malformedTest)
import Hegel.Internal.DataSource (HegelPool, Label (LabelPoolConsume, LabelPoolReuse, LabelPoolTransfer), freePool, freshPoolIdentity, labelPool, newPool, poolAdd, poolAddFrom, poolGenerate, poolRestore, spanLabel)
import Hegel.Internal.Event (Var (..))
import Hegel.Internal.TestCase (TestCase)
import Hegel.Internal.TestCase qualified as TestCase
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
new :: (HasCallStack, MonadIO m) => PropertyT m (Pool a)
new = withFrozenCallStack $ do
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
named :: (HasCallStack, MonadIO m) => Text -> PropertyT m (Pool a)
named label = withFrozenCallStack $ do
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
-- Drawing from an empty pool is equivalent to @assume False@, as the module
-- header describes.
reuse :: Pool a -> Gen a
reuse pool = Draw (spanLabel LabelPoolReuse) \tc ->
  withMVar pool.values \m -> snd <$> drawVar "reuse" Reuse pool tc m

-- | A generator that consumes values from the pool, removing each yielded
-- value so it is never drawn again.
--
-- Drawing from an empty pool is equivalent to @assume False@, as the module
-- header describes. Under 'Hegel.Gen.filtered' or 'Hegel.Gen.mapMaybe', every
-- rejected attempt's value goes back once the attempts are over, so only the
-- kept value is consumed.
consume :: Pool a -> Gen a
consume pool = Draw (spanLabel LabelPoolConsume) \tc -> do
  (vid, v) <- drawConsuming "consume" pool tc
  TestCase.recordUndo tc $ restore pool tc vid v
  pure v

-- | Undo a consuming draw of @v@ under the variable id @vid@.
restore :: Pool a -> TestCase -> Int -> a -> IO ()
restore pool tc vid v = do
  modifyMVar_ pool.values \m -> do
    poolRestore tc pool.handle pool.identity vid
    pure (IntMap.insert vid v m)
  TestCase.forgetDraw tc Var {pool = pool.identity, id = vid}

-- | Take a value out of the pool for 'consume' and 'transfer', removing it
-- from both the engine and the mirror.
--
-- Throws 'AssumeRejected' when the pool is empty.
drawConsuming :: Text -> Pool a -> TestCase -> IO (Int, a)
drawConsuming caller pool tc =
  modifyMVar pool.values \m -> do
    (vid, v) <- drawVar caller Consume pool tc m
    pure (IntMap.delete vid m, (vid, v))

-- | Whether a draw leaves its value in the pool or removes it.
data DrawMode = Reuse | Consume

-- | Have the engine pick one of the pool's variables and return its id
-- with its value from @m@, the pool's mirror, which the caller must hold
-- locked across this call.
--
-- Throws 'AssumeRejected' when the pool is empty.
drawVar :: Text -> DrawMode -> Pool a -> TestCase -> IntMap a -> IO (Int, a)
drawVar caller mode pool tc m = do
  when (IntMap.null m) $ throwIO AssumeRejected
  vid <- poolGenerate tc pool.handle pool.identity consuming
  case IntMap.lookup vid m of
    Just v -> pure (vid, v)
    Nothing ->
      throwIO InvariantViolation {detail = "Hegel.Pool." <> caller <> ": unknown variable id " <> T.pack (show vid)}
  where
    consuming = case mode of
      Reuse -> False
      Consume -> True

-- | A generator that moves a value from one pool to another: a consuming
-- draw from @src@ whose value is immediately registered in @dst@, with the
-- identity link /declared/ in the event stream.
--
-- Use this when modeling state changes, such as closing a handle to some
-- resource.
--
-- The move is not atomic across the two pools, and is therefore not safe to
-- retry. Drawing it inside a 'Hegel.Gen.filtered' or 'Hegel.Gen.mapMaybe'
-- attempt throws 'MalformedTest', since a rejected attempt could not undo the
-- move, and a stateful rule that is rejected after a transfer keeps it.
transfer :: Pool a -> Pool a -> Gen a
transfer src dst = Draw (spanLabel LabelPoolTransfer) \tc -> do
  nested <- TestCase.inFilter tc
  when nested $
    throwIO (malformedTest "Hegel.Pool.transfer" "a transfer cannot be drawn inside a filter attempt, which cannot undo the move" [])
  (vid, v) <- drawConsuming "transfer" src tc
  modifyMVar_ dst.values \m -> do
    vid' <- poolAddFrom tc dst.handle dst.identity Var {pool = src.identity, id = vid}
    pure (IntMap.insert vid' v m)
  pure v
