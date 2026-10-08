-- | Failing workloads that put the engine into nondeterministic handling,
-- where it stamps generation, confirmation, and report-time cases for
-- capture, so every stamped case pays for a recording journal.
--
-- 'flakyProperty' fails on a schedule kept outside the choice sequence, so
-- its failures recur on some replays and not others. 'raceMachine' loses
-- updates to a shared counter when two workers interleave, which adds the
-- per-worker journal fold to the cost a stamped case pays.
module Nondeterministic
  ( flakyProperty,
    raceMachine,
  )
where

import Control.Concurrent (yield)
import Control.Monad (replicateM_)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Function ((&))
import Hegel.Gen qualified as Gen
import Hegel.Property (Property, annotate, assert, forAll, (===))
import Hegel.Report (renderValue)
import Hegel.Stateful.Concurrent qualified as Concurrent
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)

-- | Counts executions across the whole process, so the same choices fail on
-- some executions and pass on others.
executions :: IORef Int
executions = unsafePerformIO (newIORef 0)
{-# NOINLINE executions #-}

-- | A hundred draws and twenty annotations, failing on every third execution
-- once the first draw is large.
flakyProperty :: Property ()
flakyProperty = do
  xs <- traverse (const (forAll (Gen.int & Gen.min 0 & Gen.max 100 & Gen.build))) [1 .. 100 :: Int]
  for_ (take 20 xs) \x -> annotate ("draw " <> renderValue x)
  n <- liftIO (atomicModifyIORef' executions \k -> (k + 1, k + 1))
  assert (take 1 xs < [50] || n `mod` 3 /= 0) "large first draw on a third execution"

data RaceModel = RaceModel
  { counter :: IORef Int,
    attempts :: IORef Int
  }

-- | Two workers incrementing a shared counter with a read, a yield, and a
-- write, checked against the number of increments at every round join.
raceMachine :: Concurrent.Machine RaceModel IO
raceMachine =
  Concurrent.Machine
    { initial = liftIO (RaceModel <$> newIORef 0 <*> newIORef 0),
      rules = [increment],
      invariants = [Concurrent.Invariant "no_lost_updates" noLostUpdates],
      stepCount = 20
    }
  where
    increment :: Concurrent.Rule RaceModel IO
    increment = Concurrent.rule "increment" \m -> do
      _ <- forAll (Gen.int & Gen.min 0 & Gen.max 100 & Gen.build)
      liftIO do
        v <- readIORef m.counter
        replicateM_ 3 yield
        writeIORef m.counter (v + 1)
        modifyIORef' m.attempts (+ 1)
    noLostUpdates :: RaceModel -> Property ()
    noLostUpdates m = do
      c <- liftIO (readIORef m.counter)
      a <- liftIO (readIORef m.attempts)
      c === a
