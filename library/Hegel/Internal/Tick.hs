-- | A monotonic per-test-case sequence stamp that can be used to reconstruct
-- a stateful test timeline after a failing counterexample has been found.
--
-- A 'Tick' orders entries drawn from the user-facing note note journal and
-- the pool's event stream, so a renderer can zip them back into one history.
--
-- __NOTE__: This module is deliberately domain-agnostic: it knows nothing of
-- pools, events, notes, or state machines.
--
-- Callers pair it with their own buffer and entry type.
module Hegel.Internal.Tick
  ( -- * Stamp
    Tick (..),

    -- * Recording
    Recording (..),
    newRecording,
    next,
    record,
    drain,
  )
where

import Data.Foldable (toList)
import Data.Sequence (Seq, (|>))
import UnliftIO.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)

-- * Stamp

-- | A monotonic per-test-case sequence stamp.
newtype Tick = Tick Int
  deriving newtype (Eq, Ord, Show, Enum)

-- * Recording

-- | Whether the current test case is recording.
--
-- Ordinary cases are 'Silent', and only the final reconstruction replay
-- carries the monotonic sequence stamp under 'Active'.
data Recording
  = Silent
  | Active !(IORef Tick)

-- | A fresh 'Active' recording, its clock at @'Tick' 0@.
newRecording :: IO Recording
newRecording = Active <$> newIORef (Tick 0)

-- | Advance the shared clock and return the fresh stamp.
--
-- Under 'Silent' there is no clock; returns @'Tick' 0@.
next :: Recording -> IO Tick
next Silent = pure (Tick 0)
next (Active clk) = atomicModifyIORef' clk \c -> (succ c, c)
{-# INLINE next #-}

-- | Append a stamped entry to a buffer, advancing the clock first.
--
-- Takes a closure so that under 'Silent' the entry (and its fields) are never
-- constructed.
record :: Recording -> IORef (Seq a) -> (Tick -> a) -> IO ()
record Silent _ _ = pure ()
record rec ref mk = do
  c <- next rec
  modifyIORef' ref (|> mk c)
{-# INLINE record #-}

-- | Read back a buffer's entries, in append order.
drain :: IORef (Seq a) -> IO [a]
drain ref = toList <$> readIORef ref
