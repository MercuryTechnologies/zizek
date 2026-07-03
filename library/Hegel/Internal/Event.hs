-- | The per-test-case structural event stream: engine-visible pool activity
-- recorded alongside (but separate from) the user-facing note journal.
--
-- __Internal module.__ Implementation substrate of @zizek@ itself, exposed so
-- you can reach past the public API when you must; it is not part of the
-- stable public interface and may change without notice.
--
-- Events are renderer-facing topology (which value was born, drawn, or
-- consumed at which point), not user-level report vocabulary — that is why
-- they are a second stream rather than new 'Hegel.Report.Note' kinds. The two
-- streams share one monotonic 'Clock': 'Hegel.Property.Internal.journalNote'
-- stamps each note from the same counter that stamps each event, so the
-- render boundary can zip them back into a single ordered history. In
-- particular, a pool draw's event always immediately precedes its journaled
-- @Drawn@ note (the draw runs inside the generator, strictly before @forAll@
-- journals), which is what lets the trace builder correlate the two by clock
-- adjacency.
--
-- Like the journal, the stream only records during the final reconstruction
-- replay: live runs and shrink replays run 'Silent', where 'emit' never
-- constructs the event at all.
module Hegel.Internal.Event
  ( -- * Clock
    Clock (..),

    -- * Events
    Event (..),
    EventKind (..),
    Var (..),

    -- * Recording
    Log (..),
    newLog,
    tick,
    emit,
    drain,
  )
where

import Data.Foldable (toList)
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import UnliftIO.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)

-- * Clock

-- | A monotonic per-test-case sequence stamp, shared between the note journal
-- and the event stream. Ordering is the only meaning; there is no time unit.
newtype Clock = Clock Int
  deriving newtype (Eq, Ord, Show, Enum)

-- * Events

-- | A variable in an engine pool: the pool's id paired with the
-- engine-assigned variable id (which is unique only within its pool).
--
-- This is a pool value's /identity/ across a trace. Display names
-- (@h₁, h₂, …@) are assigned by the renderer over birth order, not carried
-- here.
data Var = Var
  { pool :: !Int,
    id :: !Int
  }
  deriving stock (Show, Eq, Ord)

-- | One engine-visible pool operation.
data Event = Event
  { clock :: !Clock,
    var :: !Var,
    kind :: !EventKind
  }
  deriving stock (Show, Eq)

-- | What the pool operation was.
--
-- There is no @Removed@: @libhegel@ has no @pool_remove@, so the only way a
-- value leaves a pool is a consuming draw — 'Consumed' /is/ the death event.
data EventKind
  = -- | Registered in the pool ('Hegel.Pool.add'). The payload is the
    -- value's /lineage/: 'Just' the source var when this birth is the
    -- destination half of a 'Hegel.Pool.transfer' — a declared identity
    -- link, letting the trace reconnect one logical value across pools.
    Born !(Maybe Var)
  | -- | Drawn without removal ('Hegel.Pool.valuesReusable').
    Reused
  | -- | Drawn and removed ('Hegel.Pool.valuesConsumed'); the value's death —
    -- unless a lineage-linked 'Born' in the same step continues it (a
    -- transfer).
    Consumed
  | -- | A pool label ('Hegel.Pool.named'): the event's @var.pool@ names the
    -- pool, @var.id@ is meaningless (0), and the payload is the display
    -- label. Not a touch; the trace lifts it out of the step stream.
    Named !Text
  deriving stock (Show, Eq)

-- * Recording

-- | Whether (and where) the current test case records events.
--
-- Mirrors 'Hegel.Property.Internal.Journal': ordinary cases (including every
-- shrink replay) run 'Silent'; only the final reconstruction replay records
-- (via 'newLog').
data Log
  = Silent
  | Recording !(IORef Clock) !(IORef (Seq Event))

-- | A recording log, for the final reconstruction replay.
newLog :: IO Log
newLog = Recording <$> newIORef (Clock 0) <*> newIORef Seq.empty

-- | Advance the shared clock and return the fresh stamp.
--
-- Under 'Silent' there is no clock; returns @'Clock' 0@ (the stamp is never
-- observed — nothing records under 'Silent').
tick :: Log -> IO Clock
tick Silent = pure (Clock 0)
tick (Recording clk _) = atomicModifyIORef' clk \c -> (succ c, c)
{-# INLINE tick #-}

-- | Record an event stamped with a fresh clock.
--
-- Takes a closure so that under 'Silent' the event (and its fields) are never
-- constructed — the same zero-cost discipline as
-- 'Hegel.Property.Internal.journalNote' under a silent journal. An append is
-- complete the moment it happens; an exception arriving mid-step simply stops
-- further appends (the journal's exception property).
emit :: Log -> (Clock -> Event) -> IO ()
emit Silent _ = pure ()
emit log_@(Recording _ ref) mkEvent = do
  c <- tick log_
  modifyIORef' ref (|> mkEvent c)
-- The INLINE makes the zero-cost claim structural rather than
-- optimizer-dependent: inlined at the call site, the event closure floats
-- into the 'Recording' branch and a 'Silent' draw allocates nothing.
{-# INLINE emit #-}

-- | Read back the recorded events, in emission order.
drain :: Log -> IO [Event]
drain Silent = pure []
drain (Recording _ ref) = toList <$> readIORef ref
