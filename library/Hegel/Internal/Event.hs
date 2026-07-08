-- | A per-test-case stream of events associated with a given pool; this module
-- defines an interface for recording @libhegel@ pooled variable operations
-- alongside (but separate from) the user-facing note journal.
--
-- Captures when a pooled value was born, drawn, or consumed and at which point
-- in test history this occurred.
module Hegel.Internal.Event
  ( Event (..),
    Operation (..),
    Var (..),
  )
where

import Data.Text (Text)
import Hegel.Internal.Tick (Tick)

-- | A variable drawn from a @libhegel@ pool, uniquely identified by the pool's
-- integer identifier & the per-pool variable identifier.
--
-- This is a pool value's /identity/ across a trace.
data Var = Var
  { pool :: !Int,
    id :: !Int
  }
  deriving stock (Show, Eq, Ord)

-- | One engine-visible pool operation.
data Event = Event
  { clock :: !Tick,
    var :: !Var,
    kind :: !Operation
  }
  deriving stock (Show, Eq)

-- | What the pool operation was.
--
-- __NOTE__: @libhegel@ does not provide a primitive for tracking variable
-- "death", so the only way a value leaves a pool is via a consuming draw.
data Operation
  = -- | A value is registered via 'Hegel.Pool.add'.
    Born !(Maybe Var)
  | -- | Drawn without removal via 'Hegel.Pool.reuse'.
    Reused
  | -- | Drawn and removed via 'Hegel.Pool.consume'.
    Consumed
  | -- | Labeled via 'Hegel.Pool.named'.
    Named !Text
  deriving stock (Show, Eq)
