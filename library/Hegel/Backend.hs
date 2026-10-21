-- | The engine's source of randomness, as reported to @libhegel@.
module Hegel.Backend
  ( Backend (..),
  )
where

import Data.Word (Word32)
import Hegel.Internal.Foreign.Raw
  ( pattern HEGEL_BACKEND_DEFAULT,
    pattern HEGEL_BACKEND_URANDOM,
  )
import Witch qualified

-- | Which source of randomness the engine draws from.
--
-- This selects the /source/, not a seed: the seed is configured separately
-- (@seed@ \/ @derandomize@ in "Hegel.Settings") and, when left unset, is chosen
-- fresh at the start of each run.
data Backend
  = -- | Drive the whole run from a pseudo-random generator seeded once at the
    -- start, so every draw is a deterministic function of that one seed. A run
    -- therefore replays exactly given the same seed — which is what lets
    -- shrinking, replay, and the failure database work. This is the
    -- default.
    Default
  | -- | Take fresh entropy from outside the process on every draw, ignoring the
    -- seed entirely. Under Antithesis it comes from Antithesis's fuzzer, and
    -- elsewhere from @\/dev\/urandom@.
    --
    -- Intended for running under Antithesis, and selected by the shipped
    -- @workload@ profile; you almost certainly don't want it otherwise.
    Urandom
  deriving stock (Show, Eq, Enum, Bounded)

-- | The @hegel_backend_t@ wire value.
instance Witch.From Backend Word32 where
  from Default = HEGEL_BACKEND_DEFAULT
  from Urandom = HEGEL_BACKEND_URANDOM
