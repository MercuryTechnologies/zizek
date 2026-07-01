-- | The engine's source of randomness, as reported to @libhegel@.
module Hegel.Backend
  ( Backend (..),
  )
where

import Foreign.C.Types (CInt)
import Hegel.Internal.FFI
  ( pattern HEGEL_BACKEND_AUTO,
    pattern HEGEL_BACKEND_DEFAULT,
    pattern HEGEL_BACKEND_URANDOM,
  )
import Witch qualified

-- | Which source of randomness the engine draws from.
--
-- This selects the /source/, not a seed: the seed is configured separately
-- (@seed@ \/ @derandomize@ in "Hegel.Settings") and, when left unset, is chosen
-- fresh at the start of each run.
data Backend
  = -- | Choose automatically (the default): 'Urandom' when running inside
    -- Antithesis, otherwise 'Default'.
    Auto
  | -- | Drive the whole run from a pseudo-random generator seeded once at the
    -- start, so every draw is a deterministic function of that one seed. A run
    -- therefore replays exactly given the same seed — which is what lets
    -- shrinking, replay, and the failure database work.
    Default
  | -- | Read fresh entropy from @\/dev\/urandom@ on every draw, ignoring the
    -- seed entirely.
    --
    -- Intended for running under Antithesis, whose fuzzer controls
    -- @\/dev\/urandom@; you almost certainly don't want it otherwise.
    Urandom
  deriving stock (Show, Eq)

-- | The @hegel_backend_t@ wire value.
instance Witch.From Backend CInt where
  from Auto = HEGEL_BACKEND_AUTO
  from Default = HEGEL_BACKEND_DEFAULT
  from Urandom = HEGEL_BACKEND_URANDOM
