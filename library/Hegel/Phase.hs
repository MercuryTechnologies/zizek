-- | Phases of a property run, as reported to @libhegel@.
module Hegel.Phase
  ( Phase (..),
  )
where

import Data.Word (Word32)
import Hegel.Internal.FFI
  ( pattern HEGEL_PHASE_EXPLICIT,
    pattern HEGEL_PHASE_GENERATE,
    pattern HEGEL_PHASE_REUSE,
    pattern HEGEL_PHASE_SHRINK,
    pattern HEGEL_PHASE_TARGET,
  )
import Witch qualified

-- | Phases of a property run, in execution order.
data Phase
  = -- | Replay explicitly-provided examples.
    Explicit
  | -- | Replay examples from the example database.
    Reuse
  | -- | Generate new random examples.
    Generate
  | -- | Guide generation toward target values.
    Target
  | -- | Shrink discovered failures.
    Shrink
  deriving stock (Show, Eq)

-- | The @hegel_phase_t@ single-bit wire flag.
--
-- OR these together for @hegel_settings_set_phases@.
instance Witch.From Phase Word32 where
  from Explicit = HEGEL_PHASE_EXPLICIT
  from Reuse = HEGEL_PHASE_REUSE
  from Generate = HEGEL_PHASE_GENERATE
  from Target = HEGEL_PHASE_TARGET
  from Shrink = HEGEL_PHASE_SHRINK
