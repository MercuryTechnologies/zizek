-- | How a run reacts to nondeterministic test behavior, as reported to
-- @libhegel@.
module Hegel.Nondeterminism
  ( Nondeterminism (..),
  )
where

import Data.Word (Word32)
import Hegel.Internal.Foreign.Raw
  ( pattern HEGEL_NONDETERMINISM_ERROR,
    pattern HEGEL_NONDETERMINISM_QUIET,
    pattern HEGEL_NONDETERMINISM_WARN,
  )
import Witch qualified

-- | What a run does once it sees a test whose structure or outcome changes
-- when the same choices are replayed.
data Nondeterminism
  = -- | Switch silently to nondeterministic handling, which confirms each
    -- failure by repeated replay before reporting or shrinking it. A failure
    -- found this way carries a caveat describing how reliably it reproduced.
    -- This is the default.
    Tolerate
  | -- | Switch as 'Tolerate' does, and have the engine print a one-line notice
    -- once per run.
    Warn
  | -- | Abort the run with a nondeterminism error, for suites that treat
    -- determinism as a requirement.
    Forbid
  deriving stock (Show, Eq)

-- | The @hegel_nondeterminism_strictness_t@ wire value.
instance Witch.From Nondeterminism Word32 where
  from Tolerate = HEGEL_NONDETERMINISM_QUIET
  from Warn = HEGEL_NONDETERMINISM_WARN
  from Forbid = HEGEL_NONDETERMINISM_ERROR
