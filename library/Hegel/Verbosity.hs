-- | Verbosity of engine-emitted output, as reported to @libhegel@.
module Hegel.Verbosity
  ( Verbosity (..),
  )
where

import Foreign.C.Types (CInt)
import Hegel.Internal.Foreign.Raw
  ( pattern HEGEL_VERBOSITY_DEBUG,
    pattern HEGEL_VERBOSITY_NORMAL,
    pattern HEGEL_VERBOSITY_QUIET,
    pattern HEGEL_VERBOSITY_VERBOSE,
  )
import Witch qualified

-- | How much diagnostic output the engine emits during a run.
data Verbosity
  = -- | Nothing besides the final result.
    Quiet
  | -- | A short summary line per run.
    Normal
  | -- | Per-test-case progress and drawn values, panic diagnostics as they
    -- happen.
    Verbose
  | -- | As 'Verbose', plus Hypothesis-style shrinker trace output.
    Debug
  deriving stock (Show, Eq)

-- | The @hegel_verbosity_t@ wire value.
instance Witch.From Verbosity CInt where
  from Quiet = HEGEL_VERBOSITY_QUIET
  from Normal = HEGEL_VERBOSITY_NORMAL
  from Verbose = HEGEL_VERBOSITY_VERBOSE
  from Debug = HEGEL_VERBOSITY_DEBUG
