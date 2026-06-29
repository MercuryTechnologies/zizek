-- | Verbosity of engine-emitted output, as reported to @libhegel@.
module Hegel.Verbosity
  ( Verbosity (..),
  )
where

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
