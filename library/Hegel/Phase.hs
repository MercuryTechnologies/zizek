-- | Phases of a property run, as reported to @libhegel@.
module Hegel.Phase
  ( Phase (..),
  )
where

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
