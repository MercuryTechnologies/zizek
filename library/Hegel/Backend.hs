-- | The engine's source of randomness, as reported to @libhegel@.
module Hegel.Backend
  ( Backend (..),
  )
where

-- | Which source of randomness the engine draws from.
data Backend
  = -- | Choose automatically (the default): the urandom backend when running
    -- inside Antithesis, otherwise the seeded PRNG.
    Auto
  | -- | Expand a single seeded PRNG. Runs are reproducible from the seed and
    -- shrinking \/ replay work as usual.
    Default
  | -- | Read fresh entropy from @\/dev\/urandom@ on every draw. Intended for
    -- running under Antithesis; you almost certainly don't want it otherwise.
    Urandom
  deriving stock (Show, Eq)
