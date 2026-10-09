-- | How a property run seeds its random generation.
module Hegel.Seed
  ( Seed (..),
  )
where

import Data.Word (Word64)

-- | The seed a run starts its random generation from.
data Seed
  = -- | Pick a fresh seed at the start of each run, or derive one from the
    -- database key when 'Hegel.Settings.derandomize' is on.
    SeedFresh
  | -- | Start every run from this seed.
    SeedFixed !Word64
  deriving stock (Show, Eq)
