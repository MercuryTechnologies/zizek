-- | Where the engine persists failing examples for replay.
module Hegel.Database
  ( Database (..),
  )
where

-- | The example database: a key\/value store of failing choice sequences,
-- replayed by the 'Hegel.Phase.Reuse' phase on subsequent runs.
--
-- A run files and replays failures only under a 'Hegel.Settings.databaseKey'.
data Database
  = -- | Use the engine's default store: @.hegel/@ relative to the working
    -- directory.
    DatabaseDefault
  | -- | No persistence.
    DatabaseDisabled
  | -- | A directory-backed store at the given path.
    DatabaseDirectory !FilePath
  deriving stock (Show, Eq)
