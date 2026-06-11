-- | Where the engine persists failing examples for replay.
module Hegel.Database
  ( Database (..),
  )
where

-- | The example database: a key\/value store of failing choice sequences,
-- replayed by the 'Hegel.Phase.Reuse' phase on subsequent runs.
--
-- A database is only useful together with a stable
-- 'Hegel.Settings.databaseKey'; without one there is nothing to file failures
-- under, which is why 'Hegel.Settings.defaultSettings' disables persistence.
data Database
  = -- | Use the engine's default store: @.hegel/@ relative to the working
    -- directory.
    DatabaseDefault
  | -- | No persistence.
    DatabaseDisabled
  | -- | A directory-backed store at the given path.
    DatabaseDirectory !FilePath
  deriving stock (Show)
