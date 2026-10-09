-- | Settings profiles: named sets of settings that @libhegel@ resolves every
-- run's 'Settings' over.
--
-- Designed for qualified import:
--
-- @
-- import Hegel.Profile qualified as Profile
--
-- main = do
--   Profile.register "nightly" defaultSettings {testCases = Just 10000}
--   Profile.setDefault "nightly"
-- @
--
-- Registrations and the default profile apply to the whole process, and to
-- settings resolved after the call. A @hegel.toml@ section with the same name
-- still merges over a registered profile.
module Hegel.Profile
  ( register,
    setDefault,
    clearDefault,
    resolve,
  )
where

import Control.Exception (throwIO, try)
import Data.Text (Text)
import Foreign (Ptr, nullPtr)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hegel.Internal.Foreign.CString qualified as CString
import Hegel.Internal.Foreign.Raw
import Hegel.Internal.Settings (readSettings, settingsError, withResolvedSettings)
import Hegel.Settings (Settings (..))
import Hegel.Settings qualified as Settings

-- | Register what @settings@ resolves to as the profile @name@, replacing any
-- earlier registration of that name, including a shipped profile's.
--
-- The profile holds every engine setting @settings@ resolves to, starting from
-- the profile it selects. Its 'databaseKey' and 'maxCloneDepth' are not part
-- of the profile.
--
-- Throws 'Hegel.Exception.SettingsError' for a reserved name, @base@ or @default@, or one
-- with characters other than ASCII letters, digits, @-@, and @_@, and for
-- @settings@ that 'Settings.validate' rejects.
register :: (HasCallStack) => Text -> Settings -> IO ()
register name settings =
  validated settings *> withContext \ctx -> withResolvedSettings ctx settings \_ s ->
    CString.withText name \p ->
      rejecting ctx "Hegel.Profile.register" name =<< hegel_settings_register_profile ctx p s

-- | Make @name@ the default profile, the one a 'Settings' with no 'profile'
-- resolves over.
--
-- This takes precedence over @HEGEL_DEFAULT_PROFILE@, a @hegel.toml@ @default@
-- entry, and CI or Antithesis detection. The profile need not exist yet, but
-- resolving settings throws 'Hegel.Exception.SettingsError' until it does.
--
-- Throws 'Hegel.Exception.SettingsError' for an invalid name or for @default@ itself.
setDefault :: (HasCallStack) => Text -> IO ()
setDefault name = withContext \ctx -> CString.withText name \p -> setDefaultTo ctx name p

-- | Undo 'setDefault', so the default profile is chosen from the environment
-- again.
clearDefault :: (HasCallStack) => IO ()
clearDefault = withContext \ctx -> setDefaultTo ctx "" nullPtr

setDefaultTo :: (HasCallStack) => Ptr HegelContext -> Text -> CString -> IO ()
setDefaultTo ctx name p = rejecting ctx "Hegel.Profile.setDefault" name =<< hegel_set_default_profile ctx p

-- | The settings a run under @settings@ would use, with every engine setting
-- filled in from the resolved profile.
--
-- The result's 'profile' and 'databaseKey' are those of @settings@, and its
-- 'maxCloneDepth' is filled in with the default when unset.
--
-- Throws 'Hegel.Exception.SettingsError' when the profile is unknown, or when a @hegel.toml@
-- or one of the engine's @HEGEL_*@ settings environment variables is
-- malformed, or when 'Settings.validate' rejects @settings@.
resolve :: (HasCallStack) => Settings -> IO Settings
resolve settings = validated settings *> withContext \ctx -> withResolvedSettings ctx settings \_ s -> readSettings ctx settings s

-- | Throw a 'Hegel.Exception.SettingsError' for a name the engine rejected, and any other
-- engine error as it is.
rejecting :: (HasCallStack) => Ptr HegelContext -> Text -> Text -> CInt -> IO ()
rejecting ctx context name rc =
  try (throwOnError ctx rc) >>= \case
    Right () -> pure ()
    Left e
      | e.code == HEGEL_E_INVALID_ARG -> throwIO (settingsError context e [("profile", name)])
      | otherwise -> throwIO e

-- | Throw the 'Hegel.Exception.SettingsError' that 'Settings.validate' reports
-- for @settings@, if any.
validated :: (HasCallStack) => Settings -> IO ()
validated settings = either throwIO pure (withFrozenCallStack (Settings.validate settings))
