-- | Settings handles: resolving a profile, applying 'Settings' overrides to
-- it, and reading the result back.
module Hegel.Internal.Settings
  ( Resolved (..),
    withResolvedSettings,
    readSettings,
    settingsError,
    phasesBitmask,
    hcBitmask,
  )
where

import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Bits ((.&.), (.|.))
import Data.Foldable (for_)
import Data.List (find)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32)
import Foreign (Ptr, Storable, alloca, castPtr, fromBool, nullPtr, peek)
import Foreign.C.Types (CBool (..), CChar, CInt)
import GHC.Stack (HasCallStack, callStack)
import Hegel.Database (Database (..))
import Hegel.Exception (Diagnostic (..), SettingsError (..))
import Hegel.HealthCheck (HealthCheck)
import Hegel.Internal.Foreign.CString qualified as CString
import Hegel.Internal.Foreign.Raw
import Hegel.Phase (Phase)
import Hegel.Seed (Seed (..))
import Hegel.Settings (Settings (..))
import Hegel.Settings qualified as Settings
import Hegel.Verbosity (Verbosity (..))
import Witch qualified

-- | What a settings handle resolved to, read back once its overrides are
-- applied.
data Resolved = Resolved
  { -- | Whether the run has a database to file failures in.
    persists :: !Bool,
    -- | Whether a failure's report carries its replay token.
    printBlob :: !Bool
  }

-- | Resolve the profile @settings@ selects, apply its overrides, and pass the
-- action what the handle resolved to along with the handle itself.
--
-- Throws 'SettingsError' when the profile is unknown, or when a @hegel.toml@
-- or one of the engine's @HEGEL_*@ settings environment variables is
-- malformed.
withResolvedSettings :: (HasCallStack) => Ptr HegelContext -> Settings -> (Resolved -> Ptr HegelSettings -> IO a) -> IO a
withResolvedSettings ctx settings action =
  maybe ($ nullPtr) CString.withText settings.profile \name ->
    withProfileSettings ctx name configured >>= \case
      Right a -> pure a
      Left e
        | e.code == HEGEL_E_INVALID_ARG ->
            throwIO (settingsError "Hegel.Settings.profile" e [("profile", fromMaybe "default" settings.profile)])
        | otherwise -> throwIO e
  where
    configured s = do
      applySettings ctx settings s
      quietByDefault ctx settings s
      resolved <- readResolved ctx s
      action resolved s

-- | A 'SettingsError' carrying the engine's message for a rejected call.
settingsError :: (HasCallStack) => Text -> HegelError -> [(Text, Text)] -> SettingsError
settingsError context e values =
  SettingsError
    Diagnostic
      { context,
        detail = fromMaybe "the engine rejected the settings" e.message,
        values,
        callStack = callStack
      }

-- | Read back the resolved fields the runner acts on itself.
readResolved :: Ptr HegelContext -> Ptr HegelSettings -> IO Resolved
readResolved ctx s = do
  database <- readDatabase ctx s
  printBlob <- readBool ctx (hegel_settings_get_print_blob ctx s)
  pure Resolved {persists = database /= DatabaseDisabled, printBlob}

-- | Read every engine-owned field of a resolved handle, keeping the
-- 'profile', 'databaseKey', and 'maxCloneDepth' that @settings@ supplied.
readSettings :: Ptr HegelContext -> Settings -> Ptr HegelSettings -> IO Settings
readSettings ctx settings s = do
  testCases <- out (hegel_settings_get_test_cases ctx s)
  seed <- alloca \seedOut -> alloca \hasSeedOut -> do
    throwOnError ctx =<< hegel_settings_get_seed ctx s seedOut hasSeedOut
    CBool hasSeed <- peek hasSeedOut
    if hasSeed /= 0 then SeedFixed <$> peek seedOut else pure SeedFresh
  derandomize <- readBool ctx (hegel_settings_get_derandomize ctx s)
  database <- readDatabase ctx s
  phases <- fromBitmask <$> out (hegel_settings_get_phases ctx s)
  suppressHealthCheck <- fromBitmask <$> out (hegel_settings_get_suppress_health_check ctx s)
  reportMultipleFailures <- readBool ctx (hegel_settings_get_report_multiple_failures ctx s)
  unboundedChoices <- readBool ctx (hegel_settings_get_unbounded_choices ctx s)
  printBlob <- readBool ctx (hegel_settings_get_print_blob ctx s)
  verbosity <- out (hegel_settings_get_verbosity ctx s) >>= decode "verbosity"
  backend <- out (hegel_settings_get_backend ctx s) >>= decode "backend"
  nondeterminism <- out (hegel_settings_get_nondeterminism_strictness ctx s) >>= decode "nondeterminism strictness"
  pure
    Settings
      { profile = settings.profile,
        testCases = Just (fromIntegral testCases),
        seed = Just seed,
        derandomize = Just derandomize,
        database = Just database,
        databaseKey = settings.databaseKey,
        phases = Just phases,
        backend = Just backend,
        verbosity = Just verbosity,
        reportMultipleFailures = Just reportMultipleFailures,
        suppressHealthCheck = Just suppressHealthCheck,
        unboundedChoices = Just unboundedChoices,
        nondeterminism = Just nondeterminism,
        printBlob = Just printBlob,
        maxCloneDepth = Just (fromMaybe Settings.defaultMaxCloneDepth settings.maxCloneDepth)
      }
  where
    out :: (Storable a) => (Ptr a -> IO CInt) -> IO a
    out = readOut ctx
    decode :: (Enum a, Bounded a, Witch.From a Word32) => String -> Word32 -> IO a
    decode name w = case find ((== w) . Witch.into @Word32) [minBound .. maxBound] of
      Just a -> pure a
      Nothing -> throwIO (userError ("libhegel reported an unknown " <> name <> " value " <> show w))
    fromBitmask :: (Enum a, Bounded a, Witch.From a Word32) => Word32 -> [a]
    fromBitmask mask = filter (\a -> mask .&. Witch.into @Word32 a /= 0) [minBound .. maxBound]

-- | Read the database a handle resolved to: @NULL@ is the default store and
-- @\"\"@ a disabled one.
readDatabase :: Ptr HegelContext -> Ptr HegelSettings -> IO Database
readDatabase ctx s = do
  database <- readOut ctx (hegel_settings_get_database ctx s)
  if database == nullPtr
    then pure DatabaseDefault
    else do
      first <- peek (castPtr database :: Ptr CChar)
      if first == 0 then pure DatabaseDisabled else DatabaseDirectory . T.unpack <$> peekUtf8 database

readBool :: Ptr HegelContext -> (Ptr CBool -> IO CInt) -> IO Bool
readBool ctx get = (\(CBool b) -> b /= 0) <$> readOut ctx get

-- | Run one @out_*@ getter, checking its return code and reading the result.
readOut :: (Storable a) => Ptr HegelContext -> (Ptr a -> IO CInt) -> IO a
readOut ctx get = alloca \o -> do
  throwOnError ctx =<< get o
  peek o

-- | Apply every override a 'Settings' value sets to a resolved handle, leaving
-- the profile's value for every field it leaves unset.
applySettings :: Ptr HegelContext -> Settings -> Ptr HegelSettings -> IO ()
applySettings ctx s ptr = do
  for_ s.backend \b -> chk $ hegel_settings_set_backend ctx ptr (Witch.into @Word32 b)
  for_ s.testCases \n -> chk $ hegel_settings_set_test_cases ctx ptr (fromIntegral n)
  for_ s.verbosity \v -> chk $ hegel_settings_set_verbosity ctx ptr (Witch.into @Word32 v)
  for_ s.seed \case
    SeedFresh -> chk $ hegel_settings_set_seed ctx ptr 0 (CBool 0)
    SeedFixed seed -> chk $ hegel_settings_set_seed ctx ptr seed (CBool 1)
  for_ s.derandomize \b -> chk $ hegel_settings_set_derandomize ctx ptr (fromBool b)
  for_ s.reportMultipleFailures \b -> chk $ hegel_settings_set_report_multiple_failures ctx ptr (fromBool b)
  for_ s.phases \ps -> chk $ hegel_settings_set_phases ctx ptr (phasesBitmask ps)
  for_ s.suppressHealthCheck \hcs -> chk $ hegel_settings_set_suppress_health_check ctx ptr (hcBitmask hcs)
  for_ s.unboundedChoices \b -> chk $ hegel_settings_set_unbounded_choices ctx ptr (fromBool b)
  for_ s.nondeterminism \n -> chk $ hegel_settings_set_nondeterminism_strictness ctx ptr (Witch.into @Word32 n)
  for_ s.printBlob \b -> chk $ hegel_settings_set_print_blob ctx ptr (fromBool b)
  for_ s.database \case
    DatabaseDefault -> chk $ hegel_settings_set_database ctx ptr nullPtr
    DatabaseDisabled -> CString.withFilePath "" \p -> chk $ hegel_settings_set_database ctx ptr p
    DatabaseDirectory dir -> CString.withFilePath dir \p -> chk $ hegel_settings_set_database ctx ptr p
  for_ s.databaseKey \key ->
    CString.withText key \p -> chk $ hegel_settings_set_database_key ctx ptr p
  where
    chk io = io >>= throwOnError ctx

-- | Quiet the engine unless @settings@ sets a 'verbosity' or the profile asks
-- for 'Verbose' or 'Debug' output.
--
-- A profile's 'Normal' looks the same as the engine default, so only an
-- explicit override keeps it.
quietByDefault :: Ptr HegelContext -> Settings -> Ptr HegelSettings -> IO ()
quietByDefault ctx settings s = when (isNothing settings.verbosity) do
  level <- readOut ctx (hegel_settings_get_verbosity ctx s)
  when (level == Witch.into @Word32 Normal) do
    throwOnError ctx =<< hegel_settings_set_verbosity ctx s (Witch.into @Word32 Quiet)

-- | OR the per-phase wire flags into a bitmask.
--
-- An empty list yields @0@, which disables all phases.
phasesBitmask :: [Phase] -> Word32
phasesBitmask = foldl' (\acc p -> acc .|. Witch.into @Word32 p) 0

-- | OR the per-health-check wire flags into a suppression bitmask.
hcBitmask :: [HealthCheck] -> Word32
hcBitmask = foldl' (\acc hc -> acc .|. Witch.into @Word32 hc) 0
