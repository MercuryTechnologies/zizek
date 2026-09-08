{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | hspec integration.
--
-- 'prop' is a drop-in for @it@ that derives a stable example-database key from
-- the test's path and persists failures for replay (the usual entry point):
--
-- @
-- describe "reverse" $ do
--   'prop' "is involutive" do
--     xs <- 'Hegel.Property.forAll' (Gen.list (Gen.int & Gen.build) & Gen.build)
--     reverse (reverse xs) 'Hegel.Property.===' xs
-- @
--
-- Use 'propWith' for explicit 'Settings'; for example @propWith def@ runs a
-- property without the replay database.
--
-- 'propModify' customizes the persisted defaults. 'propFor' provides the same
-- identity and persistence for a property that consumes an Hspec fixture.
-- Direct @it "name" (\fixture -> property)@ uses unkeyed, nonpersisted defaults.
--
-- Shared @HEGEL_TEST_CASES@, @HEGEL_STATEFUL_STEPS@, @HEGEL_SEED@, and
-- @HEGEL_DATABASE@ overrides are read during each example's execution.
-- Database values are @off@, @default@, or @directory:PATH@.
-- @HEGEL_REPLAY@ and @HEGEL_REPLAY_KEY@ must be supplied together and select
-- one replay for the exact matching key, bypassing phases and database access.
-- Filter with Hspec's @--match@ and check the replay-selection message;
-- leaf integrations cannot detect a key that matched no selected example.
--
-- Specs converted with @tasty-hspec@ retain these identities and shared
-- environment controls. Outer Tasty groups do not contribute to their keys,
-- and native Hegel Tasty options do not configure adapted examples.
--
-- For a property over a custom base monad, use 'propT'\/'propWithT'.
--
-- __NOTE__: While the @arg ->@ 'Hspec.Example' instance composes with hspec's
-- @around@\/fixtures, it's important to keep track of how stateful fixtures
-- can interact with shrinking and replays.
module Hegel.Hspec
  ( prop,
    propT,
    propWith,
    propWithT,
    propFor,
    propForWith,
    propModify,
    propForModify,
    propModifyT,
  )
where

import Control.Monad ((>=>))
import Data.Default.Class (def)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Data.Text qualified as T
import GHC.Stack (CallStack, HasCallStack, SrcLoc (..), callStack, withFrozenCallStack)
import Hegel.Database (Database (..))
import Hegel.Internal.DatabaseKey (propKey)
import Hegel.Internal.RunnerConfig qualified as Config
import Hegel.Property.Internal (Property, PropertyT, hoist)
import Hegel.Report
  ( Abort (..),
    FailureEvidence (..),
    FailureEvidenceStatus (..),
    FailureOutcome (..),
    Report (..),
    Result (..),
    renderReport,
    renderReportAnsi,
    renderReportAuto,
  )
import Hegel.Report.Style qualified as Style
import Hegel.Settings (Settings (..), defaultSettings, withDatabaseKey)
import System.Environment (lookupEnv)
import System.IO (hIsTerminalDevice, stderr, stdout)
import Test.Hspec.Core.Spec qualified as Hspec
import UnliftIO.IORef (newIORef, readIORef, writeIORef)

-- | A property that takes a fixture is an hspec 'Hspec.Example', so it composes
-- with @around@\/@aroundWith@. This path runs with 'defaultSettings' (no key,
-- no persistence); use 'propFor' for keyed, persisted properties.
instance (m ~ IO) => Hspec.Example (arg -> PropertyT m ()) where
  type Arg (arg -> PropertyT m ()) = arg
  evaluateExample mkProp _params aroundAction _progress =
    withAroundResult aroundAction (runProperty def . mkProp)

-- | A property paired with the 'Settings' it should run under, so the
-- example-database key derived by 'prop'\/'propWith' reaches the runner.
data HegelExample = HegelExample CallStack Settings (Property ())

instance Hspec.Example HegelExample where
  type Arg HegelExample = ()
  evaluateExample (HegelExample cs settings body) _params aroundAction _progress =
    let ?callStack = cs in withFrozenCallStack $ withAroundResult aroundAction \() -> runProperty settings body

-- | A property over an arbitrary base monad @m@ (e.g. an application stack),
-- paired with the 'Settings' to run under and a runner that — given the fixture
-- @env@ — collapses @m@ to 'IO'. Built by 'propT'\/'propWithT'.
data HegelExampleT env m
  = HegelExampleT CallStack Settings (env -> forall x. m x -> IO x) (PropertyT m ())

instance Hspec.Example (HegelExampleT env m) where
  type Arg (HegelExampleT env m) = env
  evaluateExample (HegelExampleT cs settings nat body) _params aroundAction _progress =
    let ?callStack = cs in withFrozenCallStack $ withAroundResult aroundAction \env -> runProperty settings (hoist (nat env) body)

-- | Run @mk@ inside hspec's around-action (which owns any fixture) and return
-- its result.
--
-- The result is produced inside the around callback, so it is smuggled out
-- through a ref. The ref is seeded with a failing sentinel: if the around-action
-- never runs the example (broken plumbing), that surfaces as a loud failure
-- rather than a spurious pass.
withAroundResult :: (Hspec.ActionWith a -> IO ()) -> (a -> IO Hspec.Result) -> IO Hspec.Result
withAroundResult aroundAction mk = do
  ref <- newIORef neverRan
  aroundAction (mk >=> writeIORef ref)
  readIORef ref
  where
    neverRan =
      Hspec.Result "" $
        Hspec.Failure Nothing $
          Hspec.Reason
            "internal error: the property never ran (hspec's around-action did \
            \not invoke its callback). This should be impossible; please report it."

-- | Check a property and render its 'Report' as an hspec 'Hspec.Result'.
runProperty :: (HasCallStack) => Settings -> Property () -> IO Hspec.Result
runProperty settings body = do
  overrides <- Config.readOverrides
  case overrides >>= \o -> (,o) <$> withFrozenCallStack (Config.resolve settings o) of
    Left message -> pure (Hspec.Result "" (Hspec.Failure Nothing (Hspec.Reason message)))
    Right (resolved, o) -> do
      report <- Config.execute (\_ -> pure ()) resolved o body
      useColor <- shouldUseColor
      pref <- Style.preference stdout
      Hspec.Result info status <- toHspecResult useColor pref report
      let instructions = T.unpack (Style.cleanFor pref (Config.replayInstructions False resolved o))
      pure case status of
        Hspec.Failure loc (Hspec.Reason reason) -> Hspec.Result info (Hspec.Failure loc (Hspec.Reason (reason <> instructions)))
        _ -> Hspec.Result (info <> instructions) status

-- | A property as a keyed hspec example: a drop-in for @it@ that derives a
-- stable example-database key from the test's @describe@ & @it@ labels (salted
-- with the call-site module via 'HasCallStack') and persists failures for
-- replay.
--
-- @
-- describe "reverse" do
--   prop "is involutive" do
--     xs <- 'Hegel.Property.forAll' (Gen.list (Gen.int & Gen.build) & Gen.build)
--     reverse (reverse xs) 'Hegel.Property.===' xs
-- @
--
-- The key is built from the same describe\/it path as hspec's @--match@
-- identity (plus a module salt): rewording the label orphans that test's
-- stored failures, exactly as renaming would.
--
-- For explicit 'Settings', use 'propWith'.
prop :: (HasCallStack) => String -> Property () -> Hspec.Spec
prop = withFrozenCallStack (propWith defaultSettings {database = DatabaseDefault})

-- | 'prop' with explicit 'Settings'.
--
-- A key is derived from the path only when @settings@ has no 'databaseKey' of
-- its own.
--
-- Persistence follows the settings as given, so @propWith def@ runs with no
-- database, while a 'Settings' whose 'database' is set persists there.
propWith :: (HasCallStack) => Settings -> String -> Property () -> Hspec.Spec
propWith settings label body = do
  path <- Hspec.getSpecDescriptionPath
  let settings' = case settings.databaseKey of
        Just _ -> settings
        Nothing -> withDatabaseKey (propKey callStack path label) settings
  Hspec.it label (HegelExample callStack settings' body)

-- | 'prop' for a property over a custom base monad @m@.
--
-- The first argument turns the fixture @env@ (from an enclosing @around@\/
-- @before@) into a runner @(forall x. m x -> IO x)@ that collapses @m@ to 'IO';
-- for a @ReaderT Env IO@ stack that is just @\\env m -> runReaderT m env@:
--
-- @
-- around withEnv $
--   propT (\\env m -> runReaderT m env) "round-trips" prop_roundTrip
-- @
--
-- Persistence is enabled as for 'prop'.
--
-- __NOTE__: Replays reproduce stored failures /only/ when the fixture is
-- deterministic\/in-memory!
--
-- The choice sequence is replayed against whatever @env@ the fixture builds
-- next run, so against external mutable state (e.g. a live database) a stored
-- counterexample may not re-trigger.
--
-- Use 'propWithT' to provide an explicit 'Settings' record.
propT ::
  (HasCallStack) =>
  (env -> forall x. m x -> IO x) ->
  String ->
  PropertyT m () ->
  Hspec.SpecWith env
propT = keyedT callStack defaultSettings {database = DatabaseDefault}

-- | 'propT' with explicit 'Settings', mirroring 'propWith'.
propWithT ::
  (HasCallStack) =>
  Settings ->
  (env -> forall x. m x -> IO x) ->
  String ->
  PropertyT m () ->
  Hspec.SpecWith env
propWithT settings = keyedT callStack settings

-- | Shared implementation of 'propT'\/'propWithT'.
--
-- Takes the 'CallStack' explicitly so the public entry points capture the
-- user's call site for the module salt.
keyedT ::
  CallStack ->
  Settings ->
  (env -> forall x. m x -> IO x) ->
  String ->
  PropertyT m () ->
  Hspec.SpecWith env
keyedT cs settings nat label body = do
  path <- Hspec.getSpecDescriptionPath
  let settings' = case settings.databaseKey of
        Just _ -> settings
        Nothing -> withDatabaseKey (propKey cs path label) settings
  Hspec.it label (HegelExampleT cs settings' nat body)

-- | Returns 'True' when ANSI color output is appropriate: the output handle
-- is a terminal AND the @NO_COLOR@ environment variable is unset.
shouldUseColor :: IO Bool
shouldUseColor = do
  noColor <- isJust <$> lookupEnv "NO_COLOR"
  if noColor
    then pure False
    else hIsTerminalDevice stderr

toHspecResult :: Bool -> Style.Preference -> Report -> IO Hspec.Result
toHspecResult useColor pref report = case report.result of
  Ok -> pure (Hspec.Result (T.unpack (clean (render report))) Hspec.Success)
  Failures outcomes ->
    -- The ┏━━ header already shows the file, so there's no need to duplicate
    -- it in hspec's Location slot — but we still fill that slot so hspec can
    -- jump to the right line.
    renderFailureAt (firstOutcomeLoc outcomes)
  GaveUp msg ->
    pure (failed Nothing (Hspec.Reason (T.unpack (clean ("gave up: " <> msg)))))
  Aborted (Errored e) ->
    pure (failed Nothing (Hspec.Error Nothing e))
  Aborted (UnhealthyInput msg) ->
    pure (failed Nothing (Hspec.Reason (T.unpack (clean ("health check failed: " <> msg)))))
  where
    render = if useColor then renderReportAnsi else renderReport
    renderFailureAt loc = do
      rendered <- renderReportAuto useColor pref report
      pure (failed (hspecLocation <$> loc) (Hspec.Reason (T.unpack rendered)))
    -- Every string handed to hspec is cleaned: the 7-bit guarantee covers
    -- gave-up and abort messages (user text) too, not just counterexamples.
    clean = Style.cleanFor pref

    firstOutcomeLoc :: NonEmpty FailureOutcome -> Maybe SrcLoc
    firstOutcomeLoc =
      listToMaybe
        . mapMaybe
          ( \outcome -> case outcome.failureEvidence of
              Reconstructed evidence -> evidence.loc
              Observed evidence -> evidence.loc
              Diverged _ -> Nothing
              Skipped _ -> Nothing
          )
        . toList
    failed loc reason = Hspec.Result "" (Hspec.Failure loc reason)

hspecLocation :: SrcLoc -> Hspec.Location
hspecLocation sl =
  Hspec.Location
    { Hspec.locationFile = sl.srcLocFile,
      Hspec.locationLine = sl.srcLocStartLine,
      Hspec.locationColumn = sl.srcLocStartCol
    }

-- | Customize the persisted defaults of 'prop'.
propModify :: (HasCallStack) => (Settings -> Settings) -> String -> Property () -> Hspec.Spec
propModify modify = withFrozenCallStack (propWith (modify defaultSettings {database = DatabaseDefault}))

-- | Run a fixture property with a key derived from its module and describe path.
-- The fixture spans the whole run; per-case cleanup belongs in the property.
propFor :: (HasCallStack) => String -> (fixture -> Property ()) -> Hspec.SpecWith fixture
propFor = withFrozenCallStack (propForWith defaultSettings {database = DatabaseDefault})

-- | 'propFor' with explicit settings, honoring an explicit database key.
propForWith :: (HasCallStack) => Settings -> String -> (fixture -> Property ()) -> Hspec.SpecWith fixture
propForWith settings label body = do
  path <- Hspec.getSpecDescriptionPath
  let keyed = case settings.databaseKey of
        Just _ -> settings
        Nothing -> withDatabaseKey (propKey callStack path label) settings
  Hspec.it label (HegelFixture callStack keyed body)

data HegelFixture fixture = HegelFixture CallStack Settings (fixture -> Property ())

instance Hspec.Example (HegelFixture fixture) where
  type Arg (HegelFixture fixture) = fixture
  evaluateExample (HegelFixture cs settings body) _params aroundAction _progress =
    let ?callStack = cs in withFrozenCallStack $ withAroundResult aroundAction (runProperty settings . body)

-- | Customize the persisted defaults of 'propFor'.
propForModify :: (HasCallStack) => (Settings -> Settings) -> String -> (fixture -> Property ()) -> Hspec.SpecWith fixture
propForModify modify = withFrozenCallStack (propForWith (modify defaultSettings {database = DatabaseDefault}))

-- | Customize the persisted defaults of 'propT'.
propModifyT :: (HasCallStack) => (Settings -> Settings) -> (env -> forall x. m x -> IO x) -> String -> PropertyT m () -> Hspec.SpecWith env
propModifyT modify = keyedT callStack (modify defaultSettings {database = DatabaseDefault})
