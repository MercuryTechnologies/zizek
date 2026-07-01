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
-- Use 'propWith' for explicit 'Settings'; @propWith def@ runs a property with
-- no persistence. The @arg ->@ 'Hspec.Example' instance composes with hspec's
-- @around@\/fixtures. For a property over a custom base monad (e.g. a
-- @ReaderT Env IO@ application stack), use 'propT'\/'propWithT'.
module Hegel.Hspec
  ( prop,
    propT,
    propWith,
    propWithT,
  )
where

import Control.Monad ((>=>))
import Data.Maybe (isJust)
import Data.Text qualified as T
import GHC.Stack (CallStack, HasCallStack, SrcLoc (..), callStack, withFrozenCallStack)
import Hegel.Database (Database (..))
import Hegel.Internal.DatabaseKey (propKey)
import Hegel.Property.Internal (Property, PropertyT, hoist)
import Hegel.Report
  ( Abort (..),
    Report (..),
    Result (..),
    renderReport,
    renderReportAnsi,
    renderReportRich,
    renderReportRichAnsi,
  )
import Hegel.Runner (check)
import Hegel.Settings (Settings (..), defaultSettings, withDatabaseKey)
import System.Environment (lookupEnv)
import System.IO (hIsTerminalDevice, stderr)
import Test.Hspec.Core.Spec qualified as Hspec
import UnliftIO.IORef (newIORef, readIORef, writeIORef)

-- | A property that takes a fixture is an hspec 'Hspec.Example', so it composes
-- with @around@\/@aroundWith@. This path runs with 'defaultSettings' (no key,
-- no persistence); use 'prop' for keyed, persisted properties.
instance (m ~ IO) => Hspec.Example (arg -> PropertyT m ()) where
  type Arg (arg -> PropertyT m ()) = arg
  evaluateExample mkProp _params aroundAction _progress =
    withAroundResult aroundAction (runProperty defaultSettings . mkProp)

-- | A property paired with the 'Settings' it should run under, so the
-- example-database key derived by 'prop'\/'propWith' reaches the runner.
data HegelExample = HegelExample Settings (Property ())

instance Hspec.Example HegelExample where
  type Arg HegelExample = ()
  evaluateExample (HegelExample settings body) _params aroundAction _progress =
    withAroundResult aroundAction \() -> runProperty settings body

-- | A property over an arbitrary base monad @m@ (e.g. an application stack),
-- paired with the 'Settings' to run under and a runner that — given the fixture
-- @env@ — collapses @m@ to 'IO'. Built by 'propT'\/'propWithT'.
data HegelExampleT env m
  = HegelExampleT Settings (env -> forall x. m x -> IO x) (PropertyT m ())

instance Hspec.Example (HegelExampleT env m) where
  type Arg (HegelExampleT env m) = env
  evaluateExample (HegelExampleT settings nat body) _params aroundAction _progress =
    withAroundResult aroundAction \env -> runProperty settings (hoist (nat env) body)

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
runProperty :: Settings -> Property () -> IO Hspec.Result
runProperty settings body = do
  report <- check settings body
  useColor <- shouldUseColor
  toHspecResult useColor report

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
  Hspec.it label (HegelExample settings' body)

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

-- | Shared implementation of 'propT'\/'propWithT'. Takes the 'CallStack'
-- explicitly so the public entry points capture the user's call site (for the
-- module salt) rather than this module.
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
  Hspec.it label (HegelExampleT settings' nat body)

-- | Returns 'True' when ANSI colour output is appropriate: the output handle
-- is a terminal AND the @NO_COLOR@ environment variable is unset (per
-- <https://no-color.org>).
shouldUseColor :: IO Bool
shouldUseColor = do
  noColor <- isJust <$> lookupEnv "NO_COLOR"
  if noColor
    then pure False
    else hIsTerminalDevice stderr

toHspecResult :: Bool -> Report -> IO Hspec.Result
toHspecResult useColor report = case report.result of
  Ok -> pure (Hspec.Result (T.unpack (render report)) Hspec.Success)
  Counterexample {loc} -> do
    -- The ┏━━ header already shows the file, so there's no need to duplicate
    -- it in hspec's Location slot — but we still fill that slot so hspec can
    -- jump to the right line.
    rendered <- richRender report
    pure (failed (hspecLocation <$> loc) (Hspec.Reason (T.unpack rendered)))
  GaveUp msg ->
    pure (failed Nothing (Hspec.Reason ("gave up: " <> T.unpack msg)))
  Aborted (Errored e) ->
    pure (failed Nothing (Hspec.Error Nothing e))
  Aborted (UnhealthyInput msg) ->
    pure (failed Nothing (Hspec.Reason ("health check failed: " <> T.unpack msg)))
  where
    render = if useColor then renderReportAnsi else renderReport
    richRender = if useColor then renderReportRichAnsi else renderReportRich
    failed loc reason = Hspec.Result "" (Hspec.Failure loc reason)

hspecLocation :: SrcLoc -> Hspec.Location
hspecLocation sl =
  Hspec.Location
    { Hspec.locationFile = sl.srcLocFile,
      Hspec.locationLine = sl.srcLocStartLine,
      Hspec.locationColumn = sl.srcLocStartCol
    }
