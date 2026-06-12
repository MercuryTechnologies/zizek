{-# OPTIONS_GHC -Wno-orphans #-}

-- | hspec integration: a @'PropertyT' IO ()@ is an hspec 'Hspec.Example',
-- so a property can be the body of an @it@ directly:
--
-- @
-- describe "reverse" $ do
--   it "is involutive" $ 'hegel' do
--     xs <- 'Hegel.Property.forAll' (Gen.list (Gen.int & Gen.build) & Gen.build)
--     reverse (reverse xs) 'Hegel.Property.===' xs
-- @
--
-- Properties run with 'defaultSettings'; for custom settings, call
-- 'Hegel.Property.check_' inside the @it@ instead (an @IO ()@ is already an
-- 'Hspec.Example'). The @arg ->@ instance composes with hspec's
-- @around@\/fixtures.
module Hegel.Hspec
  ( hegel,
  )
where

import Data.Maybe (isJust)
import Data.Text qualified as T
import GHC.Stack (SrcLoc (..))
import Hegel.Property.Internal (PropertyT)
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
import Hegel.Settings (defaultSettings)
import System.Environment (lookupEnv)
import System.IO (hIsTerminalDevice, stderr)
import Test.Hspec.Core.Spec qualified as Hspec
import UnliftIO.IORef (newIORef, readIORef, writeIORef)

-- | Pin an ambiguously-typed property block to @'PropertyT' IO ()@. This is
-- 'id'; it exists purely so type inference succeeds in a bare @do@ block
-- under @it@.
hegel :: PropertyT IO () -> PropertyT IO ()
hegel = id

instance (m ~ IO) => Hspec.Example (PropertyT m ()) where
  type Arg (PropertyT m ()) = ()
  evaluateExample prop = Hspec.evaluateExample (\() -> prop)

instance (m ~ IO) => Hspec.Example (arg -> PropertyT m ()) where
  type Arg (arg -> PropertyT m ()) = arg
  evaluateExample mkProp _params aroundAction _progress = do
    -- The result is produced inside the around-action (which owns the
    -- fixture), so smuggle it out through a ref.
    ref <- newIORef (Hspec.Result "" Hspec.Success)
    aroundAction \arg -> do
      report <- check defaultSettings (mkProp arg)
      -- hspec's Params carries no color preference (it has only QuickCheck
      -- args and smallcheck depth). Honour the standard NO_COLOR env var and
      -- fall back to a terminal check; this is the closest we can get to
      -- hspec's own color decision from within evaluateExample.
      useColor <- shouldUseColor
      result <- toHspecResult useColor report
      writeIORef ref result
    readIORef ref

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
    -- Use the source-aware rich renderer; falls back internally to the plain
    -- renderer when source files can't be read.  The ┏━━ header already shows
    -- the file, so there's no need to duplicate it in hspec's Location slot —
    -- but we still fill that slot so hspec can jump to the right line.
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
