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

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text qualified as T
import GHC.Stack (SrcLoc (..))
import Hegel.Native.Runner (check)
import Hegel.Property.Internal (PropertyT)
import Hegel.Report (Abort (..), Report (..), Result (..), renderFailure, renderReport)
import Hegel.Settings (defaultSettings)
import Test.Hspec.Core.Spec qualified as Hspec

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
      writeIORef ref (toHspecResult report)
    readIORef ref

toHspecResult :: Report -> Hspec.Result
toHspecResult report = case report.result of
  Ok -> Hspec.Result (T.unpack (renderReport report)) Hspec.Success
  Counterexample {message, notes, loc} ->
    failed
      (hspecLocation <$> loc)
      -- The location travels in hspec's own slot; keep it out of the text.
      (Hspec.Reason (T.unpack (renderFailure message notes Nothing)))
  GaveUp msg ->
    failed Nothing (Hspec.Reason ("gave up: " <> T.unpack msg))
  Aborted (Errored e) ->
    failed Nothing (Hspec.Error Nothing e)
  Aborted (UnhealthyInput msg) ->
    failed Nothing (Hspec.Reason ("health check failed: " <> T.unpack msg))
  where
    failed loc reason = Hspec.Result "" (Hspec.Failure loc reason)

hspecLocation :: SrcLoc -> Hspec.Location
hspecLocation sl =
  Hspec.Location
    { Hspec.locationFile = sl.srcLocFile,
      Hspec.locationLine = sl.srcLocStartLine,
      Hspec.locationColumn = sl.srcLocStartCol
    }
