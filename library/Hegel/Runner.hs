-- | Property runner with session lifecycle handling.
--
-- Wraps 'Hegel.Client.runTest' so that connection failures invalidate the
-- 'Session' (forcing a fresh @hegel@ process on the next run) and surface
-- as 'Errored' outcomes rather than escaping as exceptions.
module Hegel.Runner
  ( -- * Running properties
    runPropertyOn,

    -- * Re-exports
    Settings (..),
    defaultSettings,
  )
where

import Control.Exception (SomeException, toException)
import Hegel.Client (Settings (..), defaultSettings, runTest)
import Hegel.Gen.Internal (Gen)
import Hegel.Outcome (Outcome (..))
import Hegel.Protocol.Error (ConnectionClosedError (..), ProtocolError (..))
import Hegel.Session (Session, invalidateSession)
import Hegel.Session.Internal (LiveSession (..), liveSession)
import UnliftIO.Exception (Handler (..), catches)

-- | Run a property against the given 'Session'.
--
-- Connection or protocol failures invalidate the session and are returned
-- as 'Errored' outcomes.
runPropertyOn ::
  forall a.
  Session ->
  Settings ->
  Gen a ->
  (a -> IO ()) ->
  IO (Outcome a)
runPropertyOn ses settings gen body =
  run
    `catches` [ Handler \(e :: ConnectionClosedError) -> recover (toException e),
                Handler \(e :: ProtocolError) -> recover (toException e)
              ]
  where
    run :: IO (Outcome a)
    run = do
      live <- liveSession ses
      runTest live.client settings gen body
    recover :: SomeException -> IO (Outcome a)
    recover se = do
      invalidateSession ses
      pure (Errored se)
