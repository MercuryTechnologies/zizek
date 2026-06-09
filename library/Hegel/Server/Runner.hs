-- | Server-backend property runner.
module Hegel.Server.Runner
  ( -- * Running properties
    runProperty,
    runPropertyOn,
  )
where

import Control.Exception (toException)
import Hegel.Gen.Internal (Gen)
import Hegel.Outcome (Outcome (..))
import Hegel.Server.Client (runTest)
import Hegel.Server.Protocol.Error (ConnectionClosedError (..), ProtocolError (..))
import Hegel.Server.Session (Session, invalidateSession)
import Hegel.Server.Session.Internal (LiveSession (..), globalSession, liveSession)
import Hegel.Settings (Settings)
import UnliftIO.Exception (Handler (..), catches)

-- | Run a property against the global server session.
runProperty ::
  forall a.
  Settings ->
  Gen a ->
  (a -> IO ()) ->
  IO (Outcome a)
runProperty = runPropertyOn globalSession

-- | Run a property against an explicit 'Session'.
--
-- Connection or protocol failures invalidate the session and surface as
-- 'Errored' outcomes.
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
    run = do
      live <- liveSession ses
      runTest live.client settings gen body
    recover se = do
      invalidateSession ses
      pure (Errored se)
