module Hegel.Runner
  ( Settings (..),
    defaultSettings,
    runPropertyOn,
  )
where

import Control.Exception (SomeException, toException)
import Hegel.Client (Settings (..), defaultSettings, runTest)
import Hegel.Gen.Internal (Generator)
import Hegel.Outcome (Outcome (..))
import Hegel.Protocol.Error (ConnectionClosedError (..), ProtocolError (..))
import Hegel.Session (Session, invalidateSession)
import Hegel.Session.Internal (LiveSession (..), liveSession)
import UnliftIO.Exception (Handler (..), catches)

runPropertyOn ::
  Session ->
  Settings ->
  Generator a ->
  (a -> IO ()) ->
  IO (Outcome a)
runPropertyOn ses settings gen body =
  ( do
      live <- liveSession ses
      runTest live.client settings gen body
  )
    `catches` [ Handler \(e :: ConnectionClosedError) -> recover (toException e),
                Handler \(e :: ProtocolError) -> recover (toException e)
              ]
  where
    recover :: SomeException -> IO (Outcome a)
    recover se = do
      invalidateSession ses
      pure (Errored se)
