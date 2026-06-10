-- | Server-backend property runner.
module Hegel.Server.Runner
  ( -- * Running properties
    runProperty,
    runPropertyWith,
    runPropertyOn,
    runPropertyOnWith,
    check,
    checkOn,
  )
where

import Control.Exception (toException)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Hegel.Gen.Internal (Gen)
import Hegel.Property.Internal (Property, forAllWith)
import Hegel.Report (Abort (..), Report, aborted, renderValue)
import Hegel.Server.Client (checkTest)
import Hegel.Server.Protocol.Error (ConnectionClosedError (..), ProtocolError (..))
import Hegel.Server.Session (Session, invalidateSession)
import Hegel.Server.Session.Internal (LiveSession (..), globalSession, liveSession)
import Hegel.Settings (Settings)
import Hegel.TestCase (UnsupportedCapability)
import UnliftIO.Exception (Handler (..), catches)

-- | Run a generator-plus-body property against the global server session:
-- 'runPropertyWith' rendering drawn values via 'show'.
runProperty ::
  forall a.
  (Show a) =>
  Settings ->
  Gen a ->
  (a -> IO ()) ->
  IO Report
runProperty = runPropertyOn globalSession

-- | 'runProperty' with an explicit renderer, for values without a 'Show'
-- instance (or with an unhelpful one).
runPropertyWith ::
  forall a.
  (a -> Text) ->
  Settings ->
  Gen a ->
  (a -> IO ()) ->
  IO Report
runPropertyWith = runPropertyOnWith globalSession

-- | 'runProperty' against an explicit 'Session'.
runPropertyOn ::
  forall a.
  (Show a) =>
  Session ->
  Settings ->
  Gen a ->
  (a -> IO ()) ->
  IO Report
runPropertyOn ses = runPropertyOnWith ses renderValue

-- | 'runPropertyWith' against an explicit 'Session': sugar for 'checkOn'
-- over @'forAllWith' render gen '>>=' 'liftIO' . body@.
runPropertyOnWith ::
  forall a.
  Session ->
  (a -> Text) ->
  Settings ->
  Gen a ->
  (a -> IO ()) ->
  IO Report
runPropertyOnWith ses render settings gen body =
  checkOn ses settings (forAllWith render gen >>= liftIO . body)

-- | Run a 'Property' against the global server session.
check :: Settings -> Property () -> IO Report
check = checkOn globalSession

-- | Run a 'Property' against an explicit 'Session'.
--
-- Connection or protocol failures invalidate the session and surface as
-- 'Errored' aborts.
checkOn :: Session -> Settings -> Property () -> IO Report
checkOn ses settings prop =
  recovering ses do
    live <- liveSession ses
    checkTest live.client settings prop

-- | Map mid-protocol aborts to an 'Errored' report, invalidating the session
-- so the next run starts from a fresh connection.
recovering :: Session -> IO Report -> IO Report
recovering ses run =
  run
    `catches` [ Handler \(e :: ConnectionClosedError) -> recover (toException e),
                Handler \(e :: ProtocolError) -> recover (toException e),
                -- A generator used a primitive this backend lacks. The
                -- exception escapes runCase mid-case (before the per-case
                -- stream is closed), so invalidate the session like the other
                -- mid-protocol aborts rather than reuse it in an unknown state.
                Handler \(e :: UnsupportedCapability) -> recover (toException e)
              ]
  where
    recover se = do
      invalidateSession ses
      pure (aborted (Errored se))
