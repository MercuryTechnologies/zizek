-- | Lower-level client API for talking to a @hegel@ server.
module Hegel.Server.Client
  ( -- * Client
    Client (..),
    newClient,

    -- * Running tests
    checkTest,
  )
where

import CBOR.Decode qualified as CD
import CBOR.Encode qualified as CE
import CBOR.Value (Value (..))
import Control.Exception (SomeException, toException)
import Data.ByteString.Char8 qualified as BS8
import Data.Foldable (for_)
import Data.Functor (($>), (<&>))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word32, Word64)
import Hegel.Assertion (originOf)
import Hegel.Database (Database (..))
import Hegel.Gen.Internal (AssumeRejected (..))
import Hegel.HealthCheck qualified as HealthCheck
import Hegel.Phase qualified as Phase
import Hegel.Property.Internal (Property, failureDetails, observeProperty, propertyAction)
import Hegel.Protocol.Cbor
  ( asBool,
    asText,
    asWord32,
    asWord64,
    buildMap,
    bytesVal,
    lookupKey,
    nullVal,
    textVal,
    (.=),
  )
import Hegel.Report (Abort (..), Report (..), Result (..), Stats (..))
import Hegel.Server.Protocol.Connection (Connection, connectStream, controlStream, newStream)
import Hegel.Server.Protocol.Error (ProtocolError (..))
import Hegel.Server.Protocol.Stream
  ( Stream,
    closeStream,
    mkStream,
    receiveRequest,
    requestCbor,
    requestRaw,
    writeReply,
  )
import Hegel.Server.TestCase (mkTestCase)
import Hegel.Settings (Settings (..))
import Hegel.TestCase (Status (..), TestCase, TestStopped (..), UnsupportedCapability, markComplete)
import Text.Read (readMaybe)
import UnliftIO.Exception (Handler (..), catches, finally, throwIO)

encodeAck :: Value -> BS8.ByteString
encodeAck v = CE.encode (buildMap ["result" .= v])

handshakeString :: BS8.ByteString
handshakeString = "hegel_handshake_start"

supportedProtocolLo :: (Int, Int)
supportedProtocolLo = (0, 15)

supportedProtocolHi :: (Int, Int)
supportedProtocolHi = (0, 15)

data Client = Client
  { connection :: !Connection,
    control :: !Stream,
    version :: !(Int, Int)
  }

newClient :: Connection -> IO Client
newClient conn = do
  (sid, q) <- controlStream conn
  ctrl <- mkStream conn sid q
  rep <- requestRaw ctrl handshakeString
  let decoded = BS8.unpack rep
  ver <- case dropPrefix "Hegel/" decoded of
    Nothing -> throwIO (HandshakeFailure (T.pack $ "Bad handshake response: " <> show decoded))
    Just v -> pure v
  parsed <- parseVersion ver
  if parsed < supportedProtocolLo || parsed > supportedProtocolHi
    then throwIO (VersionMismatch (T.pack ver) (T.pack (showVer supportedProtocolLo)) (T.pack (showVer supportedProtocolHi)))
    else pure Client {connection = conn, control = ctrl, version = parsed}

-- | Run a 'Property' against a @hegel@ server.
--
-- The counterexample is described by the notes journaled while the
-- final-replay case re-executes the property; the exception that reproduced
-- the failure supplies the message and source location (via
-- 'failureDetails').
checkTest :: Client -> Settings -> Property () -> IO Report
checkTest client settings prop = do
  capRef <- newIORef Nothing
  let capturing tc = do
        writeIORef capRef Nothing
        (eRes, notes) <- observeProperty tc prop
        case eRes of
          Right () -> pure ()
          Left e -> do
            writeIORef capRef (Just (e, notes))
            -- Rethrow so 'runCase' classifies and reports the case normally.
            throwIO e
      readCapture =
        readIORef capRef
          <&> fmap \(e, notes) msg ->
            let (message, loc) = failureDetails msg e
             in Counterexample {message, notes, loc}
  runTestWith client settings (propertyAction prop) capturing readCapture

-- | Shared @run_test@ scaffold: issue the command, drive the event loop with
-- the per-case action, then replay final cases with the capturing action.
runTestWith ::
  Client ->
  Settings ->
  -- | Per-case action for ordinary cases.
  (TestCase -> IO ()) ->
  -- | Per-case action for final-replay cases; must record its counterexample
  -- payload where the reader below can find it.
  (TestCase -> IO ()) ->
  -- | Read the payload captured by the final-replay action, as a function of
  -- the failure origin reported for that case.
  IO (Maybe (Text -> Result)) ->
  IO Report
runTestWith client settings action finalAction readCapture = do
  (testSid, testQ) <- newStream client.connection
  testStream <- mkStream client.connection testSid testQ
  let -- The server distinguishes an absent @database@ field (use the engine
      -- default store) from an explicit null (disable persistence).
      databaseField = case settings.database of
        DatabaseDefault -> []
        DatabaseDisabled -> [("database", nullVal)]
        DatabaseDirectory p -> [("database", textVal (T.pack p))]
      runTestMsg =
        buildMap $
          [ "command" .= ("run_test" :: Text),
            "test_cases" .= settings.testCases,
            "seed" .= settings.seed,
            "stream_id" .= testSid,
            ("database_key", maybe nullVal (bytesVal . encodeUtf8) settings.databaseKey),
            "derandomize" .= settings.derandomize,
            "report_multiple_failures" .= settings.reportMultipleFailures,
            "suppress_health_check" .= fmap HealthCheck.toWire settings.suppressHealthCheck,
            "phases" .= fmap Phase.toWire settings.phases
          ]
            <> databaseField
  result <- requestCbor client.control runTestMsg
  case result of
    Bool True -> pure ()
    other -> throwIO (UnexpectedReply "run_test" other)
  flip finally (closeStream testStream) do
    (results, nInteresting, nInvalid) <-
      runEventLoop testStream client.connection action settings.perCaseFinalizer
    interpretResults results nInteresting nInvalid $
      replayFinalCases testStream client.connection (fromIntegral nInteresting) finalAction readCapture settings.perCaseFinalizer

interpretResults ::
  Value -> Word64 -> Word64 -> IO Result -> IO Report
interpretResults results nInteresting nInvalid replay = report <$> verdict
  where
    verdict
      | Just msg <- lookupKey "health_check_failure" results >>= asText = pure (Aborted (UnhealthyInput msg))
      | Just msg <- lookupKey "error" results >>= asText = pure (Aborted (Errored (toException (userError (T.unpack msg)))))
      | nInteresting > 0 = replay
      | nValid == 0 = pure (GaveUp "no valid examples found")
      | otherwise = pure Ok
    -- Report the actual count of valid cases the engine ran, not the requested
    -- target, so 'Stats.valid' means the same thing it does on the native
    -- backend (which tallies valid cases itself in 'Hegel.Native.Runner').
    nValid = maybe 0 id (lookupKey "valid_test_cases" results >>= asWord64)
    report r = Report {result = r, stats = Stats {valid = fromIntegral nValid, invalid = fromIntegral nInvalid}}

-- | Run one server-announced test case: execute the per-case action against a
-- live 'TestCase', classify how it finished, and report the resulting 'Status'
-- to the server. Returns the failure origin when the case was marked
-- interesting, 'Nothing' otherwise — the only distinction any caller consumes.
--
-- The classification covers the whole action, draw and body alike, so a
-- discard ('AssumeRejected') raised at any point marks the case 'Invalid'.
-- The handlers only classify; 'markComplete' runs once, outside the 'catches'
-- scope, so a protocol error raised while reporting propagates to
-- 'runTestWith''s caller instead of being misread as a test failure.
runCase :: Connection -> Word32 -> (TestCase -> IO ()) -> IO () -> IO (Maybe Text)
runCase conn sid action finalizer = run `finally` finalizer
  where
    run = do
      (_, caseQ) <- connectStream conn sid
      caseStream <- mkStream conn sid caseQ
      let tc = mkTestCase caseStream
      mStatus <-
        (action tc $> Just Valid)
          `catches` [ Handler \AssumeRejected -> pure (Just Invalid),
                      -- The server tracks the choice budget itself, so a stop
                      -- needs no acknowledgement; just discard the case. (Any
                      -- stop raised by a protocol request has already closed
                      -- the case stream via that request's onException.)
                      Handler \TestStopped -> pure Nothing,
                      -- Not a counterexample: a generator used a primitive this
                      -- backend lacks. Let it propagate to runPropertyOn, which
                      -- maps it to Errored. Must precede the SomeException
                      -- handler, which would otherwise swallow it as a failure.
                      Handler \(e :: UnsupportedCapability) -> throwIO e,
                      Handler \(e :: SomeException) -> pure (Just (Interesting (originOf e)))
                    ]
      for_ mStatus (markComplete tc)
      pure case mStatus of
        Just (Interesting origin) -> Just origin
        _ -> Nothing

runEventLoop :: Stream -> Connection -> (TestCase -> IO ()) -> IO () -> IO (Value, Word64, Word64)
runEventLoop testStream conn action finalizer = go
  where
    go = do
      (evId, ev) <- awaitEvent testStream
      case ev of
        TestCaseEvent _ True -> throwIO (ProtocolStateViolation "unexpected is_final=true during main loop")
        TestCaseEvent sid False -> do
          writeReply testStream evId (encodeAck Null)
          _ <- runCase conn sid action finalizer
          go
        TestDoneEvent results -> do
          writeReply testStream evId (encodeAck (Bool True))
          let nI = maybe 0 id (lookupKey "interesting_test_cases" results >>= asWord64)
              nInv = maybe 0 id (lookupKey "invalid_test_cases" results >>= asWord64)
          pure (results, nI, nInv)

data Event = TestCaseEvent !Word32 !Bool | TestDoneEvent !Value

awaitEvent :: Stream -> IO (Word32, Event)
awaitEvent s = do
  (evId, evBytes) <- receiveRequest s
  evt <- case CD.decode evBytes of
    Left err -> throwIO (CborDecodeFailure "awaitEvent" err)
    Right v -> pure v
  ev <- case lookupKey "event" evt >>= asText of
    Nothing -> throwIO (MissingField "awaitEvent" "event")
    Just "test_case" -> do
      sid <- maybe (throwIO (MissingField "test_case" "stream_id")) pure (lookupKey "stream_id" evt >>= asWord32)
      let isFinal = maybe False id (lookupKey "is_final" evt >>= asBool)
      pure (TestCaseEvent sid isFinal)
    Just "test_done" -> pure (TestDoneEvent (maybe Null id (lookupKey "results" evt)))
    Just other -> throwIO (UnknownEvent other)
  pure (evId, ev)

replayFinalCases :: Stream -> Connection -> Int -> (TestCase -> IO ()) -> IO (Maybe (Text -> Result)) -> IO () -> IO Result
replayFinalCases testStream conn n action readCapture finalizer = go n Nothing
  where
    go 0 mFail = pure $ case mFail of
      Just r -> r
      -- The engine reported at least one failure, but no final-replay case
      -- reproduced it with a captured payload (the case diverged, or the
      -- failure was flaky on replay). Surface the mismatch rather than a
      -- silent pass.
      Nothing -> Aborted (Errored (toException (userError "failure reported but no counterexample was reproduced by the final replay")))
    go k mFail = do
      (evId, ev) <- awaitEvent testStream
      case ev of
        TestCaseEvent sid True -> do
          writeReply testStream evId (encodeAck Null)
          runCase conn sid action finalizer >>= \case
            Nothing -> go (k - 1) mFail
            Just msg ->
              -- 'readCapture' yields the payload the action captured before
              -- the failure; a failure during generation captures nothing,
              -- in which case keep any counterexample from an earlier case.
              readCapture >>= \case
                Just mk -> go (k - 1) (Just (mk msg))
                Nothing -> go (k - 1) mFail
        TestCaseEvent _ False -> throwIO (ProtocolStateViolation "expected is_final=true")
        TestDoneEvent _ -> throwIO (ProtocolStateViolation "unexpected test_done during replay")

dropPrefix :: String -> String -> Maybe String
dropPrefix [] ys = Just ys
dropPrefix (x : xs) (y : ys)
  | x == y = dropPrefix xs ys
  | otherwise = Nothing
dropPrefix _ [] = Nothing

parseVersion :: String -> IO (Int, Int)
parseVersion s = case break (== '.') s of
  (maj, '.' : minS) -> case (readMaybe maj, readMaybe minS) of
    (Just a, Just b) -> pure (a, b)
    _ -> throwIO (HandshakeFailure (T.pack $ "Invalid version: " <> s))
  _ -> throwIO (HandshakeFailure (T.pack $ "Invalid version: " <> s))

showVer :: (Int, Int) -> String
showVer (maj, mn) = show maj <> "." <> show mn
