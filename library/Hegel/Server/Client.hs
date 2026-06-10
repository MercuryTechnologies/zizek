-- | Lower-level client API for talking to a @hegel@ server.
module Hegel.Server.Client
  ( -- * Client
    Client (..),
    newClient,

    -- * Running tests
    runTest,
  )
where

import CBOR.Decode qualified as CD
import CBOR.Encode qualified as CE
import CBOR.Value (Value (..))
import Control.Exception (SomeException, toException)
import Data.ByteString.Char8 qualified as BS8
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32, Word64)
import Hegel.Assertion (originOf)
import Hegel.Gen.Internal (AssumeRejected (..), Gen, draw)
import Hegel.HealthCheck qualified as HealthCheck
import Hegel.Outcome (Outcome (..), Stats (..))
import Hegel.Phase qualified as Phase
import Hegel.Protocol.Cbor
  ( asBool,
    asText,
    asWord32,
    asWord64,
    buildMap,
    lookupKey,
    (.=),
  )
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

runTest ::
  Client ->
  Settings ->
  Gen a ->
  (a -> IO ()) ->
  IO (Outcome a)
runTest client settings gen body = do
  (testSid, testQ) <- newStream client.connection
  testStream <- mkStream client.connection testSid testQ
  let action tc = draw tc gen >>= body
  let runTestMsg =
        buildMap
          [ "command" .= ("run_test" :: Text),
            "test_cases" .= settings.testCases,
            "seed" .= settings.seed,
            "stream_id" .= testSid,
            "database_key" .= (),
            "derandomize" .= settings.derandomize,
            "report_multiple_failures" .= settings.reportMultipleFailures,
            "suppress_health_check" .= fmap HealthCheck.toWire settings.suppressHealthCheck,
            "phases" .= fmap Phase.toWire settings.phases
          ]
  result <- requestCbor client.control runTestMsg
  case result of
    Bool True -> pure ()
    other -> throwIO (UnexpectedReply "run_test" other)
  flip finally (closeStream testStream) do
    (results, nInteresting, nInvalid) <-
      runEventLoop testStream client.connection action settings.perCaseFinalizer
    interpretResults results nInteresting nInvalid do
      -- Counterexample capture, scoped to the replay phase (the only reader).
      -- The capturing action resets the ref at case start so a failure during
      -- the draw itself can never pair a stale value from an earlier final
      -- case with a new failure.
      drawnRef <- newIORef Nothing
      let capturingAction tc = do
            writeIORef drawnRef Nothing
            val <- draw tc gen
            writeIORef drawnRef (Just val)
            body val
      replayFinalCases testStream client.connection (fromIntegral nInteresting) (fromIntegral nInvalid) capturingAction (readIORef drawnRef) settings.perCaseFinalizer

interpretResults ::
  Value -> Word64 -> Word64 -> IO (Outcome a) -> IO (Outcome a)
interpretResults results nInteresting nInvalid replay
  | Just msg <- lookupKey "health_check_failure" results >>= asText = pure (UnhealthyInput msg)
  | Just msg <- lookupKey "error" results >>= asText = pure (Errored (toException (userError (T.unpack msg))))
  | nInteresting > 0 = replay
  | nValid == 0 = pure (Rejected "no valid examples found")
  -- Report the actual count of valid cases the engine ran, not the requested
  -- target, so 'Stats.valid' means the same thing it does on the native
  -- backend (which tallies valid cases itself in 'Hegel.Native.Runner').
  | otherwise = pure (Passed Stats {valid = fromIntegral nValid, invalid = fromIntegral nInvalid})
  where
    nValid = maybe 0 id (lookupKey "valid_test_cases" results >>= asWord64)

-- | Run one server-announced test case: execute the per-case action against a
-- live 'TestCase', classify how it finished, and report the resulting 'Status'
-- to the server. Returns the failure origin when the case was marked
-- interesting, 'Nothing' otherwise — the only distinction any caller consumes.
--
-- The classification covers the whole action, draw and body alike, so a
-- discard ('AssumeRejected') raised at any point marks the case 'Invalid'.
-- The handlers only classify; 'markComplete' runs once, outside the 'catches'
-- scope, so a protocol error raised while reporting propagates to
-- 'runTest''s caller instead of being misread as a test failure.
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

replayFinalCases :: Stream -> Connection -> Int -> Int -> (TestCase -> IO ()) -> IO (Maybe a) -> IO () -> IO (Outcome a)
replayFinalCases testStream conn n nInvalid action readDrawn finalizer = go n Nothing
  where
    go 0 mFail = pure $ case mFail of
      Just (v, msg) -> Failed {counterexample = v, message = msg, notes = []}
      Nothing -> Passed Stats {valid = 0, invalid = nInvalid}
    go k mFail = do
      (evId, ev) <- awaitEvent testStream
      case ev of
        TestCaseEvent sid True -> do
          writeReply testStream evId (encodeAck Null)
          runCase conn sid action finalizer >>= \case
            Nothing -> go (k - 1) mFail
            Just msg ->
              -- 'readDrawn' yields the value the action captured before the
              -- failure; a failure during the draw itself captures nothing,
              -- in which case keep any counterexample from an earlier case.
              readDrawn >>= \case
                Just v -> go (k - 1) (Just (v, msg))
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
