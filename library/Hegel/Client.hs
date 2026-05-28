{-# LANGUAGE ScopedTypeVariables #-}

-- | Lower-level client API for talking to a @hegel@ server.
--
-- Most code should use 'Hegel.runProperty' or 'Hegel.Runner.runPropertyOn'.
-- This module is the layer just below them, useful when you need direct
-- control over the 'Client' lifecycle.
module Hegel.Client
  ( -- * Client
    Client (..),
    newClient,

    -- * Settings
    Settings (..),
    defaultSettings,

    -- * Running tests
    runTest,
  )
where

import CBOR.Decode qualified as CD
import CBOR.Encode qualified as CE
import CBOR.Value (Value (..))
import Control.Exception (SomeException, toException)
import Data.ByteString.Char8 qualified as BS8
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word32, Word64)
import Hegel.Assertion (originOf)
import Hegel.Gen.Internal (AssumeRejected (..), Gen, draw)
import Hegel.Outcome (Outcome (..), Stats (..))
import Hegel.Phase (Phase (..), toWire)
import Hegel.Protocol.Cbor
  ( asBool,
    asText,
    asWord32,
    asWord64,
    boolVal,
    buildMap,
    intVal,
    lookupKey,
    nullVal,
    textVal,
  )
import Hegel.Protocol.Connection (Connection, connectStream, controlStream, newStream)
import Hegel.Protocol.Error (ProtocolError (..))
import Hegel.Protocol.Stream
  ( Stream,
    closeStream,
    mkStream,
    receiveRequest,
    requestCbor,
    requestRaw,
    writeReply,
  )
import Hegel.TestCase (Status (..), TestCase (..), TestStopped (..), markComplete)
import Text.Read (readMaybe)
import UnliftIO.Exception (Handler (..), catches, finally, throwIO, tryAny)

handshakeString :: BS8.ByteString
handshakeString = "hegel_handshake_start"

supportedProtocolLo :: (Int, Int)
supportedProtocolLo = (0, 15)

supportedProtocolHi :: (Int, Int)
supportedProtocolHi = (0, 15)

-- | A live connection to a @hegel@ server with a validated protocol version.
data Client = Client
  { connection :: !Connection,
    control :: !Stream,
    version :: !(Int, Int)
  }

-- | Configuration for a single property run.
data Settings = Settings
  { -- | Number of test cases to attempt.
    testCases :: !Int,
    -- | RNG seed. 'Nothing' picks a fresh seed each run.
    seed :: !(Maybe Word64),
    -- | Use a fixed, source-derived seed so failures reproduce; ignored when
    -- 'seed' is set.
    derandomize :: !Bool,
    -- | Phases the server should execute, in order.
    phases :: ![Phase],
    -- | When 'True', the server collects every distinct failure instead of
    -- stopping at the first.
    reportMultipleFailures :: !Bool,
    -- | Wire-protocol names of health checks to skip.
    suppressHealthCheck :: ![Text],
    -- | Action run after each test case, on both success and failure.
    perCaseFinalizer :: !(IO ())
  }

instance Show Settings where
  showsPrec p s =
    showParen (p > 10) $
      showString "Settings {testCases = "
        . shows s.testCases
        . showString ", seed = "
        . shows s.seed
        . showString ", derandomize = "
        . shows s.derandomize
        . showString ", phases = "
        . shows s.phases
        . showString ", reportMultipleFailures = "
        . shows s.reportMultipleFailures
        . showString ", suppressHealthCheck = "
        . shows s.suppressHealthCheck
        . showString ", perCaseFinalizer = <<function>>}"

-- | Defaults for a property run: 100 test cases, a fresh seed each run,
-- all phases enabled, and no per-case finalizer.
--
-- Customize by overriding individual fields:
--
-- > defaultSettings { testCases = 1000 }
defaultSettings :: Settings
defaultSettings =
  Settings
    { testCases = 100,
      seed = Nothing,
      derandomize = False,
      phases = [Explicit, Reuse, Generate, Target, Shrink],
      reportMultipleFailures = False,
      suppressHealthCheck = [],
      perCaseFinalizer = pure ()
    }

-- | Open the control stream, perform the handshake, and return a validated
-- 'Client'. Throws 'HandshakeFailure' or 'VersionMismatch' on failure.
newClient :: Connection -> IO Client
newClient conn = do
  (sid, q) <- controlStream conn
  ctrl <- mkStream conn sid q
  rep <- requestRaw ctrl handshakeString
  let decoded = BS8.unpack rep
  ver <- case dropPrefix "Hegel/" decoded of
    Nothing ->
      throwIO (HandshakeFailure (T.pack $ "Bad handshake response: " <> show decoded))
    Just v -> pure v
  parsed <- parseVersion ver
  if parsed < supportedProtocolLo || parsed > supportedProtocolHi
    then
      throwIO
        ( VersionMismatch
            (T.pack ver)
            (T.pack (showVer supportedProtocolLo))
            (T.pack (showVer supportedProtocolHi))
        )
    else pure Client {connection = conn, control = ctrl, version = parsed}

-- | Run a property test against the live server. Does not catch
-- 'ConnectionClosedError' or 'ProtocolError'; callers are responsible for
-- session recovery.
runTest ::
  Client ->
  Settings ->
  Gen a ->
  (a -> IO ()) ->
  IO (Outcome a)
runTest client settings gen body = do
  (testSid, testQ) <- newStream client.connection
  testStream <- mkStream client.connection testSid testQ

  let phasesVal = Array (V.fromList (map (textVal . toWire) settings.phases))
  let suppressVal = Array (V.fromList (map textVal settings.suppressHealthCheck))
  let runTestMsg =
        buildMap
          [ ("command", textVal "run_test"),
            ("test_cases", intVal settings.testCases),
            ("seed", maybe nullVal UInt settings.seed),
            ("stream_id", intVal testSid),
            ("database_key", nullVal),
            ("derandomize", boolVal settings.derandomize),
            ("report_multiple_failures", boolVal settings.reportMultipleFailures),
            ("suppress_health_check", suppressVal),
            ("phases", phasesVal)
          ]

  result <- requestCbor client.control runTestMsg
  case result of
    Bool True -> pure ()
    other -> throwIO (UnexpectedReply "run_test" other)

  let runAndInterpret = do
        (results, nInteresting, nInvalid) <-
          runEventLoop testStream client.connection gen body settings.perCaseFinalizer
        case lookupKey "health_check_failure" results >>= asText of
          Just msg -> pure (UnhealthyInput msg)
          Nothing -> case lookupKey "error" results >>= asText of
            Just msg -> pure (Errored (toException (userError (T.unpack msg))))
            Nothing ->
              if nInteresting == 0
                then
                  let nValid = maybe 0 id (lookupKey "valid_test_cases" results >>= asWord64)
                   in if nValid == 0
                        then pure (Rejected "no valid examples found")
                        else
                          pure
                            ( Passed
                                Stats
                                  { testsRun = settings.testCases,
                                    invalid = fromIntegral nInvalid
                                  }
                            )
                else
                  replayFinalCases
                    testStream
                    client.connection
                    (fromIntegral nInteresting)
                    (fromIntegral nInvalid)
                    gen
                    body
                    settings.perCaseFinalizer
  runAndInterpret `finally` closeStream testStream

data CaseResult a
  = CaseValid
  | CaseInvalid
  | CaseInteresting !Text !(Maybe a)

runCase ::
  forall a.
  Connection ->
  Word32 ->
  Gen a ->
  (a -> IO ()) ->
  IO () ->
  IO (CaseResult a)
runCase conn sid gen body finalizer = run `finally` finalizer
  where
    run = do
      (_, caseQ) <- connectStream conn sid
      caseStream <- mkStream conn sid caseQ
      let tc = TestCase caseStream
      eVal <-
        (Right <$> draw tc gen)
          `catches` [ Handler \AssumeRejected -> pure (Left Nothing),
                      Handler \TestStopped -> pure (Left Nothing),
                      Handler \(e :: SomeException) -> pure (Left (Just e))
                    ]
      case eVal of
        Left Nothing -> pure CaseInvalid
        Left (Just exc) -> do
          let msg = originOf exc
          markComplete tc (Interesting msg)
          pure (CaseInteresting msg Nothing)
        Right val -> do
          eRes <- tryAny (body val)
          case eRes of
            Right () -> do
              markComplete tc Valid
              pure CaseValid
            Left exc -> do
              let msg = originOf exc
              markComplete tc (Interesting msg)
              pure (CaseInteresting msg (Just val))

runEventLoop ::
  Stream ->
  Connection ->
  Gen a ->
  (a -> IO ()) ->
  IO () ->
  IO (Value, Word64, Word64)
runEventLoop testStream conn gen body finalizer = go
  where
    ackNull = CE.encode (buildMap [("result", nullVal)])
    ackTrue = CE.encode (buildMap [("result", boolVal True)])

    go = do
      (evId, evBytes) <- receiveRequest testStream
      case CD.decode evBytes of
        Left err -> throwIO (CborDecodeFailure "runEventLoop" err)
        Right evt -> case lookupKey "event" evt >>= asText of
          Nothing -> throwIO (MissingField "runEventLoop" "event")
          Just "test_case" -> do
            sid <-
              maybe
                (throwIO (MissingField "test_case" "stream_id"))
                pure
                (lookupKey "stream_id" evt >>= asWord32)
            let isFinal = maybe False id (lookupKey "is_final" evt >>= asBool)
            if isFinal
              then throwIO (ProtocolStateViolation "unexpected is_final=true during main loop")
              else do
                writeReply testStream evId ackNull
                _ <- runCase conn sid gen body finalizer
                go
          Just "test_done" -> do
            writeReply testStream evId ackTrue
            let r = maybe nullVal id (lookupKey "results" evt)
            let nInteresting = maybe 0 id (lookupKey "interesting_test_cases" r >>= asWord64)
            let nInvalid = maybe 0 id (lookupKey "invalid_test_cases" r >>= asWord64)
            pure (r, nInteresting, nInvalid)
          Just other ->
            throwIO (UnknownEvent other)

replayFinalCases ::
  Stream ->
  Connection ->
  Int ->
  Int ->
  Gen a ->
  (a -> IO ()) ->
  IO () ->
  IO (Outcome a)
replayFinalCases testStream conn n nInvalid gen body finalizer = go n Nothing
  where
    ackNull = CE.encode (buildMap [("result", nullVal)])

    go 0 mFail = pure $ case mFail of
      Just (v, msg) -> Failed {counterexample = v, message = msg, notes = []}
      Nothing -> Passed Stats {testsRun = 0, invalid = nInvalid}
    go k mFail = do
      (evId, evBytes) <- receiveRequest testStream
      case CD.decode evBytes of
        Left err -> throwIO (CborDecodeFailure "replayFinalCases" err)
        Right evt -> do
          sid <-
            maybe
              (throwIO (MissingField "test_case" "stream_id"))
              pure
              (lookupKey "stream_id" evt >>= asWord32)
          let isFinal = maybe False id (lookupKey "is_final" evt >>= asBool)
          if not isFinal
            then throwIO (ProtocolStateViolation "expected is_final=true")
            else pure ()
          writeReply testStream evId ackNull
          result <- runCase conn sid gen body finalizer
          case result of
            CaseValid -> go (k - 1) mFail
            CaseInvalid -> go (k - 1) mFail
            CaseInteresting _ Nothing -> go (k - 1) mFail
            CaseInteresting msg (Just v) -> go (k - 1) (Just (v, msg))

dropPrefix :: String -> String -> Maybe String
dropPrefix [] ys = Just ys
dropPrefix (x : xs) (y : ys)
  | x == y = dropPrefix xs ys
  | otherwise = Nothing
dropPrefix _ [] = Nothing

parseVersion :: String -> IO (Int, Int)
parseVersion s =
  case break (== '.') s of
    (maj, '.' : minS) -> case (readMaybe maj, readMaybe minS) of
      (Just a, Just b) -> pure (a, b)
      _ -> throwIO (HandshakeFailure (T.pack $ "Invalid version string: " <> s))
    _ -> throwIO (HandshakeFailure (T.pack $ "Invalid version string: " <> s))

showVer :: (Int, Int) -> String
showVer (maj, mn) = show maj <> "." <> show mn
