{-# LANGUAGE ScopedTypeVariables #-}

module Hegel.Runner
  ( Settings (..),
    defaultSettings,
    runPropertyWith,
  )
where

import CBOR.Decode qualified as CD
import CBOR.Encode qualified as CE
import CBOR.Value (Value (..))
import Control.Exception (SomeException, toException)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word32, Word64)
import Hegel.DataSource (Status (..), markComplete, newDataSource)
import Hegel.Generators (Generator, InvalidTestCase (..), draw)
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
import Hegel.Protocol.Connection (Connection, connectStream, newStream)
import Hegel.Protocol.Error (ConnectionClosedError (..), ProtocolError (..))
import Hegel.Protocol.Stream
  ( Stream,
    mkStream,
    receiveRequest,
    requestCbor,
    writeReply,
  )
import Hegel.Session (Session (..), getOrInitSession, invalidateSession)
import Hegel.TestCase (TestCase (..))
import UnliftIO.Exception (Handler (..), catches, throwIO, tryAny)

data Settings = Settings
  { testCases :: !Int,
    seed :: !(Maybe Word64),
    derandomize :: !Bool,
    phases :: ![Phase],
    reportMultipleFailures :: !Bool,
    suppressHealthCheck :: ![Text]
  }
  deriving stock (Show)

defaultSettings :: Settings
defaultSettings =
  Settings
    { testCases = 100,
      seed = Nothing,
      derandomize = False,
      phases = [Explicit, Reuse, Generate, Target, Shrink],
      reportMultipleFailures = False,
      suppressHealthCheck = []
    }

data CaseResult a
  = CaseValid
  | CaseInvalid
  | CaseInteresting !Text !(Maybe a)

runCase ::
  forall a.
  Connection ->
  Word32 ->
  Generator a ->
  (a -> IO ()) ->
  IO (CaseResult a)
runCase conn sid gen body = do
  (_, caseQ) <- connectStream conn sid
  caseStream <- mkStream conn sid caseQ
  ds <- newDataSource caseStream
  let tc = TestCase ds
  eVal <-
    (Right <$> draw tc gen)
      `catches` [ Handler \InvalidTestCase -> pure (Left Nothing),
                  Handler \(e :: SomeException) -> pure (Left (Just e))
                ]
  case eVal of
    Left Nothing -> pure CaseInvalid
    Left (Just exc) -> do
      let msg = T.pack (show exc)
      markComplete ds (Interesting msg)
      pure (CaseInteresting msg Nothing)
    Right val -> do
      eRes <- tryAny (body val)
      case eRes of
        Right () -> do
          markComplete ds Valid
          pure CaseValid
        Left exc -> do
          let msg = T.pack (show exc)
          markComplete ds (Interesting msg)
          pure (CaseInteresting msg (Just val))

runPropertyWith ::
  Settings ->
  Generator a ->
  (a -> IO ()) ->
  IO (Outcome a)
runPropertyWith settings gen body =
  runProperty' settings gen body
    `catches` [ Handler \(e :: ConnectionClosedError) -> recover (toException e),
                Handler \(e :: ProtocolError) -> recover (toException e)
              ]
  where
    recover :: SomeException -> IO (Outcome a)
    recover se = do
      invalidateSession
      pure (Errored se)

runProperty' ::
  Settings ->
  Generator a ->
  (a -> IO ()) ->
  IO (Outcome a)
runProperty' settings gen body = do
  ses <- getOrInitSession
  let conn = ses.conn

  (testSid, testQ) <- newStream conn
  testStream <- mkStream conn testSid testQ

  let phasesVal = Array (V.fromList (map (textVal . toWire) settings.phases))
  let suppressVal = Array (V.fromList (map textVal settings.suppressHealthCheck))
  let runTestMsg =
        buildMap
          [ ("command", textVal "run_test"),
            ("test_cases", intVal settings.testCases),
            ("seed", maybe nullVal UInt settings.seed),
            ("stream_id", intVal testSid),
            ("database_key", nullVal), -- See misc/03-database-and-test-replay.md
            ("database", nullVal), -- See misc/03-database-and-test-replay.md
            ("derandomize", boolVal settings.derandomize),
            ("report_multiple_failures", boolVal settings.reportMultipleFailures),
            ("suppress_health_check", suppressVal),
            ("phases", phasesVal)
          ]

  result <- requestCbor ses.control runTestMsg
  case result of
    Bool True -> pure ()
    other -> throwIO (UnexpectedReply "run_test" other)

  (results, nInteresting, nInvalid) <- runEventLoop testStream conn gen body

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
                  else pure (Passed Stats {testsRun = settings.testCases, invalid = fromIntegral nInvalid})
          else replayFinalCases testStream conn (fromIntegral nInteresting) (fromIntegral nInvalid) gen body

runEventLoop ::
  Stream ->
  Connection ->
  Generator a ->
  (a -> IO ()) ->
  IO (Value, Word64, Word64)
runEventLoop testStream conn gen body = go
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
                _ <- runCase conn sid gen body
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
  Generator a ->
  (a -> IO ()) ->
  IO (Outcome a)
replayFinalCases testStream conn n nInvalid gen body = go n Nothing
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
          result <- runCase conn sid gen body
          case result of
            CaseValid ->
              go (k - 1) mFail
            CaseInvalid ->
              go (k - 1) mFail
            CaseInteresting _ Nothing ->
              go (k - 1) mFail
            CaseInteresting msg (Just v) ->
              go (k - 1) (Just (v, msg))
