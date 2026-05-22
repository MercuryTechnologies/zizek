{-# LANGUAGE ScopedTypeVariables #-}

module Hegel.Runner
  ( Settings (..)
  , defaultSettings
  , runPropertyWith
  ) where

import CBOR.Decode qualified as CD
import CBOR.Encode qualified as CE
import CBOR.Value (Value (..))
import Control.Exception (SomeException, toException, try)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word32, Word64)
import Hegel.DataSource (Status (..), markComplete, newDataSource)
import Hegel.Generators (Generator, draw)
import Hegel.Outcome (Outcome (..), Stats (..))
import Hegel.Protocol.Cbor
  ( asBool
  , asText
  , asWord32
  , asWord64
  , buildMap
  , boolVal
  , intVal
  , lookupKey
  , nullVal
  , textVal
  )
import Hegel.Protocol.Connection (Connection, connectStream, newStream)
import Hegel.Protocol.Stream
  ( Stream
  , mkStream
  , receiveRequest
  , requestCbor
  , writeReply
  )
import Hegel.Session (Session (..), getOrInitSession)
import Hegel.TestCase (TestCase (..))
import UnliftIO.MVar (withMVar)

data Settings = Settings
  { testCases :: !Int
  , seed      :: !(Maybe Word64)
  }
  deriving stock (Show)

defaultSettings :: Settings
defaultSettings = Settings {testCases = 100, seed = Nothing}

data CaseResult a
  = CaseValid
  | CaseInteresting !Text !(Maybe a)

runCase
  :: forall a
   . Connection
  -> Word32
  -> Generator a
  -> (a -> IO ())
  -> IO (CaseResult a)
runCase conn sid gen body = do
  (_, caseQ) <- connectStream conn sid
  caseStream <- mkStream conn sid caseQ
  ds <- newDataSource caseStream
  let tc = TestCase ds
  eVal <- try (draw tc gen) :: IO (Either SomeException a)
  case eVal of
    Left exc -> do
      let msg = T.pack (show exc)
      markComplete ds (Interesting msg)
      pure (CaseInteresting msg Nothing)
    Right val -> do
      eRes <- try (body val) :: IO (Either SomeException ())
      case eRes of
        Right () -> do
          markComplete ds Valid
          pure CaseValid
        Left exc -> do
          let msg = T.pack (show exc)
          markComplete ds (Interesting msg)
          pure (CaseInteresting msg (Just val))

runPropertyWith
  :: Settings
  -> Generator a
  -> (a -> IO ())
  -> IO (Outcome a)
runPropertyWith settings gen body = do
  ses <- getOrInitSession
  let conn = ses.conn

  (testSid, testQ) <- newStream conn
  testStream <- mkStream conn testSid testQ

  let phases = Array (V.fromList (map textVal defaultPhases))
  let runTestMsg =
        buildMap
          [ ("command", textVal "run_test")
          , ("test_cases", intVal settings.testCases)
          , ("seed", maybe nullVal UInt settings.seed)
          , ("stream_id", intVal testSid)
          , ("database_key", nullVal)
          , ("database", nullVal)
          , ("derandomize", boolVal False)
          , ("report_multiple_failures", boolVal False)
          , ("suppress_health_check", Array V.empty)
          , ("phases", phases)
          ]

  withMVar ses.control \ctrl -> do
    result <- requestCbor ctrl runTestMsg
    case result of
      Bool True -> pure ()
      other     -> fail $ "run_test: unexpected reply: " <> show other

  (results, nInteresting) <- runEventLoop testStream conn gen body

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
                 else pure (Passed Stats {testsRun = settings.testCases, invalid = 0})
          else replayFinalCases testStream conn (fromIntegral nInteresting) gen body

runEventLoop
  :: Stream
  -> Connection
  -> Generator a
  -> (a -> IO ())
  -> IO (Value, Word64)
runEventLoop testStream conn gen body = go
  where
    ackNull = CE.encode (buildMap [("result", nullVal)])
    ackTrue = CE.encode (buildMap [("result", boolVal True)])

    go = do
      (evId, evBytes) <- receiveRequest testStream
      case CD.decode evBytes of
        Left err  -> fail $ "runEventLoop: CBOR decode failed: " <> err
        Right evt -> case lookupKey "event" evt >>= asText of
          Nothing  -> fail "runEventLoop: event missing 'event' field"
          Just "test_case" -> do
            sid <- maybe (fail "runEventLoop: test_case missing stream_id") pure
                     (lookupKey "stream_id" evt >>= asWord32)
            let isFinal = maybe False id (lookupKey "is_final" evt >>= asBool)
            if isFinal
              then fail "runEventLoop: unexpected is_final=true during main loop"
              else do
                writeReply testStream evId ackNull
                _ <- runCase conn sid gen body
                go
          Just "test_done" -> do
            writeReply testStream evId ackTrue
            let r = maybe nullVal id (lookupKey "results" evt)
            let n = maybe 0 id (lookupKey "interesting_test_cases" r >>= asWord64)
            pure (r, n)
          Just other ->
            fail $ "runEventLoop: unknown event type: " <> T.unpack other

replayFinalCases
  :: Stream
  -> Connection
  -> Int
  -> Generator a
  -> (a -> IO ())
  -> IO (Outcome a)
replayFinalCases testStream conn n gen body = go n Nothing
  where
    ackNull = CE.encode (buildMap [("result", nullVal)])

    go 0 mFail = pure $ case mFail of
      Just (v, msg) -> Failed v msg []
      Nothing       -> Passed Stats {testsRun = 0, invalid = 0}
    go k mFail = do
      (evId, evBytes) <- receiveRequest testStream
      case CD.decode evBytes of
        Left err -> fail $ "replayFinalCases: CBOR decode: " <> err
        Right evt -> do
          sid <- maybe (fail "replayFinalCases: test_case missing stream_id") pure
                   (lookupKey "stream_id" evt >>= asWord32)
          let isFinal = maybe False id (lookupKey "is_final" evt >>= asBool)
          if not isFinal
            then fail "replayFinalCases: expected is_final=true"
            else pure ()
          writeReply testStream evId ackNull
          result <- runCase conn sid gen body
          case result of
            CaseValid ->
              go (k - 1) mFail
            CaseInteresting _ Nothing ->
              go (k - 1) mFail
            CaseInteresting msg (Just v) ->
              go (k - 1) (Just (v, msg))

defaultPhases :: [Text]
defaultPhases = ["explicit", "reuse", "generate", "target", "shrink"]
