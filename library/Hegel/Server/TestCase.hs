-- | Server-backend 'TestCase' constructor.
module Hegel.Server.TestCase
  ( mkTestCase,
  )
where

import CBOR.Decode qualified as CD
import CBOR.Encode qualified as CE
import CBOR.Value (Value (..))
import Data.Text (Text)
import Hegel.Protocol.Cbor (asBool, asInt, asText, buildMap, intVal, lookupKey, (.=))
import Hegel.Server.Protocol.Error (ProtocolError (..), ServerError (..))
import Hegel.Server.Protocol.Stream (Stream, closeStream, requestCbor, requestRaw, sendRequest)
import Hegel.TestCase (Label, Status (..), TestCase (..), TestStopped (..), labelValue)
import UnliftIO.Exception (catch, finally, onException, throwIO)

mkTestCase :: Stream -> TestCase
mkTestCase s =
  TestCase
    { generate = serverGenerate s,
      newCollection = serverNewCollection s,
      collectionMore = serverCollectionMore s,
      collectionReject = serverCollectionReject s,
      startSpan = serverStartSpan s,
      stopSpan = serverStopSpan s,
      markComplete = serverMarkComplete s
    }

mapStopTest :: ServerError -> IO a
mapStopTest e = case e.errorType of
  "StopTest" -> throwIO TestStopped
  _ -> throwIO e

serverGenerate :: Stream -> Value -> IO Value
serverGenerate s sch =
  requestCbor s (buildMap ["command" .= ("generate" :: Text), "schema" .= sch])
    `catch` mapStopTest
    `onException` closeStream s

serverNewCollection :: Stream -> Int -> Maybe Int -> IO Int
serverNewCollection s minSz maxSz = do
  rep <-
    requestCbor s req
      `catch` mapStopTest
  case asInt rep of
    Just cid -> pure cid
    Nothing -> throwIO (UnexpectedReply "newCollection" rep)
  where
    req =
      buildMap
        [ "command" .= ("new_collection" :: Text),
          "min_size" .= minSz,
          ("max_size", maybe Null intVal maxSz)
        ]

serverCollectionMore :: Stream -> Int -> IO Bool
serverCollectionMore s cid = do
  rep <-
    requestCbor s (buildMap ["command" .= ("collection_more" :: Text), "collection_id" .= cid])
      `catch` mapStopTest
  case asBool rep of
    Just b -> pure b
    Nothing -> throwIO (UnexpectedReply "collectionMore" rep)

serverCollectionReject :: Stream -> Int -> Maybe Text -> IO ()
serverCollectionReject s cid why = do
  _ <-
    requestCbor
      s
      (buildMap ["command" .= ("collection_reject" :: Text), "collection_id" .= cid, ("why", maybe Null TextString why)])
      `catch` mapStopTest
  pure ()

serverStartSpan :: Stream -> Label -> IO ()
serverStartSpan s label =
  sendRequest s . CE.encode $
    buildMap ["command" .= ("start_span" :: Text), "label" .= labelValue label]

serverStopSpan :: Stream -> Bool -> IO ()
serverStopSpan s isDiscard =
  sendRequest s . CE.encode $
    buildMap ["command" .= ("stop_span" :: Text), "discard" .= isDiscard]

serverMarkComplete :: Stream -> Status -> IO ()
serverMarkComplete s status = send `finally` closeStream s
  where
    send = do
      let req =
            CE.encode $
              buildMap
                [ "command" .= ("mark_complete" :: Text),
                  "status" .= statusText,
                  "origin" .= originVal
                ]
      rep <- requestRaw s req
      case CD.decode rep of
        Left err -> throwIO (CborDecodeFailure "markComplete" err)
        Right val -> case lookupKey "error" val of
          Nothing -> pure ()
          Just _ -> case lookupKey "type" val >>= asText of
            Just "StopTest" -> pure ()
            _ -> throwIO (UnexpectedReply "markComplete" val)
    statusText :: Text
    (statusText, originVal) = case status of
      Valid -> ("VALID", Null)
      Invalid -> ("INVALID", Null)
      Overrun -> ("OVERRUN", Null)
      Interesting msg -> ("INTERESTING", TextString msg)
