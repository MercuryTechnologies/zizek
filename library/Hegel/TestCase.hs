{-# LANGUAGE ViewPatterns #-}

-- | Per-test-case state and the protocol commands a generator can send
-- through it.
module Hegel.TestCase
  ( -- * Test case
    TestCase (..),

    -- * Generation
    generate,
    TestStopped (..),

    -- * Collections
    newCollection,
    collectionMore,
    collectionReject,

    -- * Spans
    Label (..),
    startSpan,
    stopSpan,

    -- * Completion
    Status (..),
    markComplete,
  )
where

import CBOR.Decode qualified as CD
import CBOR.Encode qualified as CE
import CBOR.Value (Value (..))
import Control.Exception (Exception)
import Data.Text (Text)
import Hegel.Protocol.Cbor (asBool, asInt, asText, buildMap, intVal, lookupKey, (.=))
import Hegel.Protocol.Error (ProtocolError (..), ServerError (..))
import Hegel.Protocol.Stream (Stream, closeStream, requestCbor, requestRaw, sendRequest)
import UnliftIO.Exception (catch, finally, onException, throwIO)

-- | The 'Stream' carrying one test case's generation traffic, passed to
-- 'Hegel.Gen.Internal.Draw' actions.
newtype TestCase = TestCase {stream :: Stream}

-- | Thrown when the server signals @StopTest@ during 'generate', meaning
-- the current test case should be abandoned. The runner treats this as a
-- discard.
data TestStopped = TestStopped
  deriving stock (Show)

instance Exception TestStopped

-- | Final outcome of a test case, sent via 'markComplete'.
data Status
  = -- | The case completed successfully.
    Valid
  | -- | The case was deliberately discarded.
    Invalid
  | -- | The case ran out of entropy mid-generation.
    Overrun
  | -- | The case failed; the payload is the origin string used for
    -- server-side deduplication.
    Interesting Text

-- | Span labels used to group related draws so the server can shrink them
-- as a unit. Values match @hegel@'s wire-level numeric ids.
data Label
  = LabelList
  | LabelListElement
  | LabelSet
  | LabelSetElement
  | LabelMap
  | LabelMapEntry
  | LabelTuple
  | LabelOneOf
  | LabelOptional
  | LabelFixedDict
  | LabelFlatMap
  | LabelFilter
  | LabelMapped
  | LabelSampledFrom
  | LabelEnumVariant
  deriving stock (Show)

labelValue :: Label -> Int
labelValue LabelList = 1
labelValue LabelListElement = 2
labelValue LabelSet = 3
labelValue LabelSetElement = 4
labelValue LabelMap = 5
labelValue LabelMapEntry = 6
labelValue LabelTuple = 7
labelValue LabelOneOf = 8
labelValue LabelOptional = 9
labelValue LabelFixedDict = 10
labelValue LabelFlatMap = 11
labelValue LabelFilter = 12
labelValue LabelMapped = 13
labelValue LabelSampledFrom = 14
labelValue LabelEnumVariant = 15

-- | Ask the server to produce a value matching the given CBOR schema;
-- throws 'TestStopped' on @StopTest@.
generate :: TestCase -> Value -> IO Value
generate tc sch = req `onException` closeStream tc.stream
  where
    req =
      requestCbor tc.stream (buildMap ["command" .= ("generate" :: Text), "schema" .= sch])
        `catch` mapStopTest
    mapStopTest :: ServerError -> IO Value
    mapStopTest (e :: ServerError) = case e.errorType of
      "StopTest" -> throwIO TestStopped
      _ -> throwIO e

-- | Ask the server to begin a new variable-length collection; returns an
-- integer collection id used with 'collectionMore' and 'collectionReject'.
-- Throws 'TestStopped' if the server signals @StopTest@.
newCollection :: TestCase -> Int -> Maybe Int -> IO Int
newCollection tc minSz maxSz = do
  rep <- requestCbor tc.stream req `catch` mapStopTest
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
    mapStopTest :: ServerError -> IO Value
    mapStopTest (e :: ServerError) = case e.errorType of
      "StopTest" -> throwIO TestStopped
      _ -> throwIO e

-- | Ask whether the server wants another element for the given collection.
-- Returns 'True' to draw one more element, 'False' when the collection is
-- complete. Throws 'TestStopped' on @StopTest@.
collectionMore :: TestCase -> Int -> IO Bool
collectionMore tc cid = do
  rep <- requestCbor tc.stream req `catch` mapStopTest
  case asBool rep of
    Just b -> pure b
    Nothing -> throwIO (UnexpectedReply "collectionMore" rep)
  where
    req =
      buildMap
        [ "command" .= ("collection_more" :: Text),
          "collection_id" .= cid
        ]
    mapStopTest :: ServerError -> IO Value
    mapStopTest (e :: ServerError) = case e.errorType of
      "StopTest" -> throwIO TestStopped
      _ -> throwIO e

-- | Notify the server that the last drawn element was rejected (e.g. a
-- duplicate). The optional reason string is advisory only. Throws
-- 'TestStopped' if the server exhausts its rejection budget with
-- @count < min_size@ and signals @StopTest@.
collectionReject :: TestCase -> Int -> Maybe Text -> IO ()
collectionReject tc cid why = do
  _ <- requestCbor tc.stream req `catch` mapStopTest
  pure ()
  where
    req =
      buildMap
        [ "command" .= ("collection_reject" :: Text),
          "collection_id" .= cid,
          ("why", maybe Null TextString why)
        ]
    mapStopTest :: ServerError -> IO Value
    mapStopTest (e :: ServerError) = case e.errorType of
      "StopTest" -> throwIO TestStopped
      _ -> throwIO e

-- | Open a span that groups related draws under the given 'Label'.
startSpan :: TestCase -> Label -> IO ()
startSpan tc (labelValue -> label) =
  sendRequest tc.stream . CE.encode $
    buildMap ["command" .= ("start_span" :: Text), "label" .= label]

-- | Close the most-recently-opened span. Pass 'True' to mark the span as
-- discarded so the server doesn't try to shrink within it.
stopSpan :: TestCase -> Bool -> IO ()
stopSpan tc isDiscard =
  sendRequest tc.stream . CE.encode $
    buildMap ["command" .= ("stop_span" :: Text), "discard" .= isDiscard]

-- | Send the final 'Status' for this test case and close the stream.
markComplete :: TestCase -> Status -> IO ()
markComplete tc status = send `finally` closeStream tc.stream
  where
    send = do
      let req =
            CE.encode $
              buildMap
                [ "command" .= ("mark_complete" :: Text),
                  "status" .= statusText,
                  "origin" .= originVal
                ]
      rep <- requestRaw tc.stream req
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
