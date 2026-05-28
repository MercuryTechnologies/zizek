-- | Per-test-case state and the protocol commands a generator can send
-- through it.
module Hegel.TestCase
  ( -- * Test case
    TestCase (..),

    -- * Generation
    generate,
    TestStopped (..),

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
import Hegel.Protocol.Cbor (asText, boolVal, buildMap, intVal, lookupKey, nullVal, textVal)
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
generate tc schema = req `onException` closeStream tc.stream
  where
    req = requestCbor tc.stream payload `catch` mapStopTest
    payload = buildMap [("command", textVal "generate"), ("schema", schema)]
    mapStopTest (e :: ServerError) = case e.errorType of
      "StopTest" -> throwIO TestStopped
      _ -> throwIO e

-- | Open a span that groups related draws under the given 'Label'.
startSpan :: TestCase -> Label -> IO ()
startSpan tc label =
  sendRequest tc.stream . CE.encode $
    buildMap
      [ ("command", textVal "start_span"),
        ("label", intVal (labelValue label))
      ]

-- | Close the most-recently-opened span. Pass 'True' to mark the span as
-- discarded so the server doesn't try to shrink within it.
stopSpan :: TestCase -> Bool -> IO ()
stopSpan tc discard =
  sendRequest tc.stream . CE.encode $
    buildMap
      [ ("command", textVal "stop_span"),
        ("discard", boolVal discard)
      ]

-- | Send the final 'Status' for this test case and close the stream.
markComplete :: TestCase -> Status -> IO ()
markComplete tc status = send `finally` closeStream tc.stream
  where
    send = do
      let req =
            CE.encode $
              buildMap
                [ ("command", textVal "mark_complete"),
                  ("status", textVal statusText),
                  ("origin", origin)
                ]
      rep <- requestRaw tc.stream req
      case CD.decode rep of
        Left err -> throwIO (CborDecodeFailure "markComplete" err)
        Right val -> case lookupKey "error" val of
          Nothing -> pure ()
          Just _ -> case lookupKey "type" val >>= asText of
            Just "StopTest" -> pure ()
            _ -> throwIO (UnexpectedReply "markComplete" val)
    (statusText, origin) = case status of
      Valid -> ("VALID", nullVal)
      Invalid -> ("INVALID", nullVal)
      Overrun -> ("OVERRUN", nullVal)
      Interesting msg -> ("INTERESTING", textVal msg)
