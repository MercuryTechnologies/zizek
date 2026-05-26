module Hegel.DataSource
  ( DataSource (..),
    Status (..),
    Label (..),
    newDataSource,
    generate,
    startSpan,
    stopSpan,
    markComplete,
  )
where

import CBOR.Decode qualified as CD
import CBOR.Encode qualified as CE
import CBOR.Value (Value (..))
import Data.Text (Text)
import Hegel.Protocol.Cbor (asText, boolVal, buildMap, intVal, lookupKey, nullVal, textVal)
import Hegel.Protocol.Error (ProtocolError (..))
import Hegel.Protocol.Stream (Stream, closeStream, requestCbor, requestRaw, sendRequest)
import UnliftIO.Exception (finally, onException, throwIO)

data Status
  = Valid
  | Invalid
  | Overrun
  | Interesting Text

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

data DataSource = DataSource
  { stream :: !Stream
  }

newDataSource :: Stream -> IO DataSource
newDataSource s = pure (DataSource s)

generate :: DataSource -> Value -> IO Value
generate ds schema =
  requestCbor
    ds.stream
    (buildMap [("command", textVal "generate"), ("schema", schema)])
    `onException` closeStream ds.stream

startSpan :: DataSource -> Label -> IO ()
startSpan ds label =
  sendRequest ds.stream . CE.encode $
    buildMap
      [ ("command", textVal "start_span"),
        ("label", intVal (labelValue label))
      ]

stopSpan :: DataSource -> Bool -> IO ()
stopSpan ds discard =
  sendRequest ds.stream . CE.encode $
    buildMap
      [ ("command", textVal "stop_span"),
        ("discard", boolVal discard)
      ]

markComplete :: DataSource -> Status -> IO ()
markComplete ds status =
  ( do
      let req =
            CE.encode $
              buildMap
                [ ("command", textVal "mark_complete"),
                  ("status", textVal statusText),
                  ("origin", origin)
                ]
      rep <- requestRaw ds.stream req
      case CD.decode rep of
        Left err -> throwIO (CborDecodeFailure "markComplete" err)
        Right val -> case lookupKey "error" val of
          Nothing -> pure ()
          Just _ -> case lookupKey "type" val >>= asText of
            Just "StopTest" -> pure ()
            _ -> throwIO (UnexpectedReply "markComplete" val)
  )
    `finally` closeStream ds.stream
  where
    (statusText, origin) = case status of
      Valid -> ("VALID", nullVal)
      Invalid -> ("INVALID", nullVal)
      Overrun -> ("OVERRUN", nullVal)
      Interesting msg -> ("INTERESTING", textVal msg)
