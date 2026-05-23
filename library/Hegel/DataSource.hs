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
import Hegel.Protocol.Cbor (asText, buildMap, lookupKey, nullVal, textVal)
import Hegel.Protocol.Stream (Stream, closeStream, requestCbor, requestRaw)
import UnliftIO.Exception (finally, onException)

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

-- | No-op stub until protocol span support is wired up.
startSpan :: DataSource -> Label -> IO ()
startSpan _ _ = pure ()

-- | No-op stub until protocol span support is wired up.
stopSpan :: DataSource -> Bool -> IO ()
stopSpan _ _ = pure ()

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
        Left err -> fail $ "markComplete: CBOR decode: " <> err
        Right val -> case lookupKey "error" val of
          Nothing -> pure ()
          Just _ -> case lookupKey "type" val >>= asText of
            Just "StopTest" -> pure ()
            other -> fail $ "markComplete: unexpected server error: " <> show other
  )
    `finally` closeStream ds.stream
  where
    (statusText, origin) = case status of
      Valid -> ("VALID", nullVal)
      Invalid -> ("INVALID", nullVal)
      Overrun -> ("OVERRUN", nullVal)
      Interesting msg -> ("INTERESTING", textVal msg)
