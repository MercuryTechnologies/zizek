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
import Hegel.Protocol.Stream (Stream, closeStream, receiveReply, requestCbor, sendRequest)
import UnliftIO.Exception (throwIO, tryAny)
import UnliftIO.MVar (MVar, modifyMVar_, newMVar, withMVar)

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
  { stream :: !(MVar (Maybe Stream))
  }

newDataSource :: Stream -> IO DataSource
newDataSource s = DataSource <$> newMVar (Just s)

generate :: DataSource -> Value -> IO Value
generate ds schema = do
  result <- tryAny $
    withMVar ds.stream $ \ms -> case ms of
      Nothing -> fail "DataSource: already aborted (StopTest)"
      Just s ->
        requestCbor s $
          buildMap [("command", textVal "generate"), ("schema", schema)]
  case result of
    Right v -> pure v
    Left e -> do
      modifyMVar_ ds.stream $ \ms -> do
        case ms of
          Just s -> closeStream s
          Nothing -> pure ()
        pure Nothing
      throwIO e

-- | No-op stub until protocol span support is wired up.
startSpan :: DataSource -> Label -> IO ()
startSpan _ _ = pure ()

-- | No-op stub until protocol span support is wired up.
stopSpan :: DataSource -> Bool -> IO ()
stopSpan _ _ = pure ()

markComplete :: DataSource -> Status -> IO ()
markComplete ds status =
  modifyMVar_ ds.stream $ \ms -> case ms of
    Nothing -> pure Nothing
    Just s -> do
      let (statusText, origin) = case status of
            Valid -> ("VALID", nullVal)
            Invalid -> ("INVALID", nullVal)
            Overrun -> ("OVERRUN", nullVal)
            Interesting msg -> ("INTERESTING", textVal msg)
      let req =
            CE.encode $
              buildMap
                [ ("command", textVal "mark_complete"),
                  ("status", textVal statusText),
                  ("origin", origin)
                ]
      mid <- sendRequest s req
      rep <- receiveReply s mid
      case CD.decode rep of
        Left err -> fail $ "markComplete: CBOR decode: " <> err
        Right val -> case lookupKey "error" val of
          Nothing -> pure ()
          Just _ -> case lookupKey "type" val >>= asText of
            Just "StopTest" -> pure ()
            other -> fail $ "markComplete: unexpected server error: " <> show other
      closeStream s
      pure Nothing
