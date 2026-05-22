module Hegel.Protocol.Stream
  ( Stream,
    mkStream,
    streamId,
    sendRequest,
    writeReply,
    receiveReply,
    receiveRequest,
    closeStream,
    markClosed,
    requestCbor,
  )
where

import CBOR.Decode qualified as CD
import CBOR.Encode qualified as CE
import CBOR.Value (Value (..))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (TBQueue, readTBQueue)
import Data.Bits (shiftL)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word32)
import Hegel.Protocol.Cbor (asText, lookupKey)
import Hegel.Protocol.Connection (Connection, sendPacket, unregisterStream)
import Hegel.Protocol.Packet (Packet (..))

closeStreamPayload :: ByteString
closeStreamPayload = BS.singleton 0xFE

closeStreamMessageId :: Word32
closeStreamMessageId = (1 `shiftL` 31) - 1

data Stream = Stream
  { streamId :: !Word32,
    connection :: !Connection,
    inbox :: !(TBQueue Packet),
    nextMessageId :: !(IORef Word32),
    replies :: !(IORef (Map Word32 ByteString)),
    requests :: !(IORef [Packet]),
    closed :: !(IORef Bool)
  }

mkStream :: Connection -> Word32 -> TBQueue Packet -> IO Stream
mkStream connection streamId inbox = do
  nextMessageId <- newIORef 1
  replies <- newIORef Map.empty
  requests <- newIORef []
  closed <- newIORef False
  pure Stream {streamId, connection, inbox, nextMessageId, replies, requests, closed}

checkClosed :: Stream -> IO ()
checkClosed s = do
  c <- readIORef s.closed
  if c then fail "stream is closed" else pure ()

sendRequest :: Stream -> ByteString -> IO Word32
sendRequest s pay = do
  checkClosed s
  mid <- readIORef s.nextMessageId
  modifyIORef' s.nextMessageId (+ 1)
  sendPacket s.connection Packet {stream = s.streamId, messageId = mid, isReply = False, payload = pay}
  pure mid

writeReply :: Stream -> Word32 -> ByteString -> IO ()
writeReply s mid pay =
  sendPacket s.connection Packet {stream = s.streamId, messageId = mid, isReply = True, payload = pay}

receiveReply :: Stream -> Word32 -> IO ByteString
receiveReply s mid = do
  cached <- readIORef s.replies
  case Map.lookup mid cached of
    Just pay -> do
      modifyIORef' s.replies (Map.delete mid)
      pure pay
    Nothing -> do
      checkClosed s
      receiveOnePacket s
      receiveReply s mid

receiveRequest :: Stream -> IO (Word32, ByteString)
receiveRequest s = do
  pending <- readIORef s.requests
  case pending of
    (p : rest) -> do
      writeIORef s.requests rest
      pure (p.messageId, p.payload)
    [] -> do
      checkClosed s
      receiveOnePacket s
      receiveRequest s

receiveOnePacket :: Stream -> IO ()
receiveOnePacket s = do
  pkt <- atomically (readTBQueue s.inbox)
  if pkt.isReply
    then modifyIORef' s.replies (Map.insert pkt.messageId pkt.payload)
    else modifyIORef' s.requests (<> [pkt])

markClosed :: Stream -> IO ()
markClosed s = writeIORef s.closed True

closeStream :: Stream -> IO ()
closeStream s = do
  markClosed s
  unregisterStream s.connection s.streamId
  sendPacket
    s.connection
    Packet
      { stream = s.streamId,
        messageId = closeStreamMessageId,
        isReply = False,
        payload = closeStreamPayload
      }

-- | Encode a Value as CBOR, send as a request, await the reply,
-- decode it, and return the "result" field (or the whole map on success).
-- Raises an IO error if the reply contains an "error" field.
requestCbor :: Stream -> Value -> IO Value
requestCbor s msg = do
  let pay = CE.encode msg
  mid <- sendRequest s pay
  rep <- receiveReply s mid
  case CD.decode rep of
    Left err -> fail $ "requestCbor: CBOR decode: " <> err
    Right val -> case lookupKey "error" val of
      Just errVal -> do
        let errType = maybe "" id (lookupKey "type" errVal >>= asText)
        fail $ "Server error (" <> show errType <> "): " <> show errVal
      Nothing ->
        pure $ maybe val id (lookupKey "result" val)
