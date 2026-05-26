module Hegel.Protocol.Stream
  ( Stream,
    mkStream,
    streamId,
    sendRequest,
    requestRaw,
    requestCbor,
    writeReply,
    receiveRequest,
    closeStream,
  )
where

import CBOR.Decode qualified as CD
import CBOR.Encode qualified as CE
import CBOR.Value (Value (..))
import Control.Concurrent.STM (atomically, orElse)
import Control.Concurrent.STM.TBQueue (TBQueue, readTBQueue)
import Data.Bits (shiftL)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Word (Word32)
import Hegel.Protocol.Cbor (asText, lookupKey)
import Hegel.Protocol.Connection (Connection, awaitServerExited, sendPacket, unregisterStream)
import Hegel.Protocol.Error (ConnectionClosedError (..), ProtocolError (..), ServerError (..))
import Hegel.Protocol.Packet (Packet (..))
import UnliftIO.Exception (finally, throwIO)
import UnliftIO.MVar (MVar, modifyMVar, modifyMVar_, newMVar)

closeStreamPayload :: ByteString
closeStreamPayload = BS.singleton 0xFE

closeStreamMessageId :: Word32
closeStreamMessageId = (1 `shiftL` 31) - 1

data StreamState = StreamState
  { nextMessageId :: !Word32,
    replies :: !(Map Word32 ByteString),
    requests :: !(Seq Packet),
    closed :: !Bool
  }

data Stream = Stream
  { streamId :: !Word32,
    connection :: !Connection,
    inbox :: !(TBQueue Packet),
    state :: !(MVar StreamState)
  }

mkStream :: Connection -> Word32 -> TBQueue Packet -> IO Stream
mkStream connection streamId inbox = do
  state <-
    newMVar
      StreamState
        { nextMessageId = 1,
          replies = Map.empty,
          requests = Seq.empty,
          closed = False
        }
  pure Stream {streamId, connection, inbox, state}

-- | Fire-and-forget send: enqueue the payload on the stream without waiting
-- for a reply. No-ops silently when the stream is already closed.
sendRequest :: Stream -> ByteString -> IO ()
sendRequest s pay = modifyMVar_ s.state \st ->
  if st.closed
    then pure st
    else do
      let mid = st.nextMessageId
      sendPacket
        s.connection
        Packet {stream = s.streamId, messageId = mid, isReply = False, payload = pay}
      pure st {nextMessageId = mid + 1}

-- | Send a raw request and block until the matching reply arrives.
-- Holds the stream's state lock for the entire duration; streams are
-- single-threaded by design.
requestRaw :: Stream -> ByteString -> IO ByteString
requestRaw s pay = modifyMVar s.state \st ->
  if st.closed
    then throwIO StreamClosed
    else do
      let mid = st.nextMessageId
      sendPacket s.connection Packet {stream = s.streamId, messageId = mid, isReply = False, payload = pay}
      go mid st
  where
    go mid st = case Map.lookup mid st.replies of
      Just rep ->
        pure (st {replies = Map.delete mid st.replies, nextMessageId = mid + 1}, rep)
      Nothing -> do
        mPkt <-
          atomically $
            (Just <$> readTBQueue s.inbox)
              `orElse` (Nothing <$ awaitServerExited s.connection)
        case mPkt of
          Nothing -> throwIO (ConnectionClosedError "stream inbox abandoned")
          Just pkt -> do
            let st' =
                  if pkt.isReply
                    then st {replies = Map.insert pkt.messageId pkt.payload st.replies}
                    else st {requests = st.requests Seq.|> pkt}
            go mid st'

-- Error envelope shape: {"error": <payload>, "type": "<tag>"} — "type" is at
-- the top level of the response, not nested inside the "error" value.
requestCbor :: Stream -> Value -> IO Value
requestCbor s msg = do
  let pay = CE.encode msg
  rep <- requestRaw s pay
  case CD.decode rep of
    Left err -> throwIO (CborDecodeFailure "requestCbor" err)
    Right val -> case lookupKey "error" val of
      Just errVal -> do
        let errType = maybe "" id (lookupKey "type" val >>= asText)
        throwIO ServerError {errorType = errType, errorPayload = errVal}
      Nothing ->
        pure $ maybe val id (lookupKey "result" val)

writeReply :: Stream -> Word32 -> ByteString -> IO ()
writeReply s mid pay =
  sendPacket s.connection Packet {stream = s.streamId, messageId = mid, isReply = True, payload = pay}

receiveRequest :: Stream -> IO (Word32, ByteString)
receiveRequest s = modifyMVar s.state \st ->
  if st.closed
    then throwIO StreamClosed
    else go st
  where
    go st = case Seq.viewl st.requests of
      p Seq.:< rest -> pure (st {requests = rest}, (p.messageId, p.payload))
      Seq.EmptyL -> do
        mPkt <-
          atomically $
            (Just <$> readTBQueue s.inbox)
              `orElse` (Nothing <$ awaitServerExited s.connection)
        case mPkt of
          Nothing -> throwIO (ConnectionClosedError "stream inbox abandoned")
          Just pkt -> do
            let st' =
                  if pkt.isReply
                    then st {replies = Map.insert pkt.messageId pkt.payload st.replies}
                    else st {requests = st.requests Seq.|> pkt}
            go st'

-- | Idempotent: a second call after the first succeeds is a no-op.
-- Uses 'finally' rather than 'uninterruptibleMask_' because 'sendPacket'
-- can block indefinitely if the child stops draining stdin.
closeStream :: Stream -> IO ()
closeStream s = do
  alreadyClosed <- modifyMVar s.state \st ->
    pure (st {closed = True}, st.closed)
  if alreadyClosed
    then pure ()
    else
      sendPacket
        s.connection
        Packet
          { stream = s.streamId,
            messageId = closeStreamMessageId,
            isReply = False,
            payload = closeStreamPayload
          }
        `finally` unregisterStream s.connection s.streamId
