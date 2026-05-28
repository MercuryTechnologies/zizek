-- | A single logical stream multiplexed over a 'Connection'. Handles
-- request/reply correlation, server-initiated request delivery, and stream
-- close.
module Hegel.Protocol.Stream
  ( -- * Stream
    Stream,
    mkStream,
    closeStream,

    -- * Client-initiated requests
    sendRequest,
    request,
    await,
    requestRaw,
    PendingRequest,

    -- * CBOR requests
    requestCbor,
    requestCborPending,
    awaitCbor,

    -- * Server-initiated requests
    receiveRequest,
    writeReply,
  )
where

import CBOR.Decode qualified as CD
import CBOR.Encode qualified as CE
import CBOR.Value (Value (..))
import Control.Concurrent.STM
  ( TVar,
    atomically,
    modifyTVar',
    newTVarIO,
    orElse,
    readTVar,
    readTVarIO,
    writeTVar,
  )
import Control.Concurrent.STM.TBQueue (TBQueue, readTBQueue)
import Data.Bits (shiftL)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
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

closeStreamPayload :: ByteString
closeStreamPayload = BS.singleton 0xFE

closeStreamMessageId :: Word32
closeStreamMessageId = (1 `shiftL` 31) - 1

-- | One logical stream's state: its ID, the underlying connection, the
-- inbox the connection's reader writes into, an ID counter for outgoing
-- requests, buffers for unmatched replies and server-initiated requests,
-- and a closed flag.
data Stream = Stream
  { streamId :: !Word32,
    connection :: !Connection,
    inbox :: !(TBQueue Packet),
    nextMessageId :: !(TVar Word32),
    replies :: !(TVar (Map Word32 ByteString)),
    requests :: !(TVar (Seq Packet)),
    closed :: !(TVar Bool)
  }

-- | A handle to an in-flight request. Call 'await' to block until the reply
-- arrives. The result is cached so 'await' is safe to call multiple times.
data PendingRequest = PendingRequest
  { prStream :: !Stream,
    prMessageId :: !Word32,
    prCached :: !(IORef (Maybe ByteString))
  }

-- | Wrap a connection-allocated stream ID and inbox in a fresh 'Stream'.
mkStream :: Connection -> Word32 -> TBQueue Packet -> IO Stream
mkStream connection streamId inbox = do
  nextMessageId <- newTVarIO 1
  replies <- newTVarIO Map.empty
  requests <- newTVarIO Seq.empty
  closed <- newTVarIO False
  pure Stream {streamId, connection, inbox, nextMessageId, replies, requests, closed}

-- | Fire-and-forget send. Silently no-ops when the stream is closed.
sendRequest :: Stream -> ByteString -> IO ()
sendRequest s payload = do
  isClosed <- readTVarIO s.closed
  if isClosed
    then pure ()
    else do
      messageId <- atomically $ do
        mid <- readTVar s.nextMessageId
        writeTVar s.nextMessageId (mid + 1)
        pure mid
      sendPacket
        s.connection
        Packet {streamId = s.streamId, messageId, isReply = False, payload}

-- | Send a request and return a handle to the pending reply without blocking.
request :: Stream -> ByteString -> IO PendingRequest
request s payload = do
  isClosed <- readTVarIO s.closed
  if isClosed
    then throwIO StreamClosed
    else do
      messageId <- atomically $ do
        mid <- readTVar s.nextMessageId
        writeTVar s.nextMessageId (mid + 1)
        pure mid
      sendPacket
        s.connection
        Packet {streamId = s.streamId, messageId, isReply = False, payload}
      ref <- newIORef Nothing
      pure PendingRequest {prStream = s, prMessageId = messageId, prCached = ref}

-- | Block until the reply to a pending request arrives. Caches the result;
-- idempotent.
await :: PendingRequest -> IO ByteString
await pr = do
  mCached <- readIORef pr.prCached
  case mCached of
    Just bs -> pure bs
    Nothing -> do
      bs <- drainUntilReply pr.prStream pr.prMessageId
      writeIORef pr.prCached (Just bs)
      pure bs

-- Pump packets from the inbox, routing each to the replies or requests TVar,
-- until the reply for the given message ID arrives.
--
-- The check of s.replies and the inbox read are one atomic transaction so that
-- when another thread stores a buffered reply, STM retries all waiters. Two
-- separate transactions would let a thread get stuck blocking on the inbox
-- after another thread already stored the reply it needed in s.replies.
drainUntilReply :: Stream -> Word32 -> IO ByteString
drainUntilReply s mid = loop
  where
    loop = do
      r <- atomically $ do
        m <- readTVar s.replies
        case Map.lookup mid m of
          Just bs -> writeTVar s.replies (Map.delete mid m) >> pure (Right (Just bs))
          Nothing -> do
            mPkt <-
              (Just <$> readTBQueue s.inbox)
                `orElse` (Nothing <$ awaitServerExited s.connection)
            case mPkt of
              Nothing -> pure (Left ())
              Just pkt -> do
                if pkt.isReply
                  then modifyTVar' s.replies (Map.insert pkt.messageId pkt.payload)
                  else modifyTVar' s.requests (Seq.|> pkt)
                pure (Right Nothing)
      case r of
        Left () -> throwIO (ConnectionClosedError "stream inbox abandoned")
        Right Nothing -> loop
        Right (Just bs) -> pure bs

-- | Send a raw request and block until the matching reply arrives.
requestRaw :: Stream -> ByteString -> IO ByteString
requestRaw s payload = request s payload >>= await

-- | Send a CBOR-encoded request and return a 'PendingRequest' handle.
requestCborPending :: Stream -> Value -> IO PendingRequest
requestCborPending s msg = request s (CE.encode msg)

-- | Block until the reply to a CBOR pending request arrives; unwrap the
-- result envelope or throw 'ServerError'.
awaitCbor :: PendingRequest -> IO Value
awaitCbor pr = do
  rep <- await pr
  case CD.decode rep of
    Left err -> throwIO (CborDecodeFailure "awaitCbor" err)
    Right val -> case lookupKey "error" val of
      Just errVal -> do
        let errType = maybe "" id (lookupKey "type" val >>= asText)
        throwIO ServerError {errorType = errType, errorPayload = errVal}
      Nothing -> pure $ maybe val id (lookupKey "result" val)

-- | Send a CBOR-encoded request and block until the decoded reply arrives.
requestCbor :: Stream -> Value -> IO Value
requestCbor s msg = requestCborPending s msg >>= awaitCbor

-- | Reply to a server-initiated request with the given message ID and
-- payload.
writeReply :: Stream -> Word32 -> ByteString -> IO ()
writeReply s messageId payload =
  sendPacket
    s.connection
    Packet {streamId = s.streamId, messageId, isReply = True, payload}

-- | Receive the next server-initiated request on this stream.
receiveRequest :: Stream -> IO (Word32, ByteString)
receiveRequest s = do
  mPkt <- atomically $ do
    reqs <- readTVar s.requests
    case Seq.viewl reqs of
      p Seq.:< rest -> writeTVar s.requests rest >> pure (Just (p.messageId, p.payload))
      Seq.EmptyL -> pure Nothing
  case mPkt of
    Just r -> pure r
    Nothing -> do
      mPkt' <-
        atomically $
          (Just <$> readTBQueue s.inbox)
            `orElse` (Nothing <$ awaitServerExited s.connection)
      case mPkt' of
        Nothing -> throwIO (ConnectionClosedError "stream inbox abandoned")
        Just pkt -> do
          atomically $
            if pkt.isReply
              then modifyTVar' s.replies (Map.insert pkt.messageId pkt.payload)
              else modifyTVar' s.requests (Seq.|> pkt)
          receiveRequest s

-- | Close the stream. Idempotent.
closeStream :: Stream -> IO ()
closeStream s = do
  alreadyClosed <- atomically $ do
    c <- readTVar s.closed
    if c then pure True else writeTVar s.closed True >> pure False
  if alreadyClosed
    then pure ()
    else
      sendPacket
        s.connection
        Packet
          { streamId = s.streamId,
            messageId = closeStreamMessageId,
            isReply = False,
            payload = closeStreamPayload
          }
        `finally` unregisterStream s.connection s.streamId
