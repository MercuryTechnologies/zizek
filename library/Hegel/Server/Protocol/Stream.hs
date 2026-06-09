-- | A logical stream multiplexed over a 'Connection'.
module Hegel.Server.Protocol.Stream
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
import Hegel.Server.Protocol.Connection (Connection, awaitServerExited, sendPacket, unregisterStream)
import Hegel.Server.Protocol.Error (ConnectionClosedError (..), ProtocolError (..), ServerError (..))
import Hegel.Server.Protocol.Packet (Packet (..))
import UnliftIO.Exception (finally, throwIO)

closeStreamPayload :: ByteString
closeStreamPayload = BS.singleton 0xFE

closeStreamMessageId :: Word32
closeStreamMessageId = (1 `shiftL` 31) - 1

data Stream = Stream
  { streamId :: !Word32,
    connection :: !Connection,
    inbox :: !(TBQueue Packet),
    nextMessageId :: !(TVar Word32),
    replies :: !(TVar (Map Word32 ByteString)),
    requests :: !(TVar (Seq Packet)),
    closed :: !(TVar Bool)
  }

data PendingRequest = PendingRequest
  { prStream :: !Stream,
    prMessageId :: !Word32,
    prCached :: !(IORef (Maybe ByteString))
  }

mkStream :: Connection -> Word32 -> TBQueue Packet -> IO Stream
mkStream connection streamId inbox = do
  nextMessageId <- newTVarIO 1
  replies <- newTVarIO Map.empty
  requests <- newTVarIO Seq.empty
  closed <- newTVarIO False
  pure Stream {streamId, connection, inbox, nextMessageId, replies, requests, closed}

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
      sendPacket s.connection Packet {streamId = s.streamId, messageId, isReply = False, payload}

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
      sendPacket s.connection Packet {streamId = s.streamId, messageId, isReply = False, payload}
      ref <- newIORef Nothing
      pure PendingRequest {prStream = s, prMessageId = messageId, prCached = ref}

await :: PendingRequest -> IO ByteString
await pr = do
  mCached <- readIORef pr.prCached
  case mCached of
    Just bs -> pure bs
    Nothing -> do
      bs <- drainUntilReply pr.prStream pr.prMessageId
      writeIORef pr.prCached (Just bs)
      pure bs

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

requestRaw :: Stream -> ByteString -> IO ByteString
requestRaw s payload = request s payload >>= await

requestCborPending :: Stream -> Value -> IO PendingRequest
requestCborPending s msg = request s (CE.encode msg)

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

requestCbor :: Stream -> Value -> IO Value
requestCbor s msg = requestCborPending s msg >>= awaitCbor

writeReply :: Stream -> Word32 -> ByteString -> IO ()
writeReply s messageId payload =
  sendPacket s.connection Packet {streamId = s.streamId, messageId, isReply = True, payload}

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
