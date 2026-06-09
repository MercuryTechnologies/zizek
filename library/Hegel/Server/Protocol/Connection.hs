-- | Connection layer: a duplex pipe to the @hegel@ child process, multiplexed
-- across logical streams.
module Hegel.Server.Protocol.Connection
  ( -- * Connection
    Connection,
    newConnection,
    sendPacket,

    -- * Streams
    controlStream,
    newStream,
    connectStream,
    unregisterStream,

    -- * Exit signalling
    awaitServerExited,
    markServerExited,
    serverHasExited,
  )
where

import Control.Concurrent.Async (async, link)
import Control.Concurrent.STM (STM, TVar, atomically, check, newTVarIO, readTVar, writeTVar)
import Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO, writeTBQueue)
import Data.Bits (shiftL, (.|.))
import Data.Text qualified as T
import Data.Word (Word32)
import GHC.IO.Exception (IOErrorType (..))
import Hegel.Server.Protocol.Error (ConnectionClosedError (..))
import Hegel.Server.Protocol.Packet (Packet (..), readPacket, writePacket)
import Numeric.Natural (Natural)
import StmContainers.Map (Map)
import StmContainers.Map qualified as Map
import System.IO (Handle)
import System.IO.Error (ioeGetErrorType)
import UnliftIO.Exception (IOException, catch, throwIO, tryAny)
import UnliftIO.MVar (MVar, newMVar, withMVar)

streamInboxCapacity :: Natural
streamInboxCapacity = 128

data Connection = Connection
  { connWriter :: !(MVar Handle),
    connStreams :: !(Map Word32 (TBQueue Packet)),
    connNextId :: !(TVar Word32),
    connExited :: !(TVar Bool)
  }

newConnection :: Handle -> Handle -> IO Connection
newConnection rh wh = do
  writer <- newMVar wh
  streams <- Map.newIO
  nextId <- newTVarIO 1
  exited <- newTVarIO False
  let conn = Connection writer streams nextId exited
  a <- async (readerLoop conn rh)
  link a
  pure conn

readerLoop :: Connection -> Handle -> IO ()
readerLoop conn rh = go
  where
    go = do
      result <- tryAny (readPacket rh)
      case result of
        Left _ -> atomically do
          writeTVar conn.connExited True
          Map.reset conn.connStreams
        Right pkt -> do
          atomically do
            mq <- Map.lookup pkt.streamId conn.connStreams
            case mq of
              Just q -> writeTBQueue q pkt
              Nothing -> pure ()
          go

controlStream :: Connection -> IO (Word32, TBQueue Packet)
controlStream conn = (0,) <$> registerQueue conn 0

newStream :: Connection -> IO (Word32, TBQueue Packet)
newStream conn = do
  q <- newTBQueueIO streamInboxCapacity
  sid <- atomically do
    n <- readTVar conn.connNextId
    let sid = (n `shiftL` 1) .|. 1
    writeTVar conn.connNextId (n + 1)
    Map.insert q sid conn.connStreams
    pure sid
  pure (sid, q)

connectStream :: Connection -> Word32 -> IO (Word32, TBQueue Packet)
connectStream conn sid = (sid,) <$> registerQueue conn sid

registerQueue :: Connection -> Word32 -> IO (TBQueue Packet)
registerQueue conn sid = do
  q <- newTBQueueIO streamInboxCapacity
  atomically $ Map.insert q sid conn.connStreams
  pure q

unregisterStream :: Connection -> Word32 -> IO ()
unregisterStream conn sid = atomically $ Map.delete sid conn.connStreams

awaitServerExited :: Connection -> STM ()
awaitServerExited conn = readTVar conn.connExited >>= check

markServerExited :: Connection -> IO ()
markServerExited conn = atomically $ writeTVar conn.connExited True

serverHasExited :: Connection -> IO Bool
serverHasExited conn = atomically $ readTVar conn.connExited

sendPacket :: Connection -> Packet -> IO ()
sendPacket conn pkt =
  withMVar conn.connWriter \h ->
    writePacket h pkt
      `catch` \(e :: IOException) -> case ioeGetErrorType e of
        IllegalOperation -> throwIO (ConnectionClosedError (T.pack (show e)))
        ResourceVanished -> throwIO (ConnectionClosedError (T.pack (show e)))
        _ -> throwIO e
