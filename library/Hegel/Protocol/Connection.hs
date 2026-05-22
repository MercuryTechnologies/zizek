module Hegel.Protocol.Connection
  ( Connection
  , newConnection
  , controlStream
  , newStream
  , connectStream
  , unregisterStream
  , markServerExited
  , serverHasExited
  , sendPacket
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO, writeTBQueue)
import Control.Exception (SomeException, try)
import Data.Bits (shiftL, (.|.))
import Data.Word (Word32)
import Hegel.Protocol.Packet (Packet (..), readPacket, writePacket)
import StmContainers.Map (Map)
import StmContainers.Map qualified as Map
import System.IO (Handle)
import UnliftIO.MVar (MVar, newMVar, withMVar)

data Connection = Connection
  { connWriter  :: !(MVar Handle)
  , connStreams  :: !(Map Word32 (TBQueue Packet))
  , connNextId  :: !(TVar Word32)
  , connExited  :: !(TVar Bool)
  }

newConnection :: Handle -> Handle -> IO Connection
newConnection rh wh = do
  writer  <- newMVar wh
  streams <- Map.newIO
  nextId  <- newTVarIO 1
  exited  <- newTVarIO False
  let conn = Connection writer streams nextId exited
  _ <- forkIO (readerLoop conn rh)
  pure conn

readerLoop :: Connection -> Handle -> IO ()
readerLoop conn rh = go
  where
    go = do
      result <- try (readPacket rh) :: IO (Either SomeException Packet)
      case result of
        Left _ -> atomically do
          writeTVar conn.connExited True
          Map.reset conn.connStreams
        Right pkt -> do
          atomically do
            mq <- Map.lookup pkt.stream conn.connStreams
            case mq of
              Just q  -> writeTBQueue q pkt
              Nothing -> pure ()
          go

controlStream :: Connection -> IO (Word32, TBQueue Packet)
controlStream conn = (0,) <$> registerQueue conn 0

newStream :: Connection -> IO (Word32, TBQueue Packet)
newStream conn = do
  q <- newTBQueueIO 128
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
  q <- newTBQueueIO 128
  atomically $ Map.insert q sid conn.connStreams
  pure q

unregisterStream :: Connection -> Word32 -> IO ()
unregisterStream conn sid =
  atomically $ Map.delete sid conn.connStreams

markServerExited :: Connection -> IO ()
markServerExited conn = atomically $ writeTVar conn.connExited True

serverHasExited :: Connection -> IO Bool
serverHasExited conn = atomically $ readTVar conn.connExited

sendPacket :: Connection -> Packet -> IO ()
sendPacket conn pkt = withMVar conn.connWriter \h -> writePacket h pkt
