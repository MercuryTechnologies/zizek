module Hegel.Session
  ( Session (..),
    getOrInitSession,
  )
where

import Control.Concurrent.Async (async, link)
import Data.ByteString.Char8 qualified as BS8
import Data.Function ((&))
import Hegel.Protocol.Connection
  ( Connection,
    controlStream,
    markServerExited,
    newConnection,
    serverHasExited,
  )
import Hegel.Protocol.Stream (Stream, mkStream, receiveReply, sendRequest)
import System.IO (Handle, hSetBinaryMode)
import System.IO.Unsafe (unsafePerformIO)
import System.Process.Typed
import Text.Read (readMaybe)
import UnliftIO.MVar (MVar, modifyMVar, newMVar)

handshakeString :: BS8.ByteString
handshakeString = "hegel_handshake_start"

supportedProtocolLo :: (Int, Int)
supportedProtocolLo = (0, 15)

supportedProtocolHi :: (Int, Int)
supportedProtocolHi = (0, 15)

data Session = Session
  { conn :: !Connection,
    control :: !(MVar Stream),
    process :: !(Process Handle Handle ())
  }

globalSession :: MVar (Maybe Session)
globalSession = unsafePerformIO (newMVar Nothing)
{-# NOINLINE globalSession #-}

getOrInitSession :: IO Session
getOrInitSession =
  modifyMVar globalSession \mses -> do
    case mses of
      Just ses -> do
        exited <- serverHasExited ses.conn
        if exited
          then do
            ses' <- initSession
            pure (Just ses', ses')
          else pure (Just ses, ses)
      Nothing -> do
        ses <- initSession
        pure (Just ses, ses)

initSession :: IO Session
initSession = do
  let cfg =
        proc "hegel" ["--verbosity", "normal"]
          & setStdin createPipe
          & setStdout createPipe
          & setStderr inherit
  p <- startProcess cfg
  let rh = getStdout p
      wh = getStdin p
  hSetBinaryMode rh True
  hSetBinaryMode wh True
  conn <- newConnection rh wh
  (sid, q) <- controlStream conn
  ctrl <- mkStream conn sid q
  mid <- sendRequest ctrl handshakeString
  rep <- receiveReply ctrl mid
  let decoded = BS8.unpack rep
  ver <- case dropPrefix "Hegel/" decoded of
    Nothing -> fail $ "Bad handshake response: " <> show decoded
    Just ver -> pure ver
  parsed <- parseVersion ver
  if parsed < supportedProtocolLo || parsed > supportedProtocolHi
    then
      fail $
        "Protocol version mismatch: server reported "
          <> ver
          <> ", supported "
          <> showVer supportedProtocolLo
          <> " to "
          <> showVer supportedProtocolHi
    else pure ()
  ctrlMVar <- newMVar ctrl
  a <- async (monitorProcess conn p)
  link a
  pure Session {conn = conn, control = ctrlMVar, process = p}

dropPrefix :: String -> String -> Maybe String
dropPrefix [] ys = Just ys
dropPrefix (x : xs) (y : ys)
  | x == y = dropPrefix xs ys
  | otherwise = Nothing
dropPrefix _ [] = Nothing

parseVersion :: String -> IO (Int, Int)
parseVersion s =
  case break (== '.') s of
    (maj, '.' : minS) -> case (readMaybe maj, readMaybe minS) of
      (Just a, Just b) -> pure (a, b)
      _ -> fail $ "Invalid version string: " <> s
    _ -> fail $ "Invalid version string: " <> s

showVer :: (Int, Int) -> String
showVer (maj, mn) = show maj <> "." <> show mn

monitorProcess :: Connection -> Process Handle Handle () -> IO ()
monitorProcess conn p = waitExitCode p *> markServerExited conn
