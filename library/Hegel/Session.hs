module Hegel.Session
  ( Session (..),
    getOrInitSession,
  )
where

import Control.Concurrent.Async (Async, async, link, waitCatch)
import Data.ByteString.Char8 qualified as BS8
import Data.Function ((&))
import Hegel.Protocol.Connection
  ( Connection,
    controlStream,
    markServerExited,
    newConnection,
    serverHasExited,
  )
import Hegel.Protocol.Stream (Stream, mkStream, requestRaw)
import System.IO (Handle, hSetBinaryMode)
import System.IO.Unsafe (unsafePerformIO)
import System.Process.Typed
import Text.Read (readMaybe)
import UnliftIO.Exception (bracketOnError, throwIO)
import UnliftIO.MVar (MVar, modifyMVar, modifyMVar_, newMVar)

handshakeString :: BS8.ByteString
handshakeString = "hegel_handshake_start"

supportedProtocolLo :: (Int, Int)
supportedProtocolLo = (0, 15)

supportedProtocolHi :: (Int, Int)
supportedProtocolHi = (0, 15)

data Session = Session
  { conn :: !Connection,
    control :: !Stream,
    process :: !(Process Handle Handle ())
  }

-- | 'Nothing' = never started; 'Just a' = initializing or done.
-- Callers install an 'Async' under the lock and then 'waitCatch' outside it,
-- so slow or stuck initialization does not block the MVar for other callers.
globalSession :: MVar (Maybe (Async Session))
globalSession = unsafePerformIO (newMVar Nothing)
{-# NOINLINE globalSession #-}

getOrInitSession :: IO Session
getOrInitSession = do
  a <- modifyMVar globalSession \mAsync ->
    case mAsync of
      Nothing -> do
        a <- async initSession
        pure (Just a, a)
      Just a -> pure (Just a, a)
  result <- waitCatch a
  case result of
    Right ses -> do
      exited <- serverHasExited ses.conn
      if not exited
        then pure ses
        else do
          modifyMVar_ globalSession \mAsync ->
            case mAsync of
              Just a' | a' == a -> pure Nothing
              other -> pure other
          getOrInitSession
    Left e -> do
      modifyMVar_ globalSession \mAsync ->
        case mAsync of
          Just a' | a' == a -> pure Nothing
          other -> pure other
      throwIO e

initSession :: IO Session
initSession = do
  let cfg =
        proc "hegel" ["--verbosity", "normal"]
          & setStdin createPipe
          & setStdout createPipe
          & setStderr inherit
  bracketOnError (startProcess cfg) stopProcess \p -> do
    let rh = getStdout p
        wh = getStdin p
    hSetBinaryMode rh True
    hSetBinaryMode wh True
    conn <- newConnection rh wh
    (sid, q) <- controlStream conn
    ctrl <- mkStream conn sid q
    rep <- requestRaw ctrl handshakeString
    let decoded = BS8.unpack rep
    ver <- case dropPrefix "Hegel/" decoded of
      Nothing -> fail $ "Bad handshake response: " <> show decoded
      Just v -> pure v
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
    -- link propagates unexpected crashes from the monitor async during
    -- the initSession call; it is a no-op once initSession returns since
    -- the thread that called link no longer exists
    a <- async (monitorProcess conn p)
    link a
    pure Session {conn = conn, control = ctrl, process = p}

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
