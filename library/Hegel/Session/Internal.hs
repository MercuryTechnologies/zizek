module Hegel.Session.Internal
  ( Session (..),
    SessionConfig (..),
    LiveSession (..),
    defaultSessionConfig,
    openSession,
    withSession,
    closeSession,
    invalidateSession,
    getOrInitLiveSession,
    liveProcess,
    globalSession,
  )
where

import Control.Concurrent.Async (Async, async, cancel, link, waitCatch)
import Data.ByteString.Char8 qualified as BS8
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Text qualified as T
import Hegel.Protocol.Connection
  ( Connection,
    controlStream,
    markServerExited,
    newConnection,
    serverHasExited,
  )
import Hegel.Protocol.Error (ProtocolError (..))
import Hegel.Protocol.Stream (Stream, mkStream, requestRaw)
import System.IO (Handle, hSetBinaryMode)
import System.IO.Unsafe (unsafePerformIO)
import System.Process.Typed
import Text.Read (readMaybe)
import UnliftIO.Exception (bracket, bracketOnError, throwIO)
import UnliftIO.MVar (MVar, modifyMVar, modifyMVar_, newMVar)

handshakeString :: BS8.ByteString
handshakeString = "hegel_handshake_start"

supportedProtocolLo :: (Int, Int)
supportedProtocolLo = (0, 15)

supportedProtocolHi :: (Int, Int)
supportedProtocolHi = (0, 15)

data SessionConfig = SessionConfig
  { command :: !FilePath,
    arguments :: ![String],
    environment :: !(Maybe [(String, String)])
  }

defaultSessionConfig :: SessionConfig
defaultSessionConfig =
  SessionConfig
    { command = "hegel",
      arguments = ["--verbosity", "normal"],
      environment = Nothing
    }

data LiveSession = LiveSession
  { conn :: !Connection,
    control :: !Stream,
    process :: !(Process Handle Handle ())
  }

data Session = Session
  { config :: !SessionConfig,
    slot :: !(MVar (Maybe (Async LiveSession)))
  }

globalSession :: Session
globalSession = Session defaultSessionConfig (unsafePerformIO (newMVar Nothing))
{-# NOINLINE globalSession #-}

openSession :: SessionConfig -> IO Session
openSession cfg = do
  s <- Session cfg <$> newMVar Nothing
  _ <- getOrInitLiveSession s
  pure s

withSession :: SessionConfig -> (Session -> IO r) -> IO r
withSession cfg = bracket (openSession cfg) closeSession

closeSession :: Session -> IO ()
closeSession ses = do
  mAsync <- modifyMVar ses.slot \m -> pure (Nothing, m)
  for_ mAsync \a ->
    waitCatch a >>= \case
      Left _ -> pure ()
      Right live -> stopProcess live.process

invalidateSession :: Session -> IO ()
invalidateSession ses = do
  mAsync <- modifyMVar ses.slot \m -> pure (Nothing, m)
  for_ mAsync \a -> do
    cancel a
    waitCatch a >>= \case
      Right live -> stopProcess live.process
      Left _ -> pure ()

getOrInitLiveSession :: Session -> IO LiveSession
getOrInitLiveSession ses = do
  a <- modifyMVar ses.slot \mAsync ->
    case mAsync of
      Nothing -> do
        a <- async (initLiveSession ses.config)
        pure (Just a, a)
      Just a -> pure (Just a, a)
  result <- waitCatch a
  case result of
    Right live -> do
      exited <- serverHasExited live.conn
      if not exited
        then pure live
        else do
          modifyMVar_ ses.slot \mAsync ->
            case mAsync of
              Just a' | a' == a -> pure Nothing
              other -> pure other
          getOrInitLiveSession ses
    Left e -> do
      modifyMVar_ ses.slot \mAsync ->
        case mAsync of
          Just a' | a' == a -> pure Nothing
          other -> pure other
      throwIO e

liveProcess :: Session -> IO (Process Handle Handle ())
liveProcess ses = (.process) <$> getOrInitLiveSession ses

initLiveSession :: SessionConfig -> IO LiveSession
initLiveSession cfg = do
  let pcfg =
        proc cfg.command cfg.arguments
          & setStdin createPipe
          & setStdout createPipe
          -- TODO: once .hegel/ local state lands with database support,
          -- redirect to .hegel/server.<pid>-<n>.log instead of nullStream
          -- (matches refs/hegel-rust.md:26428-26439; enables quoting the
          -- path in handshake-failure diagnostics per refs/26502-26515).
          & setStderr nullStream
  let pcfg' = maybe pcfg (\env -> setEnv env pcfg) cfg.environment
  bracketOnError (startProcess pcfg') stopProcess \p -> do
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
      Nothing -> throwIO (HandshakeFailure (T.pack $ "Bad handshake response: " <> show decoded))
      Just v -> pure v
    parsed <- parseVersion ver
    if parsed < supportedProtocolLo || parsed > supportedProtocolHi
      then
        throwIO
          ( VersionMismatch
              (T.pack ver)
              (T.pack (showVer supportedProtocolLo))
              (T.pack (showVer supportedProtocolHi))
          )
      else pure ()
    a <- async (monitorProcess conn p)
    link a
    pure LiveSession {conn = conn, control = ctrl, process = p}

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
      _ -> throwIO (HandshakeFailure (T.pack $ "Invalid version string: " <> s))
    _ -> throwIO (HandshakeFailure (T.pack $ "Invalid version string: " <> s))

showVer :: (Int, Int) -> String
showVer (maj, mn) = show maj <> "." <> show mn

monitorProcess :: Connection -> Process Handle Handle () -> IO ()
monitorProcess conn p = waitExitCode p *> markServerExited conn
