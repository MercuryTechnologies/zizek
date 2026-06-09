-- | @hegel@ session lifecycle management.
module Hegel.Server.Session.Internal
  ( -- * Sessions
    Session (..),
    LiveSession (..),
    globalSession,
    openSession,
    withSession,
    closeSession,
    invalidateSession,
    liveSession,
    liveProcess,

    -- * Configuration
    SessionConfig (..),
    defaultSessionConfig,
  )
where

import Control.Concurrent.Async (Async, async, cancel, link, waitCatch)
import Data.Foldable (for_)
import Data.Function ((&))
import Hegel.Server.Client (Client (..), newClient)
import Hegel.Server.Protocol.Connection (Connection, markServerExited, newConnection, serverHasExited)
import System.IO (Handle, hSetBinaryMode)
import System.IO.Unsafe (unsafePerformIO)
import System.Process.Typed
import UnliftIO.Exception (bracket, bracketOnError, throwIO)
import UnliftIO.MVar (MVar, modifyMVar, modifyMVar_, newMVar)

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
  { client :: !Client,
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
  _ <- liveSession s
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

liveSession :: Session -> IO LiveSession
liveSession ses = do
  a <- modifyMVar ses.slot \mAsync ->
    case mAsync of
      Nothing -> do
        a <- async (initLiveSession ses.config)
        pure (Just a, a)
      Just a -> pure (Just a, a)
  result <- waitCatch a
  case result of
    Right live -> do
      exited <- serverHasExited live.client.connection
      if not exited
        then pure live
        else do
          modifyMVar_ ses.slot \mAsync ->
            case mAsync of
              Just a' | a' == a -> pure Nothing
              other -> pure other
          liveSession ses
    Left e -> do
      modifyMVar_ ses.slot \mAsync ->
        case mAsync of
          Just a' | a' == a -> pure Nothing
          other -> pure other
      throwIO e

liveProcess :: Session -> IO (Process Handle Handle ())
liveProcess ses = (.process) <$> liveSession ses

initLiveSession :: SessionConfig -> IO LiveSession
initLiveSession cfg = do
  let pcfg =
        proc cfg.command cfg.arguments
          & setStdin createPipe
          & setStdout createPipe
          & setStderr nullStream
      pcfg' = maybe pcfg (\env -> setEnv env pcfg) cfg.environment
  bracketOnError (startProcess pcfg') stopProcess \p -> do
    let rh = getStdout p
        wh = getStdin p
    hSetBinaryMode rh True
    hSetBinaryMode wh True
    conn <- newConnection rh wh
    client <- newClient conn
    a <- async (monitorProcess conn p)
    link a
    pure LiveSession {client, process = p}

monitorProcess :: Connection -> Process Handle Handle () -> IO ()
monitorProcess conn p = waitExitCode p *> markServerExited conn
