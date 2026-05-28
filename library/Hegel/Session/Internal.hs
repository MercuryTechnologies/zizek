-- | @hegel@ session lifecycle management.
--
-- 'Hegel.Session' re-exports the parts users normally need; this module
-- additionally exposes 'LiveSession', 'liveSession', and 'liveProcess' for
-- callers that need direct access to the underlying 'Client' or 'Process'.
module Hegel.Session.Internal
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
import Hegel.Client (Client (..), newClient)
import Hegel.Protocol.Connection (Connection, markServerExited, newConnection, serverHasExited)
import System.IO (Handle, hSetBinaryMode)
import System.IO.Unsafe (unsafePerformIO)
import System.Process.Typed
import UnliftIO.Exception (bracket, bracketOnError, throwIO)
import UnliftIO.MVar (MVar, modifyMVar, modifyMVar_, newMVar)

-- | How to spawn the @hegel@ child process.
data SessionConfig = SessionConfig
  { -- | Binary to execute. Resolved against @PATH@ if unqualified.
    command :: !FilePath,
    -- | Command-line arguments passed to the binary.
    arguments :: ![String],
    -- | Process environment. 'Nothing' inherits the parent's environment.
    environment :: !(Maybe [(String, String)])
  }

-- | Invoke @hegel@ from @PATH@ with @--verbosity normal@ and an inherited
-- environment.
defaultSessionConfig :: SessionConfig
defaultSessionConfig =
  SessionConfig
    { command = "hegel",
      arguments = ["--verbosity", "normal"],
      environment = Nothing
    }

-- | A running @hegel@ child process and the 'Client' connected to it.
data LiveSession = LiveSession
  { client :: !Client,
    process :: !(Process Handle Handle ())
  }

-- | A 'LiveSession' that's spawned on first use and reused thereafter.
data Session = Session
  { config :: !SessionConfig,
    slot :: !(MVar (Maybe (Async LiveSession)))
  }

-- | Process-wide shared 'Session' using 'defaultSessionConfig'.
globalSession :: Session
globalSession = Session defaultSessionConfig (unsafePerformIO (newMVar Nothing))
{-# NOINLINE globalSession #-}

-- | Create a 'Session' and eagerly spawn the child process.
openSession :: SessionConfig -> IO Session
openSession cfg = do
  s <- Session cfg <$> newMVar Nothing
  _ <- liveSession s
  pure s

-- | Bracketed 'openSession': the child process is stopped on exit.
withSession :: SessionConfig -> (Session -> IO r) -> IO r
withSession cfg = bracket (openSession cfg) closeSession

-- | Stop the child process; the 'Session' will spawn a new 'LiveSession'
-- the next time it's used.
closeSession :: Session -> IO ()
closeSession ses = do
  mAsync <- modifyMVar ses.slot \m -> pure (Nothing, m)
  for_ mAsync \a ->
    waitCatch a >>= \case
      Left _ -> pure ()
      Right live -> stopProcess live.process

-- | Cancel any in-flight initialization and forcibly stop the process.
--
-- Used after a connection or protocol error suggests the server is no
-- longer healthy.
invalidateSession :: Session -> IO ()
invalidateSession ses = do
  mAsync <- modifyMVar ses.slot \m -> pure (Nothing, m)
  for_ mAsync \a -> do
    cancel a
    waitCatch a >>= \case
      Right live -> stopProcess live.process
      Left _ -> pure ()

-- | Get the 'LiveSession', spawning one if no process is running or if the
-- previous process has exited.
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

-- | The underlying child process handle, spawning the session if needed.
liveProcess :: Session -> IO (Process Handle Handle ())
liveProcess ses = (.process) <$> liveSession ses

initLiveSession :: SessionConfig -> IO LiveSession
initLiveSession cfg = do
  let pcfg =
        proc cfg.command cfg.arguments
          & setStdin createPipe
          & setStdout createPipe
          & setStderr nullStream
  let pcfg' = maybe pcfg (\env -> setEnv env pcfg) cfg.environment
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
