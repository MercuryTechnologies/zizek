-- | An escaping fork of a property body, running concurrently on its own
-- cloned choice stream.
--
-- 'spawn' returns a handle immediately, without waiting for it. 'join' waits
-- for its result, 'cancel' abandons it, and 'poll' checks its status without
-- discharging the join-or-cancel obligation. Every fork must be joined or
-- cancelled before its scope ends; one left open aborts the run as a
-- malformed test, since an unjoined fork is a defect in the test itself.
--
-- A joined fork's failure rethrows bare, so it shrinks as an ordinary
-- counterexample.
--
-- 'scoped' is the alternative for a fork whose lifetime is naturally bounded
-- to one block of code, cancelling it automatically at the end of that
-- block; reach for 'spawn' only when a handle genuinely needs to outlive the
-- code that created it.
--
-- __NOTE__: A fork call consumes a choice position, so it must run
-- unconditionally on every replay. Guarding a 'spawn' behind a drawn value
-- desynchronises the choice stream, and the counterexample stops
-- reproducing.
--
-- 'spawn' is not for fan-out: reach for
-- 'Hegel.Property.Branch.replicateConcurrently' or
-- 'Hegel.Property.Branch.replicateConcurrentlyBounded' instead of
-- forking the same action in a loop.
module Hegel.Property.Fork
  ( Fork,
    spawn,
    join,
    cancel,
    poll,
    scoped,
  )
where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Control.Exception qualified as E
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Internal.Control (isFailure)
import Hegel.Internal.TestCase qualified as TestCase
import Hegel.Property.Internal
  ( Env (openForks, testCase),
    ForkEntry (..),
    ForkState (..),
    OpenForks,
    PropertyT,
    askEnv,
    checkCloneDepth,
    deregisterFork,
    failureDetails,
    foldForkNotes,
    registerFork,
    runBranch,
    withBaseRunInIO,
  )
import Hegel.Report.Note (Note)
import UnliftIO (MonadUnliftIO, bracket)
import UnliftIO.Async (Async)
import UnliftIO.Async qualified as Async
import UnliftIO.IORef (IORef, newIORef, readIORef, writeIORef)

-- | A property body running concurrently on its own cloned choice stream,
-- spawned by 'spawn' or 'scoped'.
--
-- Manipulated only through 'join', 'cancel', and 'poll', so its lifetime is
-- always explicit.
data Fork a = Fork
  { handle :: !(Async (Either E.SomeException a, [Note])),
    state :: !(IORef ForkState),
    key :: !Int,
    registry :: !OpenForks
  }

-- | Spawn a property action on its own cloned choice stream and return a
-- handle to it immediately, without waiting for it.
--
-- @
-- worker <- Fork.spawn do
--   v <- forAll (Gen.int & Gen.min 0 & Gen.max 100 & Gen.build)
--   assert (v < 30) "worker: value too big"
-- ...
-- Fork.join worker
-- @
--
-- The clone is acquired before this call returns, even though the fork's body
-- then keeps running in the background. Acquisition consumes a choice
-- position, so its order has to be fixed by the order 'spawn' is called;
-- otherwise two forks racing to clone the same parent would consume choice
-- positions in a scheduling-dependent order and break replay. A background
-- thread does the acquiring, but this call blocks until it has, the same
-- guarantee 'Hegel.Property.Branch.concurrently' gets by acquiring its clones
-- up front.
--
-- The spawned thread is cancelled if anything goes wrong before the finished
-- 'Fork' is handed back, including an asynchronous exception landing here
-- while it waits, since an unregistered, uncancelled fork would never be
-- settled and would wedge the run the way a leaked clone does.
spawn :: (MonadUnliftIO m) => PropertyT m a -> PropertyT m (Fork a)
spawn body = do
  env <- askEnv
  liftIO (checkCloneDepth env)
  withBaseRunInIO \runBase -> do
    stateRef <- newIORef NotJoined
    acquired <- newEmptyMVar
    let runClone clone = putMVar acquired True *> runBranch runBase env clone body
        onAcquireFailure = tryPutMVar acquired False
        spawnAsync = Async.async (TestCase.withClone env.testCase runClone `E.onException` onAcquireFailure)
    E.bracketOnError spawnAsync Async.uninterruptibleCancel \handle -> do
      wasAcquired <- takeMVar acquired
      -- The clone acquisition itself failed, before the body ever ran;
      -- 'handle' has already died for the same reason, so rethrow it here
      -- rather than returning a 'Fork' whose thread is already gone.
      unless wasAcquired do
        Async.waitCatch handle >>= \case
          Left e -> E.throwIO e
          Right _ -> pure ()
      key <- registerFork env.openForks ForkEntry {state = stateRef, settle = settleFork stateRef handle}
      pure Fork {handle, state = stateRef, key, registry = env.openForks}

-- | Wait for a fork's result, rethrowing a branch failure bare so it shrinks
-- as an ordinary counterexample, and folding its buffered notes into the
-- ambient journal under a @Fork N@ header.
--
-- Safe to call more than once: the result is delivered again each time, but
-- notes fold only on the first call.
--
-- The state check, note fold, and deregistration run as one uninterruptible
-- unit so an asynchronous exception landing mid-update can't leave a fork
-- that really was observed still looking open to
-- 'Hegel.Property.Internal.closeOpenForks'.
join :: (MonadUnliftIO m) => Fork a -> PropertyT m a
join fork = do
  env <- askEnv
  liftIO do
    r <- Async.waitCatch fork.handle
    E.uninterruptibleMask_ do
      firstTime <- (== NotJoined) <$> readIORef fork.state
      when firstTime do
        case r of
          Left _ -> pure ()
          Right (_, notes) -> foldForkNotes env "Fork" fork.key notes
        writeIORef fork.state case r of
          Right (Right _, _) -> JoinedOk
          _ -> JoinedFailure
        deregisterFork fork.registry fork.key
    case r of
      Left threadDied -> E.throwIO threadDied
      Right (outcome, _) -> either E.throwIO pure outcome

-- | Abandon a fork, cancelling it and freeing its clone.
--
-- Idempotent against a fork already joined or cancelled. The fork's result,
-- if any, is discarded; a fork whose value matters should be joined instead.
--
-- Runs as one uninterruptible unit for the same reason 'join''s bookkeeping
-- does.
cancel :: (MonadUnliftIO m) => Fork a -> PropertyT m ()
cancel fork = liftIO do
  E.uninterruptibleMask_ do
    s <- readIORef fork.state
    when (s == NotJoined) do
      Async.uninterruptibleCancel fork.handle
      writeIORef fork.state Cancelled
      deregisterFork fork.registry fork.key

-- | Check a fork's status without joining or cancelling it.
poll :: (MonadUnliftIO m) => Fork a -> PropertyT m (Maybe (Either E.SomeException a))
poll fork = liftIO do
  mr <- Async.poll fork.handle
  pure case mr of
    Just (Right (outcome, _notes)) -> Just outcome
    Just (Left e) -> Just (Left e)
    Nothing -> Nothing

-- | Run @body@ on its own cloned choice stream for the duration of @use@,
-- cancelling it and freeing its clone when @use@ returns or throws.
--
-- Join the handle inside @use@ to wait for a result. A handle retained past
-- @use@ is already cancelled, so it yields the cancelled outcome.
scoped :: (MonadUnliftIO m) => PropertyT m a -> (Fork a -> PropertyT m b) -> PropertyT m b
scoped body use = bracket (spawn body) cancel use

-- | Await-or-cancel a fork's thread so its clone is freed, and describe its
-- outcome for a leak message when it had already failed.
--
-- Only ever invoked by the registry on a fork its owner never joined or
-- cancelled, so it settles unconditionally with no state to check first.
-- Runs as one uninterruptible unit for the same reason 'join''s bookkeeping
-- does; the registry's own close can run under a restored, interruptible
-- masking state, so this does not inherit protection for free.
settleFork :: IORef ForkState -> Async (Either E.SomeException a, [Note]) -> IO (Maybe Text)
settleFork stateRef handle = E.uninterruptibleMask_ do
  mr <- Async.poll handle
  Async.uninterruptibleCancel handle
  writeIORef stateRef Cancelled
  pure case mr of
    Just (Right (Left e, _)) | isFailure e -> Just (let (msg, _, _) = failureDetails e in msg)
    Just (Left e) -> Just (T.pack (E.displayException e))
    _ -> Nothing
