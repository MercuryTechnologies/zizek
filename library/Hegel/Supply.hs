-- | A handle for drawing generated values from plain 'IO', for stubs and fakes
-- that live outside the property body.
--
-- @
-- withSupply \\supply -> do
--   let backend = Backend {fetch = \\_ -> Supply.draw supply "status" statusGen}
--   runClient backend
-- @
--
-- A 'Supply' draws from its own cloned choice stream, so its values shrink
-- like any other draw, and a stub that draws a different number of times on
-- a replay leaves the property's own draws in place.
--
-- Every draw is journaled under its label, nested beneath a @Supply N@ header
-- in the failure report.
--
-- A 'Supply' is safe to call from several threads, but its draws only replay
-- deterministically when they happen in the same order on every run.
--
-- __NOTE__: A stub that catches every exception would swallow the signals a
-- draw raises to discard or stop the test case. The 'Supply' remembers the
-- first such signal, rethrows it from every later draw, and rethrows it again
-- when 'withSupply' returns, so the property sees it regardless of what the
-- stub did with it.
--
-- __NOTE__: 'withSupply' consumes a choice position, so it must run
-- unconditionally on every replay, exactly like 'Hegel.Property.Fork.spawn'.
module Hegel.Supply
  ( Supply,
    withSupply,
    draw,
  )
where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (SomeException)
import Control.Exception qualified as E
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (traverse_)
import Data.Text (Text)
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)
import Hegel.Assertion (callSite)
import Hegel.Gen.Internal (Gen)
import Hegel.Gen.Internal qualified as Gen
import Hegel.Internal.Control (isFailure, malformedTest)
import Hegel.Internal.TestCase (TestCase (..))
import Hegel.Internal.TestCase qualified as TestCase
import Hegel.Internal.Tick qualified as Tick
import Hegel.Property.Internal
  ( Env (..),
    Journal (..),
    PropertyT,
    askEnv,
    checkCloneDepth,
    failureDetails,
    foldForkNotes,
    newChildJournal,
    tryProperty,
  )
import Hegel.Report.Note (Note (..), NoteKind (..), renderValue)
import UnliftIO.IORef (IORef, newIORef, readIORef, writeIORef)

-- | A source of generated values for code running in plain 'IO', valid only
-- inside the 'withSupply' call that created it.
data Supply = Supply
  { testCase :: !TestCase,
    -- | Held across every draw, so concurrent callers take turns on the clone.
    lock :: !(MVar ()),
    journal :: !Journal,
    drainNotes :: !(IO [Note]),
    noteDepth :: !Int,
    -- | The first exception a draw raised, rethrown by every later draw.
    poison :: !(IORef (Maybe SomeException)),
    -- | Set once 'withSupply' has returned, after which the clone is gone.
    closed :: !(IORef Bool)
  }

-- | Run an 'IO' action with a fresh 'Supply' on its own cloned choice stream.
--
-- The 'Supply' must not be used after this returns. A draw from a closed
-- 'Supply' throws a malformed-test error.
withSupply :: (HasCallStack, MonadIO m) => (Supply -> IO r) -> PropertyT m r
withSupply body = withFrozenCallStack do
  env <- askEnv
  liftIO do
    checkCloneDepth env
    TestCase.withClone env.testCase \clone -> E.mask \restore -> do
      supply <- newSupply env clone
      result <- tryProperty (restore (body supply)) `E.onException` close supply
      close supply
      notes <- supply.drainNotes
      failureNotes <- case (supply.journal, result) of
        (Recording _, Left e) | isFailure e -> do
          clock <- Tick.next clone.recording
          let (msg, mloc, diff) = failureDetails e
          pure [Note {kind = BranchFailure diff, text = msg, loc = mloc, depth = supply.noteDepth, clock}]
        _ -> pure []
      foldForkNotes env "Supply" 1 (notes <> failureNotes)
      readIORef supply.poison >>= traverse_ E.throwIO
      either E.throwIO pure result

-- | Draw a value from the 'Supply', journaling it as @label=value@.
--
-- Throws whatever the underlying draw throws, including the signals that
-- discard or stop the test case.
draw :: (HasCallStack, Show a) => Supply -> Text -> Gen a -> IO a
draw supply label gen = withFrozenCallStack $ withMVar supply.lock \() -> do
  isClosed <- readIORef supply.closed
  when isClosed do
    E.throwIO (malformedTest "Hegel.Supply.draw" "drew from a Supply after its withSupply scope ended" [("label", label)])
  readIORef supply.poison >>= traverse_ E.throwIO
  tryProperty (Gen.draw supply.testCase gen) >>= \case
    Left e -> do
      writeIORef supply.poison (Just e)
      E.throwIO e
    Right a -> do
      -- Pool provenance has nowhere to go in a supply note, so drop it rather
      -- than let it leak onto the parent's next draw.
      _ <- TestCase.takeDraws supply.testCase
      case supply.journal of
        Silent -> pure ()
        Recording sink -> do
          clock <- Tick.next supply.testCase.recording
          sink
            Note
              { kind = Drawn [],
                text = label <> "=" <> renderValue a,
                loc = callSite callStack,
                depth = supply.noteDepth,
                clock
              }
      pure a

-- * Mechanics

newSupply :: Env -> TestCase -> IO Supply
newSupply env testCase = do
  lock <- newMVar ()
  (journal, drainNotes) <- newChildJournal env.journal
  poison <- newIORef Nothing
  closed <- newIORef False
  pure
    Supply
      { testCase,
        lock,
        journal,
        drainNotes,
        noteDepth = env.noteDepth + 1,
        poison,
        closed
      }

-- | Mark the 'Supply' closed, waiting out any draw still in flight.
close :: Supply -> IO ()
close supply = withMVar supply.lock \() -> writeIORef supply.closed True
