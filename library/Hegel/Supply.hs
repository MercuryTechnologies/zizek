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
-- deterministically when they happen in the same order on every run. Give
-- each concurrent caller its own 'Supply' with 'split' instead.
--
-- __NOTE__: A stub that catches every exception would swallow the signals a
-- draw raises to discard or stop the test case. The 'Supply' remembers the
-- first such signal, rethrows it from every later draw, and rethrows it again
-- when 'withSupply' returns, so the property sees it regardless of what the
-- stub did with it.
--
-- __NOTE__: 'withSupply' and 'split' each consume a choice position, so they
-- must run unconditionally and in the same order on every replay, exactly
-- like 'Hegel.Property.Fork.spawn'.
module Hegel.Supply
  ( Supply,
    withSupply,
    draw,
    split,
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar, withMVar)
import Control.Exception (SomeException)
import Control.Exception qualified as E
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (for_, traverse_)
import Data.Text (Text)
import Data.Text qualified as T
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
import UnliftIO.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)

-- | A source of generated values for code running in plain 'IO', valid only
-- inside the 'withSupply' call that created it.
data Supply = Supply
  { testCase :: !TestCase,
    -- | Held across every draw and split, so concurrent callers take turns on
    -- the clone.
    lock :: !(MVar ()),
    journal :: !Journal,
    drainNotes :: !(IO [Note]),
    cloneDepth :: !Int,
    family :: !Family
  }

-- | The state every 'Supply' from one 'withSupply' call shares.
data Family = Family
  { -- | Every 'Supply' in the family, newest first. Guards 'closed' too.
    members :: !(MVar [Supply]),
    -- | Set once 'withSupply' has returned, after which the clones are gone.
    closed :: !(IORef Bool),
    -- | The first exception a draw or split raised, rethrown by every later
    -- draw and by 'withSupply'.
    poison :: !(IORef (Maybe SomeException)),
    parentJournal :: !Journal,
    noteDepth :: !Int,
    cloneDepthLimit :: !Int
  }

-- | Run an 'IO' action with a fresh 'Supply' on its own cloned choice stream.
--
-- Neither the 'Supply' nor any 'split' of it may be used after this returns.
-- A draw from a closed 'Supply' throws a malformed-test error.
withSupply :: (HasCallStack, MonadIO m) => (Supply -> IO r) -> PropertyT m r
withSupply body = withFrozenCallStack do
  env <- askEnv
  liftIO do
    checkCloneDepth env
    TestCase.withClone env.testCase \clone -> E.mask \restore -> do
      family <- newFamily env
      root <- newSupply family (env.cloneDepth + 1) clone
      modifyMVar_ family.members (pure . (root :))
      let settle = do
            closeFamily family
            -- Release every split newest first. The root, last in the list,
            -- belongs to 'TestCase.withClone'.
            splits <- drop 1 . reverse <$> readMVar family.members
            for_ (reverse splits) \s -> TestCase.releaseClone s.testCase
      result <- tryProperty (restore (body root)) `E.onException` settle
      settle
      supplies <- reverse <$> readMVar family.members
      for_ (zip [1 :: Int ..] supplies) \(i, s) -> do
        notes <- s.drainNotes
        failureNotes <- case (s.journal, result) of
          (Recording _, Left e) | i == 1 && isFailure e -> do
            clock <- Tick.next clone.recording
            let (msg, mloc, diff) = failureDetails e
            pure [Note {kind = BranchFailure diff, text = msg, loc = mloc, depth = family.noteDepth, clock}]
          _ -> pure []
        foldForkNotes env "Supply" i (notes <> failureNotes)
      readIORef family.poison >>= traverse_ E.throwIO
      either E.throwIO pure result

-- | Draw a value from the 'Supply', journaling it as @label=value@.
--
-- Throws whatever the underlying draw throws, including the signals that
-- discard or stop the test case.
draw :: (HasCallStack, Show a) => Supply -> Text -> Gen a -> IO a
draw supply label gen = withFrozenCallStack $ withMVar supply.lock \() -> do
  checkUsable supply "Hegel.Supply.draw" [("label", label)]
  poisonOnError supply.family (Gen.draw supply.testCase gen) >>= \a -> do
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
              depth = supply.family.noteDepth,
              clock
            }
    pure a

-- | A new 'Supply' on its own choice stream, cloned from this one, for
-- handing to another thread.
--
-- The new 'Supply' shares this one's scope, so it closes when the enclosing
-- 'withSupply' returns, and its draws are reported under their own
-- @Supply N@ header, numbered in the order the splits happened.
split :: (HasCallStack) => Supply -> IO Supply
split parent = withFrozenCallStack $ withMVar parent.lock \() -> E.mask_ do
  let family = parent.family
  checkUsable parent "Hegel.Supply.split" []
  when (parent.cloneDepth >= family.cloneDepthLimit) do
    poisonWith family $
      malformedTest
        "Hegel.Supply.split"
        "clone-stream nesting exceeded Settings.maxCloneDepth"
        [("maxCloneDepth", T.pack (show family.cloneDepthLimit))]
  modifyMVar family.members \supplies -> do
    -- 'closeFamily' sets 'closed' under this same 'MVar', so a split can't
    -- slip a new clone in after the family has been released.
    isClosed <- readIORef family.closed
    when isClosed do
      E.throwIO (malformedTest "Hegel.Supply.split" "split a Supply after its withSupply scope ended" [])
    clone <- poisonOnError family (TestCase.acquireClone parent.testCase)
    child <- newSupply family (parent.cloneDepth + 1) clone `E.onException` TestCase.releaseClone clone
    pure (child : supplies, child)

-- * Mechanics

newFamily :: Env -> IO Family
newFamily env = do
  members <- newMVar []
  closed <- newIORef False
  poison <- newIORef Nothing
  pure
    Family
      { members,
        closed,
        poison,
        parentJournal = env.journal,
        noteDepth = env.noteDepth + 1,
        cloneDepthLimit = env.cloneDepthLimit
      }

newSupply :: Family -> Int -> TestCase -> IO Supply
newSupply family cloneDepth testCase = do
  lock <- newMVar ()
  (journal, drainNotes) <- newChildJournal family.parentJournal
  pure Supply {testCase, lock, journal, drainNotes, cloneDepth, family}

-- | Throw if the family has closed, or rethrow its poison if it has one.
--
-- Callers hold the 'Supply''s lock, so a draw that passes this check finishes
-- before 'closeFamily' can release the clone underneath it.
checkUsable :: (HasCallStack) => Supply -> Text -> [(Text, Text)] -> IO ()
checkUsable supply context values = do
  isClosed <- readIORef supply.family.closed
  when isClosed do
    E.throwIO (malformedTest context "used a Supply after its withSupply scope ended" values)
  readIORef supply.family.poison >>= traverse_ E.throwIO

-- | Run an engine call, recording any control signal or failure it raises as
-- the family's poison before rethrowing it.
poisonOnError :: Family -> IO a -> IO a
poisonOnError family act =
  tryProperty act >>= \case
    Left e -> poisonWith family e
    Right a -> pure a

poisonWith :: (E.Exception e) => Family -> e -> IO a
poisonWith family e = do
  let se = E.toException e
  atomicModifyIORef' family.poison \prior -> (Just (maybe se id prior), ())
  E.throwIO se

-- | Mark the family closed, then wait out any draw or split still in flight.
closeFamily :: Family -> IO ()
closeFamily family = do
  supplies <- modifyMVar family.members \supplies -> do
    writeIORef family.closed True
    pure (supplies, supplies)
  for_ supplies \s -> withMVar s.lock \() -> pure ()
