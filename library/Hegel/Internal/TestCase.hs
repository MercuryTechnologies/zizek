-- | The per-test-case handle and its lifecycle.
--
-- 'TestCase' pairs a @hegel_test_case_t*@ pointer with the @hegel_context_t*@
-- it is driven under, with 'mkTestCase' to construct and 'markComplete' to
-- conclude.
--
-- The generator-facing draw operations live in "Hegel.Internal.DataSource",
-- the control signals in "Hegel.Internal.Control".
module Hegel.Internal.TestCase
  ( -- * Construction
    mkTestCase,
    withClone,
    allocateClone,
    withClonePair,
    withClones,

    -- * Test case
    Handle (..),
    TestCase (..),

    -- * Draw provenance
    recordDraw,
    takeDraws,
    forgetDraw,

    -- * Filter attempts
    recordUndo,
    inFilter,
    attempts,
    undoOnReject,

    -- * Completion
    Status (..),
    markComplete,
  )
where

import Control.Exception (bracket, mask_, onException, throwIO)
import Control.Monad (void)
import Data.List qualified as List
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Word (Word32)
import Foreign (Ptr, alloca, nullPtr, peek)
import Hegel.Internal.Control (AssumeRejected (..), ControlSignal (..), TestStopped (..), catchControl)
import Hegel.Internal.Event (Event, Var)
import Hegel.Internal.Foreign.Raw
import Hegel.Internal.Foreign.String qualified as String
import Hegel.Internal.Tick qualified as Tick
import UnliftIO.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Witch qualified

-- * Construction

-- | Build the per-case environment around an engine 'Handle'.
--
-- The test-case handle stays caller-owned, and the runner frees it with
-- 'hegel_test_case_free' once the case finishes.
--
-- In 'IO' to allocate the case's reusable draw 'Slot' and its event buffer.
mkTestCase :: Tick.Recording -> Handle -> IO TestCase
mkTestCase recording handle = do
  slot <- newSlot
  events <- newIORef Seq.empty
  draws <- newIORef []
  spanDepth <- newIORef 0
  undo <- newIORef Nothing
  liveClones <- newIORef 0
  pure TestCase {handle, slot, recording, events, draws, spanDepth, undo, liveClones}

-- | Clone @src@ and run @action@ against the clone, freeing it on every
-- exit, including an exception.
--
-- The clone draws from its own independent choice stream but shares @src@'s
-- outcome and budget, so it can be driven concurrently from another thread
-- without perturbing @src@.
--
-- __NOTE__: the clone must not escape @action@. A clone that outlives
-- 'withClone' without ever being completed or freed wedges the whole run
-- (see 'hegel_test_case_clone'\'s haddock) — the bracket here is what
-- prevents that, so do not call 'hegel_test_case_free' on the clone
-- yourself, and do not return it out of @action@.
withClone :: TestCase -> (TestCase -> IO a) -> IO a
withClone src action = bracket (allocateClone src) snd (action . fst)

-- | Clone @src@ and return the clone with the action that frees it.
--
-- The caller must run the release action exactly once on every exit, since
-- a clone that is never freed wedges the run.
allocateClone :: TestCase -> IO (TestCase, IO ())
allocateClone src = mask_ do
  ctx <- hegel_context_new
  ( do
      clone <- acquire ctx
      atomicModifyIORef' src.liveClones (\n -> (n + 1, ()))
      pure (clone, release ctx clone)
    )
    `onException` void (hegel_context_free ctx)
  where
    acquire :: Ptr HegelContext -> IO TestCase
    acquire ctx = do
      ptr <- alloca \out -> do
        hegel_test_case_clone src.handle.ctx src.handle.ptr out >>= \case
          HEGEL_E_STOP_TEST -> throwIO TestStopped
          HEGEL_E_ASSUME -> throwIO AssumeRejected
          rc -> throwOnError src.handle.ctx rc
        peek out
      recording <- case src.recording of
        Tick.Silent -> pure Tick.Silent
        Tick.Active _ -> Tick.newRecording
      mkTestCase recording Handle {ctx, ptr}
        `onException` void (hegel_test_case_free ctx ptr)
    release :: Ptr HegelContext -> TestCase -> IO ()
    release ctx clone = do
      void (hegel_test_case_free clone.handle.ctx clone.handle.ptr)
      void (hegel_context_free ctx)
      atomicModifyIORef' src.liveClones (\n -> (n - 1, ()))

-- | Acquire two clones of @tc@ in a fixed order, against @tc@ itself rather
-- than each other, so both fork positions are direct children at clone depth
-- one and consume their choice positions in the same order on every replay.
withClonePair :: TestCase -> (TestCase -> TestCase -> IO r) -> IO r
withClonePair tc k = withClone tc \c1 -> withClone tc \c2 -> k c1 c2

-- | Acquire @n@ clones of @tc@ sequentially, in a fixed order, for the same
-- reason 'withClonePair' does.
withClones :: Int -> TestCase -> ([TestCase] -> IO r) -> IO r
withClones n0 tc k = go n0 []
  where
    go 0 acc = k (reverse acc)
    go n acc = withClone tc \c -> go (n - 1) (c : acc)

-- * Test case

-- | The engine's per-case pointer pair: a @hegel_test_case_t*@ together with
-- the @hegel_context_t*@ it is driven under.
data Handle = Handle
  { ctx :: !(Ptr HegelContext),
    ptr :: !(Ptr HegelTestCase)
  }

-- | The per-case environment: the engine 'Handle' plus the per-case run
-- context the Haskell side threads with it.
--
-- Generators, collections, and the runner pass 'TestCase' values into the
-- FFI bindings rather than touching the raw pointers directly.
data TestCase = TestCase
  { -- | The engine pointer pair every FFI call goes through (unpacked:
    -- the nesting is conceptual, not a layout cost on the draw hot path).
    handle :: {-# UNPACK #-} !Handle,
    -- | Where this case's draw replies return through; see 'Slot'.
    slot :: !Slot,
    -- | Whether this case is recording, and the clock the note journal and the
    -- pool-event stream share; see "Hegel.Internal.Tick".
    recording :: !Tick.Recording,
    -- | This case's pool-event buffer; appended to (via
    -- 'Hegel.Internal.Tick.record') only while 'recording' is
    -- 'Hegel.Internal.Tick.Active'. See "Hegel.Internal.Event".
    events :: !(IORef (Seq Event)),
    -- | Pool 'Var's drawn since the last 'forAll' boundary, newest-first.
    draws :: !(IORef [Var]),
    -- | How many spans this case has open, so a caller that recovers from an
    -- unwind can close or forget the spans it left behind.
    spanDepth :: !(IORef Int),
    -- | What the innermost open attempt's draws took, or 'Nothing' outside any
    -- attempt. See 'attempts' and 'undoOnReject'.
    undo :: !(IORef (Maybe Undo)),
    -- | How many clones of this case are currently held, for tests that bound
    -- a combinator's peak.
    liveClones :: !(IORef Int)
  }

-- * Draw provenance

-- | Record that a pool draw resolved 'Var' @v@, for provenance attribution.
recordDraw :: TestCase -> Var -> IO ()
recordDraw tc v = case tc.recording of
  Tick.Silent -> pure ()
  Tick.Active _ -> atomicModifyIORef' tc.draws \vs -> (v : vs, ())
{-# INLINE recordDraw #-}

-- | Take and clear the pending draw provenance, oldest-first.
takeDraws :: TestCase -> IO [Var]
takeDraws tc = case tc.recording of
  Tick.Silent -> pure []
  Tick.Active _ -> atomicModifyIORef' tc.draws \vs -> ([], reverse vs)
{-# INLINE takeDraws #-}

-- | Drop the pending provenance for 'Var' @v@, whose draw was undone.
forgetDraw :: TestCase -> Var -> IO ()
forgetDraw tc v = case tc.recording of
  Tick.Silent -> pure ()
  Tick.Active _ -> atomicModifyIORef' tc.draws \vs -> (List.delete v vs, ())

-- * Filter attempts

-- | The undo actions an open attempt's draws registered, newest first.
data Undo = Undo
  { -- | Whether the attempt is one of a filter's, rather than a rule body.
    filtering :: !Bool,
    actions :: ![IO ()]
  }

-- | Register an action that restores what a draw just took, which runs if the
-- attempt enclosing the draw is rejected. Outside any attempt it is dropped.
recordUndo :: TestCase -> IO () -> IO ()
recordUndo tc act = modifyIORef' tc.undo (fmap \u -> u {actions = act : u.actions})

-- | Is a filter attempt open on this case?
inFilter :: TestCase -> IO Bool
inFilter tc = maybe False (.filtering) <$> readIORef tc.undo

-- | Run @act@ as a single attempt that only an 'AssumeRejected' rejects,
-- putting back what it took before the exception propagates.
undoOnReject :: TestCase -> IO a -> IO a
undoOnReject tc act = attemptsIn False tc 1 (Just <$> act) >>= maybe (throwIO AssumeRejected) pure

-- | Run a filter's @attempt@ up to @n@ times until it returns 'Just', yielding
-- that result or 'Nothing' once every attempt has been rejected.
--
-- What a rejected attempt's draws took stays taken until the attempts are
-- over, so no later attempt draws it again, and is then put back. A kept
-- attempt's restores pass to the enclosing attempt, so they still run if that
-- attempt is rejected. An attempt that throws 'AssumeRejected' rejects the
-- whole loop, putting back everything the attempts took before the exception
-- propagates.
--
-- A failure or 'TestStopped' leaves everything taken, since the case is over.
attempts :: TestCase -> Int -> IO (Maybe a) -> IO (Maybe a)
attempts = attemptsIn True

-- | 'attempts', with @filtering@ saying whether they belong to a filter.
attemptsIn :: Bool -> TestCase -> Int -> IO (Maybe a) -> IO (Maybe a)
attemptsIn filtering tc n attempt = do
  outer <- readIORef tc.undo
  (result, kept, rejected) <- go n [] `onException` writeIORef tc.undo outer
  writeIORef tc.undo outer
  sequence_ rejected
  modifyIORef' tc.undo (fmap \u -> u {actions = kept <> u.actions})
  pure result
  where
    taken = maybe [] (.actions) <$> readIORef tc.undo
    go k rejected = do
      writeIORef tc.undo (Just Undo {filtering, actions = []})
      result <-
        attempt `catchControl` \case
          Assume -> do
            undos <- taken
            sequence_ (undos <> rejected)
            throwIO AssumeRejected
          Stop -> throwIO TestStopped
      undos <- taken
      case result of
        Just _ -> pure (result, undos, rejected)
        Nothing
          | k > 1 -> go (k - 1) (undos <> rejected)
          | otherwise -> pure (Nothing, [], undos <> rejected)

-- * Completion

-- | Report the final outcome for this test case.
--
-- Handles 'HEGEL_E_STOP_TEST' for all statuses: the engine may return it as
-- a normal "continue" signal at any point during the run (not only after
-- INTERESTING).
--
-- Call it exactly once per case, from the runner that pulled the case.
markComplete :: TestCase -> Status -> IO ()
markComplete tc status = do
  -- The status code is the 'Status' discriminant ('Witch.into'); only an
  -- 'Interesting' case also carries an origin string, passed separately.
  rc <- case status of
    Interesting origin ->
      String.withText origin (hegel_mark_complete tc.handle.ctx tc.handle.ptr (Witch.into @Word32 status))
    _ -> hegel_mark_complete tc.handle.ctx tc.handle.ptr (Witch.into @Word32 status) nullPtr
  case rc of
    HEGEL_OK -> pure ()
    HEGEL_E_STOP_TEST -> pure ()
    _ -> throwOnError tc.handle.ctx rc

-- | Final outcome of a test case, sent via 'markComplete'.
data Status
  = -- | The case completed successfully.
    Valid
  | -- | The case was deliberately discarded (an assume\/filter rejection).
    -- Runners tally these as invalid cases, distinct from 'Overrun'.
    Invalid
  | -- | The case ran out of entropy mid-generation. Not counted as a
    -- rejection; it is a budget-exhaustion signal (e.g. a shrink probe).
    Overrun
  | -- | The case failed; the payload is the origin string used for
    -- deduplication.
    Interesting Text
  deriving stock (Eq, Show)

-- | The @hegel_status_t@ wire value.
--
-- Note this is the status /discriminant/ only: an 'Interesting' case's origin
-- is passed to 'markComplete' separately.
instance Witch.From Status Word32 where
  from Valid = HEGEL_STATUS_VALID
  from Invalid = HEGEL_STATUS_INVALID
  from Overrun = HEGEL_STATUS_OVERRUN
  from (Interesting _) = HEGEL_STATUS_INTERESTING
