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

    -- * Test case
    Handle (..),
    TestCase (..),

    -- * Draw provenance
    recordDraw,
    takeDraws,

    -- * Completion
    Status (..),
    markComplete,
  )
where

import Control.Exception (bracket)
import Control.Monad (void)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Word (Word32)
import Foreign (Ptr, alloca, nullPtr, peek)
import Hegel.Internal.Event (Event, Var)
import Hegel.Internal.Foreign.CString qualified as CString
import Hegel.Internal.Foreign.Raw
import Hegel.Internal.Tick qualified as Tick
import UnliftIO.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef)
import Witch qualified

-- * Construction

-- | Build the per-case environment around an engine 'Handle'.
--
-- The test-case handle is caller-owned whatever its origin: one from
-- 'hegel_next_test_case' is freed once its 'markComplete' has run, and one
-- replayed from a blob is freed by its bracket.
--
-- The 'Tick.Recording' selects whether this case records: ordinary cases (and
-- every shrink replay) pass 'Hegel.Internal.Tick.Silent'; only the final
-- reconstruction replay passes a recording toggle
-- ('Hegel.Internal.Tick.newRecording') — the same once-per-failure discipline
-- as the note journal.
--
-- In 'IO' to allocate the case's reusable draw 'Slot' and its event buffer.
mkTestCase :: Tick.Recording -> Handle -> IO TestCase
mkTestCase recording handle = do
  slot <- newSlot
  events <- newIORef Seq.empty
  draws <- newIORef []
  pure TestCase {handle, slot, recording, events, draws}

-- | Clone @src@ and run @action@ against the clone, freeing it on every
-- exit, including an exception.
--
-- The clone draws from its own independent choice stream but shares @src@'s
-- outcome and budget, so it can be driven concurrently from another thread
-- without perturbing @src@. It gets its own 'Slot' and event\/draw buffers
-- from 'mkTestCase' the same way every other 'TestCase' does, and its own
-- recording clock when @src@ is 'Tick.Active' rather than @src@'s: nothing
-- about a clone is shared Haskell-side mutable state with the case it came
-- from.
--
-- __NOTE__: the clone must not escape @action@. A clone that outlives
-- 'withClone' without ever being completed or freed wedges the whole run
-- (see 'hegel_test_case_clone'\'s haddock) — the bracket here is what
-- prevents that, so do not call 'hegel_test_case_free' on the clone
-- yourself, and do not return it out of @action@.
withClone :: TestCase -> (TestCase -> IO a) -> IO a
withClone src = bracket acquire release
  where
    acquire :: IO TestCase
    acquire = do
      ptr <- alloca \out -> do
        throwOnError src.handle.ctx =<< hegel_test_case_clone src.handle.ctx src.handle.ptr out
        peek out
      recording <- case src.recording of
        Tick.Silent -> pure Tick.Silent
        Tick.Active _ -> Tick.newRecording
      mkTestCase recording Handle {ctx = src.handle.ctx, ptr}
    release :: TestCase -> IO ()
    release clone = void (hegel_test_case_free clone.handle.ctx clone.handle.ptr)

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
    draws :: !(IORef [Var])
  }

-- * Draw provenance

-- | Record that a pool draw resolved 'Var' @v@, for provenance attribution.
recordDraw :: TestCase -> Var -> IO ()
recordDraw tc v = case tc.recording of
  Tick.Silent -> pure ()
  Tick.Active _ -> modifyIORef' tc.draws (v :)
{-# INLINE recordDraw #-}

-- | Take and clear the pending draw provenance, oldest-first.
takeDraws :: TestCase -> IO [Var]
takeDraws tc = case tc.recording of
  Tick.Silent -> pure []
  Tick.Active _ -> atomicModifyIORef' tc.draws \vs -> ([], reverse vs)
{-# INLINE takeDraws #-}

-- * Completion

-- | Report the final outcome for this test case.
--
-- Handles 'HEGEL_E_STOP_TEST' for all statuses: the engine may return it as
-- a normal "continue" signal at any point during the run (not only after
-- INTERESTING).
--
-- Only called from the live run path ('Hegel.Runner.runTestCase').
--
-- The replay path ('Hegel.Runner.reconstructProperty') only draws and journals;
-- it never marks completion, so from-blob handles are safe to pass through
-- 'mkTestCase'.
markComplete :: TestCase -> Status -> IO ()
markComplete tc status = do
  -- The status code is the 'Status' discriminant ('Witch.into'); only an
  -- 'Interesting' case also carries an origin string, passed separately.
  rc <- case status of
    Interesting origin ->
      CString.withText origin (hegel_mark_complete tc.handle.ctx tc.handle.ptr (Witch.into @Word32 status))
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

-- | The @hegel_status_t@ wire value.
--
-- Note this is the status /discriminant/ only: an 'Interesting' case's origin
-- is passed to 'markComplete' separately.
instance Witch.From Status Word32 where
  from Valid = HEGEL_STATUS_VALID
  from Invalid = HEGEL_STATUS_INVALID
  from Overrun = HEGEL_STATUS_OVERRUN
  from (Interesting _) = HEGEL_STATUS_INTERESTING
