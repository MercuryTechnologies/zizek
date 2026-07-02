-- | The per-test-case handle and its lifecycle.
--
-- __Internal module.__ Implementation substrate of @zizek@ itself, exposed so
-- you can reach past the public API when you must; it is not part of the
-- stable public interface and may change without notice.
--
-- Defines 'TestCase' — a @hegel_test_case_t*@ pointer paired with the
-- @hegel_context_t*@ it is driven under — plus the runner's lifecycle verbs
-- ('mkTestCase' to construct, 'markComplete' to conclude). The generator-facing
-- draw operations live in "Hegel.Internal.DataSource"; the control signals in
-- "Hegel.Internal.Control".
module Hegel.Internal.TestCase
  ( -- * Construction
    mkTestCase,

    -- * Test case
    Handle (..),
    TestCase (..),

    -- * Completion
    Status (..),
    markComplete,
  )
where

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Foreign (Ptr, nullPtr)
import Foreign.C.Types (CInt)
import Hegel.Internal.CString qualified as CString
import Hegel.Internal.Event (Event)
import Hegel.Internal.FFI
import Hegel.Internal.Tick qualified as Tick
import UnliftIO.IORef (IORef, newIORef)
import Witch qualified

-- * Construction

-- | Build the per-case environment around an engine 'Handle'.
--
-- For run-owned handles the case pointer is borrowed from the run handle and
-- remains valid only for the duration of the current test case (until
-- 'markComplete' is called and the runner fetches the next case via
-- 'hegel_next_test_case'). Blob-derived replay handles are caller-owned and
-- freed by their bracket instead.
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
  pure TestCase {handle, slot, recording, events}

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
    events :: !(IORef (Seq Event))
  }

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
      CString.withText origin (hegel_mark_complete tc.handle.ctx tc.handle.ptr (Witch.into @CInt status))
    _ -> hegel_mark_complete tc.handle.ctx tc.handle.ptr (Witch.into @CInt status) nullPtr
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
instance Witch.From Status CInt where
  from Valid = HEGEL_STATUS_VALID
  from Invalid = HEGEL_STATUS_INVALID
  from Overrun = HEGEL_STATUS_OVERRUN
  from (Interesting _) = HEGEL_STATUS_INTERESTING
