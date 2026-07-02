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
    TestCase (..),

    -- * Completion
    Status (..),
    markComplete,
  )
where

import Data.Text (Text)
import Foreign (Ptr, nullPtr)
import Foreign.C.Types (CInt)
import Hegel.Internal.CString qualified as CString
import Hegel.Internal.FFI
import Witch qualified

-- * Construction

-- | Pair a run-owned @hegel_test_case_t*@ pointer with the error-reporting
-- context the run is driven under.
--
-- For run-owned handles the pointer is borrowed from the run handle and
-- remains valid only for the duration of the current test case (until
-- 'markComplete' is called and the runner fetches the next case via
-- 'hegel_next_test_case'). Blob-derived replay handles are caller-owned and
-- freed by their bracket instead.
--
-- In 'IO' to allocate the case's reusable draw 'Slot'.
mkTestCase :: Ptr HegelContext -> Ptr HegelTestCase -> IO TestCase
mkTestCase ctx ptr = do
  slot <- newSlot
  pure TestCase {ptr, ctx, slot}

-- * Test case

-- | A @hegel_test_case_t*@ pointer together with the @hegel_context_t*@ it is
-- driven under.
--
-- Generators, collections, and the runner pass 'TestCase' handles into the
-- FFI bindings rather than touching these pointers directly.
data TestCase = TestCase
  { ptr :: Ptr HegelTestCase,
    ctx :: Ptr HegelContext,
    -- | Where this case's draw replies return through; see 'Slot'.
    slot :: Slot
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
      CString.withText origin (hegel_mark_complete tc.ctx tc.ptr (Witch.into @CInt status))
    _ -> hegel_mark_complete tc.ctx tc.ptr (Witch.into @CInt status) nullPtr
  case rc of
    HEGEL_OK -> pure ()
    HEGEL_E_STOP_TEST -> pure ()
    _ -> throwOnError tc.ctx rc

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
