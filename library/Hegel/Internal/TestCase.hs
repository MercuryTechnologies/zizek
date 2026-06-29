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
import Data.Text qualified as T
import Foreign (Ptr, nullPtr)
import Foreign.C.String (withCString)
import Hegel.Internal.FFI

-- * Construction

-- | Pair a run-owned @hegel_test_case_t*@ pointer with the error-reporting
-- context the run is driven under.
--
-- The pointer is borrowed from the run handle and remains valid only for the
-- duration of the current test case (until 'markComplete' is called and the
-- runner fetches the next case via 'hegel_next_test_case').
mkTestCase :: Ptr HegelContext -> Ptr HegelTestCase -> TestCase
mkTestCase ctx ptr = TestCase {ptr, ctx}

-- * Test case

-- | A @hegel_test_case_t*@ pointer together with the @hegel_context_t*@ it is
-- driven under. Generators, collections, and the runner call free functions
-- (here and in "Hegel.Internal.DataSource"), passing 'TestCase' as the first
-- argument, rather than touching the pointers directly; every per-test-case
-- @libhegel@ call needs both.
data TestCase = TestCase
  { ptr :: Ptr HegelTestCase,
    ctx :: Ptr HegelContext
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
  rc <- case status of
    Valid -> hegel_mark_complete tc.ctx tc.ptr HEGEL_STATUS_VALID nullPtr
    Invalid -> hegel_mark_complete tc.ctx tc.ptr HEGEL_STATUS_INVALID nullPtr
    Overrun -> hegel_mark_complete tc.ctx tc.ptr HEGEL_STATUS_OVERRUN nullPtr
    Interesting origin ->
      withCString (T.unpack origin) \p ->
        hegel_mark_complete tc.ctx tc.ptr HEGEL_STATUS_INTERESTING p
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
