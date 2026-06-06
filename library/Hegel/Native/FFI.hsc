-- | Low-level FFI bindings to @libhegel@ (@hegeltest-c@).
--
-- Every @hegel_*@ function from @hegel.h@ is exposed as a
-- 'foreign import ccall' declaration together with phantom types representing
-- handles to C constructs, error-code pattern synonyms, and bracket helpers.
--
-- __Thread Discipline__: 'hegel_last_error_message' writes into a
-- /thread-local/ C buffer, and GHC's threaded RTS may migrate an unbound
-- green thread to a different OS thread at any safe point, including the
-- return of a @safe@ FFI call (and 'hegel_next_test_case' must be @safe@
-- because it blocks).
--
-- Therefore, __all__ libhegel call sequences must run under
-- 'Control.Concurrent.runInBoundThread' (or 'Control.Concurrent.forkOS') so
-- that 'throwOnError' reads the error string on the same OS thread that set it.
module Hegel.Native.FFI
  ( -- * Opaque handle phantoms
    -- $handles
    HegelSettings,
    HegelRun,
    HegelTestCase,
    HegelRunResult,
    HegelFailure,

    -- * Error types
    -- $errortypes
    HegelError (..),
    HegelStartupError (..),

    -- * Error-code pattern synonyms
    -- $errorcodes
    pattern HEGEL_OK,
    pattern HEGEL_E_STOP_TEST,
    pattern HEGEL_E_ASSUME,
    pattern HEGEL_E_BACKEND,
    pattern HEGEL_E_INVALID_HANDLE,
    pattern HEGEL_E_INVALID_ARG,
    pattern HEGEL_E_ALREADY_COMPLETE,
    pattern HEGEL_E_NOT_COMPLETE,
    pattern HEGEL_E_INTERNAL,

    -- * Phase bitmask pattern synonyms
    -- $phases
    pattern HEGEL_PHASE_EXPLICIT,
    pattern HEGEL_PHASE_REUSE,
    pattern HEGEL_PHASE_GENERATE,
    pattern HEGEL_PHASE_TARGET,
    pattern HEGEL_PHASE_SHRINK,
    pattern HEGEL_PHASE_ALL,

    -- * Health-check bitmask pattern synonyms
    -- $healthchecks
    pattern HEGEL_HC_FILTER_TOO_MUCH,
    pattern HEGEL_HC_TOO_SLOW,
    pattern HEGEL_HC_TEST_CASES_TOO_LARGE,
    pattern HEGEL_HC_LARGE_INITIAL_TEST_CASE,

    -- * Span label pattern synonyms
    -- $labels
    pattern HEGEL_LABEL_LIST,
    pattern HEGEL_LABEL_LIST_ELEMENT,
    pattern HEGEL_LABEL_SET,
    pattern HEGEL_LABEL_SET_ELEMENT,
    pattern HEGEL_LABEL_MAP,
    pattern HEGEL_LABEL_MAP_ENTRY,
    pattern HEGEL_LABEL_TUPLE,
    pattern HEGEL_LABEL_ONE_OF,
    pattern HEGEL_LABEL_OPTIONAL,
    pattern HEGEL_LABEL_FIXED_DICT,
    pattern HEGEL_LABEL_FLAT_MAP,
    pattern HEGEL_LABEL_FILTER,
    pattern HEGEL_LABEL_MAPPED,
    pattern HEGEL_LABEL_SAMPLED_FROM,
    pattern HEGEL_LABEL_ENUM_VARIANT,

    -- * Mode pattern synonyms
    -- $modes
    pattern HEGEL_MODE_TEST_RUN,
    pattern HEGEL_MODE_SINGLE_TEST_CASE,

    -- * Verbosity pattern synonyms
    -- $verbosity
    pattern HEGEL_VERBOSITY_QUIET,
    pattern HEGEL_VERBOSITY_NORMAL,
    pattern HEGEL_VERBOSITY_VERBOSE,
    pattern HEGEL_VERBOSITY_DEBUG,

    -- * Status pattern synonyms
    -- $status
    pattern HEGEL_STATUS_VALID,
    pattern HEGEL_STATUS_INVALID,
    pattern HEGEL_STATUS_OVERRUN,
    pattern HEGEL_STATUS_INTERESTING,

    -- * Settings lifecycle
    -- $settings
    hegel_settings_new,
    hegel_settings_free,
    hegel_settings_mode,
    hegel_settings_test_cases,
    hegel_settings_verbosity,
    hegel_settings_seed,
    hegel_settings_derandomize,
    hegel_settings_report_multiple_failures,
    hegel_settings_database,
    hegel_settings_database_key,
    hegel_settings_phases,
    hegel_settings_suppress_health_check,

    -- * Run lifecycle
    -- $run
    hegel_run_start,
    hegel_next_test_case,
    hegel_run_result,
    hegel_run_free,

    -- * Per-test-case primitives
    -- $pertestcase
    hegel_generate,
    hegel_start_span,
    hegel_stop_span,
    hegel_new_collection,
    hegel_collection_more,
    hegel_collection_reject,
    hegel_new_pool,
    hegel_pool_add,
    hegel_pool_generate,
    hegel_target,
    hegel_mark_complete,
    hegel_test_case_is_final_replay,

    -- * Failure reproduction
    -- $reproduction
    hegel_test_case_from_blob,
    hegel_test_case_free,

    -- * Result inspection
    -- $results
    hegel_run_result_passed,
    hegel_run_result_failure_count,
    hegel_run_result_failure,
    hegel_failure_panic_message,
    hegel_failure_diagnostic,
    hegel_failure_origin,
    hegel_failure_reproduction_blob,

    -- * Globals
    -- $globals
    hegel_last_error_message,
    hegel_version,

    -- * Haskell helpers
    -- $helpers
    throwOnError,
    withSettings,
    withRun,
    generate,
    failureReproductionBlob,
    withTestCaseFromBlob,
  )
where

#include <hegel.h>

import Control.Exception (Exception, bracket, throwIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32, Word64, Word8)
import Foreign (Ptr, alloca, castPtr, nullPtr, peek)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CBool (..), CDouble (..), CInt (..), CSize (..))

-- $handles
--
-- Phantom parameters passed to 'Ptr', giving each @libhegel@ handle
-- (e.g. @hegel_settings_t*@, @hegel_run_t*@) a distinct, Haskell type.

-- | Sentinel for @hegel_settings_t*@.
data HegelSettings

-- | Sentinel for @hegel_run_t*@.
data HegelRun

-- | Sentinel for @hegel_test_case_t*@ (borrowed from the run handle).
data HegelTestCase

-- | Sentinel for @hegel_run_result_t*@ (borrowed from the run handle).
data HegelRunResult

-- | Sentinel for @hegel_failure_t*@ (borrowed from the run result).
data HegelFailure

-- $errortypes
--
-- * 'HegelError': a @libhegel@ FFI call returned a non-zero @HEGEL_E_*@ code,
--   which callers can branch off of and potentially handle
-- * 'HegelStartupError': 'hegel_run_start' returned @NULL@, which represents
--   some invalid state from the underlying FFI call has no corresponding
--   return code for a caller to handle.

-- | Exception thrown when a @libhegel@ call returns a non-zero error code.
data HegelError = HegelError
  { -- | The raw @HEGEL_E_*@ error code.
    code :: !CInt,
    -- | Diagnostic from 'hegel_last_error_message', if any.
    message :: !(Maybe Text)
  }
  deriving stock (Show)

instance Exception HegelError

-- | Exception thrown when the engine fails to start.
newtype HegelStartupError = HegelStartupError
  { -- | Diagnostic from 'hegel_last_error_message', if any.
    diagnostic :: Maybe Text
  }
  deriving stock (Show)

instance Exception HegelStartupError

-- $errorcodes
--
-- @CInt@ return codes shared by most libhegel calls.
--
-- 'HEGEL_OK' signals success and the @HEGEL_E_*@ codes classify failures;
-- 'HEGEL_E_STOP_TEST' and 'HEGEL_E_ASSUME' are control-flow signals as
-- opposed to true errors.
-- 
-- Match on these directly, or let 'throwOnError' translate any non-zero
-- code into a 'HegelError'.

pattern HEGEL_OK :: CInt
pattern HEGEL_OK = (#const HEGEL_OK)

pattern HEGEL_E_STOP_TEST :: CInt
pattern HEGEL_E_STOP_TEST = (#const HEGEL_E_STOP_TEST)

pattern HEGEL_E_ASSUME :: CInt
pattern HEGEL_E_ASSUME = (#const HEGEL_E_ASSUME)

pattern HEGEL_E_BACKEND :: CInt
pattern HEGEL_E_BACKEND = (#const HEGEL_E_BACKEND)

pattern HEGEL_E_INVALID_HANDLE :: CInt
pattern HEGEL_E_INVALID_HANDLE = (#const HEGEL_E_INVALID_HANDLE)

pattern HEGEL_E_INVALID_ARG :: CInt
pattern HEGEL_E_INVALID_ARG = (#const HEGEL_E_INVALID_ARG)

pattern HEGEL_E_ALREADY_COMPLETE :: CInt
pattern HEGEL_E_ALREADY_COMPLETE = (#const HEGEL_E_ALREADY_COMPLETE)

pattern HEGEL_E_NOT_COMPLETE :: CInt
pattern HEGEL_E_NOT_COMPLETE = (#const HEGEL_E_NOT_COMPLETE)

pattern HEGEL_E_INTERNAL :: CInt
pattern HEGEL_E_INTERNAL = (#const HEGEL_E_INTERNAL)

-- $phases
--
-- @Word32@ flags, OR\'d and passed to 'hegel_settings_phases', which
-- select the stages of an engine loop run (explicit examples, database
-- reuse, generation, targeting, shrinking).

pattern HEGEL_PHASE_EXPLICIT :: Word32
pattern HEGEL_PHASE_EXPLICIT = (#const HEGEL_PHASE_EXPLICIT)

pattern HEGEL_PHASE_REUSE :: Word32
pattern HEGEL_PHASE_REUSE = (#const HEGEL_PHASE_REUSE)

pattern HEGEL_PHASE_GENERATE :: Word32
pattern HEGEL_PHASE_GENERATE = (#const HEGEL_PHASE_GENERATE)

pattern HEGEL_PHASE_TARGET :: Word32
pattern HEGEL_PHASE_TARGET = (#const HEGEL_PHASE_TARGET)

pattern HEGEL_PHASE_SHRINK :: Word32
pattern HEGEL_PHASE_SHRINK = (#const HEGEL_PHASE_SHRINK)

pattern HEGEL_PHASE_ALL :: Word32
pattern HEGEL_PHASE_ALL = (#const HEGEL_PHASE_ALL)

-- $healthchecks
--
-- @Word32@ flags, OR\'d and passed to 'hegel_settings_suppress_health_check',
-- which silence specific engine health checks such as excessive filtering or
-- test cases that run too slowly.

pattern HEGEL_HC_FILTER_TOO_MUCH :: Word32
pattern HEGEL_HC_FILTER_TOO_MUCH = (#const HEGEL_HC_FILTER_TOO_MUCH)

pattern HEGEL_HC_TOO_SLOW :: Word32
pattern HEGEL_HC_TOO_SLOW = (#const HEGEL_HC_TOO_SLOW)

pattern HEGEL_HC_TEST_CASES_TOO_LARGE :: Word32
pattern HEGEL_HC_TEST_CASES_TOO_LARGE = (#const HEGEL_HC_TEST_CASES_TOO_LARGE)

pattern HEGEL_HC_LARGE_INITIAL_TEST_CASE :: Word32
pattern HEGEL_HC_LARGE_INITIAL_TEST_CASE = (#const HEGEL_HC_LARGE_INITIAL_TEST_CASE)

-- $labels
--
-- @Word64@ identifiers passed to 'hegel_start_span', which tag a span with the
-- structure it represents (e.g. list, set, map, tuple, filter).
--
-- The engine uses these labels to shrink generated values intelligently.

pattern HEGEL_LABEL_LIST :: Word64
pattern HEGEL_LABEL_LIST = (#const HEGEL_LABEL_LIST)

pattern HEGEL_LABEL_LIST_ELEMENT :: Word64
pattern HEGEL_LABEL_LIST_ELEMENT = (#const HEGEL_LABEL_LIST_ELEMENT)

pattern HEGEL_LABEL_SET :: Word64
pattern HEGEL_LABEL_SET = (#const HEGEL_LABEL_SET)

pattern HEGEL_LABEL_SET_ELEMENT :: Word64
pattern HEGEL_LABEL_SET_ELEMENT = (#const HEGEL_LABEL_SET_ELEMENT)

pattern HEGEL_LABEL_MAP :: Word64
pattern HEGEL_LABEL_MAP = (#const HEGEL_LABEL_MAP)

pattern HEGEL_LABEL_MAP_ENTRY :: Word64
pattern HEGEL_LABEL_MAP_ENTRY = (#const HEGEL_LABEL_MAP_ENTRY)

pattern HEGEL_LABEL_TUPLE :: Word64
pattern HEGEL_LABEL_TUPLE = (#const HEGEL_LABEL_TUPLE)

pattern HEGEL_LABEL_ONE_OF :: Word64
pattern HEGEL_LABEL_ONE_OF = (#const HEGEL_LABEL_ONE_OF)

pattern HEGEL_LABEL_OPTIONAL :: Word64
pattern HEGEL_LABEL_OPTIONAL = (#const HEGEL_LABEL_OPTIONAL)

pattern HEGEL_LABEL_FIXED_DICT :: Word64
pattern HEGEL_LABEL_FIXED_DICT = (#const HEGEL_LABEL_FIXED_DICT)

pattern HEGEL_LABEL_FLAT_MAP :: Word64
pattern HEGEL_LABEL_FLAT_MAP = (#const HEGEL_LABEL_FLAT_MAP)

pattern HEGEL_LABEL_FILTER :: Word64
pattern HEGEL_LABEL_FILTER = (#const HEGEL_LABEL_FILTER)

pattern HEGEL_LABEL_MAPPED :: Word64
pattern HEGEL_LABEL_MAPPED = (#const HEGEL_LABEL_MAPPED)

pattern HEGEL_LABEL_SAMPLED_FROM :: Word64
pattern HEGEL_LABEL_SAMPLED_FROM = (#const HEGEL_LABEL_SAMPLED_FROM)

pattern HEGEL_LABEL_ENUM_VARIANT :: Word64
pattern HEGEL_LABEL_ENUM_VARIANT = (#const HEGEL_LABEL_ENUM_VARIANT)

-- $modes
--
-- @CInt@ values passed to 'hegel_settings_mode', which select whether a run
-- executes the full test loop ('HEGEL_MODE_TEST_RUN') or replays a single test
-- case ('HEGEL_MODE_SINGLE_TEST_CASE').

pattern HEGEL_MODE_TEST_RUN :: CInt
pattern HEGEL_MODE_TEST_RUN = (#const HEGEL_MODE_TEST_RUN)

pattern HEGEL_MODE_SINGLE_TEST_CASE :: CInt
pattern HEGEL_MODE_SINGLE_TEST_CASE = (#const HEGEL_MODE_SINGLE_TEST_CASE)

-- $verbosity
--
-- @CInt@ levels passed to 'hegel_settings_verbosity', which control how much
-- diagnostic output the engine emits.

pattern HEGEL_VERBOSITY_QUIET :: CInt
pattern HEGEL_VERBOSITY_QUIET = (#const HEGEL_VERBOSITY_QUIET)

pattern HEGEL_VERBOSITY_NORMAL :: CInt
pattern HEGEL_VERBOSITY_NORMAL = (#const HEGEL_VERBOSITY_NORMAL)

pattern HEGEL_VERBOSITY_VERBOSE :: CInt
pattern HEGEL_VERBOSITY_VERBOSE = (#const HEGEL_VERBOSITY_VERBOSE)

pattern HEGEL_VERBOSITY_DEBUG :: CInt
pattern HEGEL_VERBOSITY_DEBUG = (#const HEGEL_VERBOSITY_DEBUG)

-- $status
--
-- @CInt@ values passed to 'hegel_mark_complete', which report a test case's
-- outcome:
--
-- * valid
-- * invalid (an assumption failed)
-- * overrun (the choice budget was exhausted)
-- * interesting (a failure worth shrinking)

pattern HEGEL_STATUS_VALID :: CInt
pattern HEGEL_STATUS_VALID = (#const HEGEL_STATUS_VALID)

pattern HEGEL_STATUS_INVALID :: CInt
pattern HEGEL_STATUS_INVALID = (#const HEGEL_STATUS_INVALID)

pattern HEGEL_STATUS_OVERRUN :: CInt
pattern HEGEL_STATUS_OVERRUN = (#const HEGEL_STATUS_OVERRUN)

pattern HEGEL_STATUS_INTERESTING :: CInt
pattern HEGEL_STATUS_INTERESTING = (#const HEGEL_STATUS_INTERESTING)

-- $settings
--
-- Allocate, configure, and free a @hegel_settings_t@.
--
-- Setters are marked imported @unsafe@, as they only mutate the handle
-- in-memory and never block.
-- 
-- Prefer 'withSettings'.

foreign import ccall unsafe "hegel_settings_new"
  hegel_settings_new :: IO (Ptr HegelSettings)

foreign import ccall unsafe "hegel_settings_free"
  hegel_settings_free :: Ptr HegelSettings -> IO ()

-- | Set the run mode (full test loop or single test case).
foreign import ccall unsafe "hegel_settings_mode"
  hegel_settings_mode :: Ptr HegelSettings -> CInt -> IO ()

-- | Set the maximum number of valid test cases to run (default: 100).
foreign import ccall unsafe "hegel_settings_test_cases"
  hegel_settings_test_cases :: Ptr HegelSettings -> Word64 -> IO ()

-- | Set engine output verbosity.
foreign import ccall unsafe "hegel_settings_verbosity"
  hegel_settings_verbosity :: Ptr HegelSettings -> CInt -> IO ()

-- | Fix the RNG seed (@has_seed = 1@ to use @seed@, @0@ for fresh).
foreign import ccall unsafe "hegel_settings_seed"
  hegel_settings_seed :: Ptr HegelSettings -> Word64 -> CBool -> IO ()

-- | Derive the seed from a hash of the database key for reproducible CI runs.
foreign import ccall unsafe "hegel_settings_derandomize"
  hegel_settings_derandomize :: Ptr HegelSettings -> CBool -> IO ()

-- | Continue after first failure to surface additional distinct bugs.
foreign import ccall unsafe "hegel_settings_report_multiple_failures"
  hegel_settings_report_multiple_failures :: Ptr HegelSettings -> CBool -> IO ()

-- | Configure the on-disk example database.
--
-- Pass @\"\"@ to disable, @nullPtr@ to leave at the current value.
foreign import ccall unsafe "hegel_settings_database"
  hegel_settings_database :: Ptr HegelSettings -> CString -> IO ()

-- | Set the database key used to scope stored / replayed examples.
foreign import ccall unsafe "hegel_settings_database_key"
  hegel_settings_database_key :: Ptr HegelSettings -> CString -> IO ()

-- | Enable the phases listed in a @HEGEL_PHASE_*@ bitmask (default: all).
foreign import ccall unsafe "hegel_settings_phases"
  hegel_settings_phases :: Ptr HegelSettings -> Word32 -> IO ()

-- | Suppress health checks listed in a @HEGEL_HC_*@ bitmask.
foreign import ccall unsafe "hegel_settings_suppress_health_check"
  hegel_settings_suppress_health_check :: Ptr HegelSettings -> Word32 -> IO ()

-- $run
--
-- Start an engine run from a settings handle, pump test cases out of it,
-- read the aggregated result, and tear it down.
--
-- Blocking calls ('hegel_next_test_case', 'hegel_run_free') are declared @safe@.
--
-- Prefer 'withRun'.

-- | Spawn the engine worker thread and return a run handle; returns
-- immediately, returns @NULL@ on failure (read 'hegel_last_error_message').
--
-- Use 'withRun' rather than calling this directly.
foreign import ccall unsafe "hegel_run_start"
  hegel_run_start :: Ptr HegelSettings -> IO (Ptr HegelRun)

-- | Block until the engine produces the next test case.
--
-- Declared @safe@ so it does not pin a GHC capability while blocked on the
-- Rust worker.
-- 
-- Returns @NULL@ when the run is finished or on error.
foreign import ccall safe "hegel_next_test_case"
  hegel_next_test_case :: Ptr HegelRun -> IO (Ptr HegelTestCase)

-- | Return the aggregated run result, borrowed from the run handle.
-- 
-- Only valid once 'hegel_next_test_case' has returned @NULL@.
foreign import ccall unsafe "hegel_run_result"
  hegel_run_result :: Ptr HegelRun -> IO (Ptr HegelRunResult)

-- | Join the worker thread and free the run handle.
--
-- Imported @safe@ to avoid pinning a capability during the join.
foreign import ccall safe "hegel_run_free"
  hegel_run_free :: Ptr HegelRun -> IO ()

-- $pertestcase
--
-- Operations which are valid only while a 'HegelTestCase' is live:
--
-- * drawing values
-- * opening and closing spans
-- * managing collections and pools
-- * recording targeting observations
-- * marking the case complete.
--
-- All are declared @safe@ because they round-trip to the Rust worker.
--
-- __NOTE__: As mentioned in the module comment's thread-discipline note, these
-- functions __MUST__ be run on a bound thread.

-- | Draw one value using a CBOR-encoded schema.
--
-- Returns 'HEGEL_OK' and writes a /borrowed/ pointer into @*out_value_cbor@;
-- copy before the next @libhegel call@.
--
-- Returns 'HEGEL_E_STOP_TEST' when the choice budget is exhausted, in which
-- case the caller should mark the case 'HEGEL_STATUS_OVERRUN'.
foreign import ccall safe "hegel_generate"
  hegel_generate
    :: Ptr HegelTestCase
    -> Ptr Word8         -- ^ schema CBOR bytes
    -> CSize             -- ^ schema byte length
    -> Ptr (Ptr Word8)   -- ^ out: borrowed value pointer
    -> Ptr CSize         -- ^ out: value byte length
    -> IO CInt

-- | Open a labeled span, where the given @label@ is one of the @HEGEL_LABEL_*@
-- constants.
foreign import ccall safe "hegel_start_span"
  hegel_start_span :: Ptr HegelTestCase -> Word64 -> IO CInt

-- | Close the most-recently-opened span.
--
-- Pass @1@ for @discard@ to mark it rejected (e.g. a filter predicate failed).
foreign import ccall safe "hegel_stop_span"
  hegel_stop_span :: Ptr HegelTestCase -> CBool -> IO CInt

-- | Start an engine-managed variable-length collection.
--
-- Writes the opaque
-- collection ID into @*out_collection_id@.
--
-- Pass @maxBound@ for @max_size@ when unbounded.
foreign import ccall safe "hegel_new_collection"
  hegel_new_collection
    :: Ptr HegelTestCase
    -> Word64    -- ^ @min_size@
    -> Word64    -- ^ @max_size@ (@'maxBound' :: Word64@ for unbounded)
    -> Ptr Int64 -- ^ out: collection ID
    -> IO CInt

-- | Ask whether the engine wants another element; writes the answer into
-- @*out_more@.
foreign import ccall safe "hegel_collection_more"
  hegel_collection_more
    :: Ptr HegelTestCase
    -> Int64      -- ^ @collection_id@
    -> Ptr CBool  -- ^ out: more?
    -> IO CInt

-- | Notify the engine the last element was rejected.
--
-- @why@ may be @NULL@.
foreign import ccall safe "hegel_collection_reject"
  hegel_collection_reject
    :: Ptr HegelTestCase
    -> Int64   -- ^ @collection_id@
    -> CString -- ^ @why@ (optional, may be @NULL@)
    -> IO CInt

-- | Create a new variable pool for stateful testing; writes the pool ID
-- into @*out_pool_id@.
foreign import ccall safe "hegel_new_pool"
  hegel_new_pool :: Ptr HegelTestCase -> Ptr Int64 -> IO CInt

-- | Register a new variable in the pool; writes its ID into
-- @*out_variable_id@.
foreign import ccall safe "hegel_pool_add"
  hegel_pool_add
    :: Ptr HegelTestCase
    -> Int64     -- ^ @pool_id@
    -> Ptr Int64 -- ^ out: @variable_id@
    -> IO CInt

-- | Draw a variable from the pool.
--
-- Returns 'HEGEL_E_STOP_TEST' when the pool is empty.
foreign import ccall safe "hegel_pool_generate"
  hegel_pool_generate
    :: Ptr HegelTestCase
    -> Int64     -- ^ @pool_id@
    -> CBool     -- ^ @consume@ (remove from pool)
    -> Ptr Int64 -- ^ out: @variable_id@
    -> IO CInt

-- | Record a numeric observation for the targeting phase to hill-climb toward.
--
-- @label@ must be non-@NULL@ valid UTF-8.
foreign import ccall safe "hegel_target"
  hegel_target :: Ptr HegelTestCase -> CDouble -> CString -> IO CInt

-- | Mark the test case complete.
--
-- @origin@ is required (non-@NULL@) when @status == 'HEGEL_STATUS_INTERESTING'@.
-- 
-- __NOTE__: @origin@ must be a stable, draw-independent string (e.g. @\"file:line\"@),
-- so the shrinker can converge towards a target.
foreign import ccall safe "hegel_mark_complete"
  hegel_mark_complete
    :: Ptr HegelTestCase
    -> CInt    -- ^ @status@ (@HEGEL_STATUS_*@)
    -> CString -- ^ @origin@ (@NULL@ unless 'HEGEL_STATUS_INTERESTING')
    -> IO CInt

-- | Returns non-zero when this test case is the final minimal replay being
-- shown to the caller after shrinking.
foreign import ccall unsafe "hegel_test_case_is_final_replay"
  hegel_test_case_is_final_replay :: Ptr HegelTestCase -> IO CBool

-- $reproduction
--
-- Build and free a /standalone/ test case that replays a failure blob.
--
-- == Ownership model
--
-- @libhegel@ has two kinds of @hegel_test_case_t@, with different ownership:
--
-- * __Run-owned__: returned by 'hegel_next_test_case'; the 'HegelRun' handle
--   owns the memory, and it is freed by 'hegel_run_free'. Calling 'hegel_test_case_free'
--   on one of these handles is an error and @libhegel@ will loudly reject it
--   with a diagnostic.
--
-- * __Caller-owned__: returned by 'hegel_test_case_from_blob'; the caller is
--   responsible for freeing it with 'hegel_test_case_free'
-- 
-- Prefer 'withTestCaseFromBlob' over these functions wherever possible.

-- | Build a standalone test case that replays the counterexample encoded in
-- @blob@.
--
-- Returns @NULL@ with a diagnostic in 'hegel_last_error_message' when:
--
-- * @s@ or @blob@ is @NULL@
-- * @blob@ is not valid UTF-8
-- * @blob@ is corrupt or from an incompatible @libhegel@ version
--
-- The returned handle is owned by the __caller__ and must be freed with
-- 'hegel_test_case_free'.
--
-- __Do not call 'hegel_mark_complete' on this handle.__ Standalone test cases
-- have no run to report back to; calling 'hegel_mark_complete' on one will
-- trigger a Rust panic and abort the process. The caller-owned lifecycle is
-- strictly: @from_blob@ → draw → inspect → @free@.
--
-- A blob whose choice sequence no longer matches the caller's generators
-- returns 'HEGEL_E_STOP_TEST' on the overrunning draw.
--
-- 'hegel_test_case_is_final_replay' always returns @true@ on this handle.
--
-- Prefer 'withTestCaseFromBlob' over calling this directly.
foreign import ccall unsafe "hegel_test_case_from_blob"
  hegel_test_case_from_blob
    :: Ptr HegelSettings
    -> CString -- ^ @blob@: base64 failure blob from 'hegel_failure_reproduction_blob'
    -> IO (Ptr HegelTestCase)

-- | Free a __caller-owned__ test case returned by 'hegel_test_case_from_blob'.
--
-- __Do not call this on handles owned by 'HegelRun'__ (e.g. those returned by
-- 'hegel_next_test_case'); @libhegel@ will reject the call with a diagnostic
-- error and the handle will be double-freed when the run itself is freed.
-- 
-- Prefer 'withTestCaseFromBlob' to avoid having to guard against this.
foreign import ccall unsafe "hegel_test_case_free"
  hegel_test_case_free :: Ptr HegelTestCase -> IO ()

-- $results
--
-- Read-only accessors over a completed run's result and its individual
-- failures (panic message, diagnostic, origin, reproduction blob).
--
-- Returned strings and failure handles are borrowed and remain valid only
-- until 'hegel_run_free' is called.  To use a reproduction blob beyond the
-- run's lifetime, copy it (e.g. via 'failureReproductionBlob') before calling
-- 'hegel_run_free'.

-- | Returns non-zero if the run passed.
foreign import ccall unsafe "hegel_run_result_passed"
  hegel_run_result_passed :: Ptr HegelRunResult -> IO CBool

-- | Number of distinct failures (by origin) surfaced during the run.
foreign import ccall unsafe "hegel_run_result_failure_count"
  hegel_run_result_failure_count :: Ptr HegelRunResult -> IO CSize

-- | Borrow the @i@-th failure from the result (0-indexed).
foreign import ccall unsafe "hegel_run_result_failure"
  hegel_run_result_failure :: Ptr HegelRunResult -> CSize -> IO (Ptr HegelFailure)

-- | The failure's panic message (owned by the result; valid until 'hegel_run_free').
foreign import ccall unsafe "hegel_failure_panic_message"
  hegel_failure_panic_message :: Ptr HegelFailure -> IO CString

-- | Extended diagnostic string for the failure.
foreign import ccall unsafe "hegel_failure_diagnostic"
  hegel_failure_diagnostic :: Ptr HegelFailure -> IO CString

-- | The stable origin string passed to 'hegel_mark_complete'.
foreign import ccall unsafe "hegel_failure_origin"
  hegel_failure_origin :: Ptr HegelFailure -> IO CString

-- | Retrieve a base64-encoded string with a failing test's choice sequence
-- representing a minimal counterexample; this can be used to deterministically
-- replay test failure via 'hegel_test_case_from_blob'.
--
-- Returns @NULL@ when @f@ is @NULL@ or when the engine produced no blob for
-- this failure (e.g. a health-check failure).
--
-- The returned pointer is __borrowed__ from the parent @hegel_run_result_t@
-- and remains valid only until 'hegel_run_free' is called.
--
-- To preserve the blob beyond the run's lifetime, copy it before freeing the
-- run ('failureReproductionBlob' does this automatically).
foreign import ccall unsafe "hegel_failure_reproduction_blob"
  hegel_failure_reproduction_blob :: Ptr HegelFailure -> IO CString

-- $globals
--
-- Global queries that take no handle, namely the thread-local last-error
-- buffer and the static library version string.

-- | Thread-local error buffer.
--
-- Read /immediately/ after a failing call performed on the OS thread as the
-- test run (see module note on thread discipline).
-- 
-- Returns the empty string (/not/ @NULL@) when the most recent call succeeded.
foreign import ccall unsafe "hegel_last_error_message"
  hegel_last_error_message :: IO CString

-- | Static version string; valid for the program's lifetime.
foreign import ccall unsafe "hegel_version"
  hegel_version :: IO CString

-- $helpers
--
-- More idiomatic Haskell wrappers over the FFI bindings:
--
-- These are the intended entry points for higher-level code.

-- | Read the thread-local @libhegel@ error buffer.
lastErrorMessage :: IO (Maybe Text)
lastErrorMessage = do
  msgPtr <- hegel_last_error_message
  if msgPtr == nullPtr
    then pure Nothing
    else do
      msg <- T.pack <$> peekCString msgPtr
      pure (if T.null msg then Nothing else Just msg)

-- | Check a @libhegel@ return code; throws 'HegelError' on any non-zero
-- value.
--
-- __NOTE__: This /must/ be called on the same OS thread as the failing
-- @libhegel@ call (establish this by running all sequences under
-- 'Control.Concurrent.runInBoundThread').
throwOnError :: CInt -> IO ()
throwOnError rc
  | rc == HEGEL_OK = pure ()
  | otherwise = do
      msg <- lastErrorMessage
      throwIO HegelError {code = rc, message = msg}

-- | Acquire a settings handle, pass it to the action, and free it on exit.
withSettings :: (Ptr HegelSettings -> IO a) -> IO a
withSettings = bracket hegel_settings_new hegel_settings_free

-- | Start a run with the given settings, run the action, then join the
-- worker thread and free the run handle.
--
-- Throws 'HegelStartupError' if 'hegel_run_start' returns @NULL@.
withRun :: Ptr HegelSettings -> (Ptr HegelRun -> IO a) -> IO a
withRun s = bracket acquire hegel_run_free
  where
    acquire = do
      run <- hegel_run_start s
      if run == nullPtr
        then lastErrorMessage >>= throwIO . HegelStartupError
        else pure run

-- | Copy the reproduction blob for @f@ into a fresh 'ByteString', or return
-- 'Nothing' when the failure carries no blob (e.g. a health-check failure) or
-- @f@ is @NULL@.
--
-- The underlying C pointer is borrowed from the run result and only valid
-- until 'hegel_run_free'; this function copies it immediately so the
-- 'ByteString' is safe to use after the run is freed.
--
-- The blob is ASCII base64 and can be passed directly to 'withTestCaseFromBlob'
-- via 'Data.ByteString.useAsCString'.
failureReproductionBlob :: Ptr HegelFailure -> IO (Maybe ByteString)
failureReproductionBlob f = do
  ptr <- hegel_failure_reproduction_blob f
  if ptr == nullPtr
    then pure Nothing
    else Just <$> BS.packCString ptr

-- | Acquire a caller-owned test case that replays the counterexample encoded
-- in @blob@, pass it to @action@, and free it on exit.
--
-- @blob@ must be a base64 string obtained from 'failureReproductionBlob' (or
-- the underlying 'hegel_failure_reproduction_blob').  Throws 'HegelStartupError'
-- when @libhegel@ cannot decode the blob (corrupt data, incompatible version,
-- @NULL@ arguments).
--
-- The test case handle is __caller-owned__: it is freed by this bracket, not
-- by a run.  'hegel_test_case_is_final_replay' always returns @true@ on it.
-- Drive it with 'generate', 'hegel_start_span'\/'hegel_stop_span', etc.
-- Inspect the drawn values to determine whether the failure reproduced.
--
-- __Do not__ call 'hegel_mark_complete' on the handle — there is no run to
-- report back to, and doing so will abort the process via a Rust panic.
-- __Do not__ pass the handle to 'hegel_test_case_free' yourself — the bracket
-- does that.  __Do not__ pass it to 'hegel_run_free' — there is no run.
withTestCaseFromBlob
  :: Ptr HegelSettings
  -> ByteString
  -- ^ Base64 failure blob (e.g. from 'failureReproductionBlob').
  -> (Ptr HegelTestCase -> IO a)
  -> IO a
withTestCaseFromBlob s blob action =
  BS.useAsCString blob $ \blobPtr ->
    bracket (acquire blobPtr) hegel_test_case_free action
  where
    acquire blobPtr = do
      tc <- hegel_test_case_from_blob s blobPtr
      if tc == nullPtr
        then lastErrorMessage >>= throwIO . HegelStartupError
        else pure tc

-- | Draw one value from a test case using the supplied CBOR-encoded schema,
-- returning the engine's response as a freshly-copied 'ByteString'.
--
-- The engine's output buffer is borrowed and invalidated by the next
-- libhegel call on the same test case; this function copies it before
-- returning so the caller does not need to worry about the lifetime.
--
-- Calls 'throwOnError' on the return code.
--
-- Control-flow codes ('HEGEL_E_STOP_TEST', 'HEGEL_E_ASSUME') are converted to
-- 'HegelError's with the corresponding code so callers can branch on them.
generate :: Ptr HegelTestCase -> ByteString -> IO ByteString
generate tc schema =
  BS.useAsCStringLen schema $ \(schemaPtr, schemaLen) ->
    alloca $ \outPtrPtr ->
      alloca $ \outLenPtr -> do
        rc <-
          hegel_generate
            tc
            (castPtr schemaPtr)
            (fromIntegral schemaLen)
            outPtrPtr
            outLenPtr
        throwOnError rc
        valuePtr <- peek outPtrPtr
        valueLen <- peek outLenPtr
        BS.packCStringLen (castPtr valuePtr, fromIntegral valueLen)
