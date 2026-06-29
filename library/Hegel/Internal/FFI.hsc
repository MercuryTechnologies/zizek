-- | Low-level FFI bindings to @libhegel@ (@hegeltest-c@).
--
-- __Internal module.__ Implementation substrate of @zizek@ itself, exposed so
-- you can reach past the public API when you must; it is not part of the
-- stable public interface and may change without notice.
--
-- Every @hegel_*@ function from @hegel.h@ is exposed as a
-- 'foreign import ccall' declaration together with phantom types representing
-- handles to C constructs, error-code pattern synonyms, and bracket helpers.
--
-- __Calling convention__: every @libhegel@ entry point (except
-- 'hegel_context_new' and 'hegel_context_last_error') takes a
-- @hegel_context_t*@ as its first argument and returns a @hegel_result_t@
-- ('HEGEL_OK' is zero; negatives are errors).
--
-- Anything else a call produces (e.g. a handle, a string, a count) is written
-- through a trailing out-parameter.
--
-- __Error reporting__: a failed call records its diagnostic on the
-- caller-supplied 'HegelContext' rather than in thread-local state; read the
-- most recent message with 'hegel_context_last_error' (or 'throwOnError',
-- which does this for you).
--
-- A single context must not be used concurrently from multiple threads, as each
-- fallible call overwrites the stored message.
--
-- The runner drives a whole run from one bound thread (see 'withContext');
-- the blocking 'hegel_next_test_case' is declared @safe@ so it does not pin a
-- capability while parked on the Rust worker.
module Hegel.Internal.FFI
  ( -- * Opaque handle phantoms
    -- $handles
    HegelContext,
    HegelSettings,
    HegelRun,
    HegelTestCase,
    HegelRunResult,
    HegelFailure,

    -- * Error types
    -- $errortypes
    HegelError (..),

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
    pattern HEGEL_LABEL_FEATURE_FLAG,

    -- * Mode pattern synonyms
    -- $modes
    pattern HEGEL_MODE_TEST_RUN,
    pattern HEGEL_MODE_SINGLE_TEST_CASE,

    -- * Backend pattern synonyms
    -- $backend
    pattern HEGEL_BACKEND_AUTO,
    pattern HEGEL_BACKEND_DEFAULT,
    pattern HEGEL_BACKEND_URANDOM,

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

    -- * Run-status pattern synonyms
    -- $runstatus
    pattern HEGEL_RUN_STATUS_PASSED,
    pattern HEGEL_RUN_STATUS_FAILED,
    pattern HEGEL_RUN_STATUS_ERROR,

    -- * Context lifecycle
    -- $context
    hegel_context_new,
    hegel_context_free,
    hegel_context_last_error,

    -- * Settings lifecycle
    -- $settings
    hegel_settings_new,
    hegel_settings_free,
    hegel_settings_set_mode,
    hegel_settings_set_backend,
    hegel_settings_set_test_cases,
    hegel_settings_set_verbosity,
    hegel_settings_set_seed,
    hegel_settings_set_derandomize,
    hegel_settings_set_report_multiple_failures,
    hegel_settings_set_database,
    hegel_settings_set_database_key,
    hegel_settings_set_phases,
    hegel_settings_set_suppress_health_check,

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
    hegel_primitive_boolean,
    hegel_target,
    hegel_mark_complete,

    -- * Failure reproduction
    -- $reproduction
    hegel_test_case_from_blob,
    hegel_test_case_free,

    -- * Result inspection
    -- $results
    hegel_run_result_status,
    hegel_run_result_error,
    hegel_run_result_failure_count,
    hegel_run_result_failure,
    hegel_failure_origin,
    hegel_failure_reproduction_blob,

    -- * Globals
    -- $globals
    hegel_version,

    -- * Haskell helpers
    -- $helpers
    throwOnError,
    peekUtf8,
    withContext,
    withSettings,
    withRun,
    generate,
    failureReproductionBlob,
    withTestCaseFromBlob,
  )
where

#include <hegel.h>

import Control.Exception (Exception, bracket, throwIO)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word32, Word64, Word8)
import Foreign (Ptr, alloca, castPtr, nullPtr, peek)
import Foreign.C.String (CString)
import Foreign.C.Types (CBool (..), CDouble (..), CInt (..), CSize (..))

-- $handles
--
-- Phantom parameters passed to 'Ptr', giving each @libhegel@ handle
-- (e.g. @hegel_settings_t*@, @hegel_run_t*@) a distinct, Haskell type.

-- | Sentinel for @hegel_context_t*@ (error-reporting context).
data HegelContext

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
-- 'HegelError': a @libhegel@ FFI call returned a non-zero @HEGEL_E_*@ code,
-- which callers can branch off of and potentially handle. This includes
-- failures that occur before any test case is produced — constructing a run
-- ('hegel_run_start') or a replay test case ('hegel_test_case_from_blob') — as
-- those calls now report a result code like every other.

-- | Exception thrown when a @libhegel@ call returns a non-zero error code.
data HegelError = HegelError
  { -- | The raw @HEGEL_E_*@ error code.
    code :: !CInt,
    -- | Diagnostic from 'hegel_context_last_error', if any.
    message :: !(Maybe Text)
  }
  deriving stock (Show)

instance Exception HegelError

-- $errorcodes
--
-- @CInt@ return codes shared by every libhegel call.
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
-- @Word32@ flags, OR\'d and passed to 'hegel_settings_set_phases', which
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
-- @Word32@ flags, OR\'d and passed to 'hegel_settings_set_suppress_health_check',
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

pattern HEGEL_LABEL_FEATURE_FLAG :: Word64
pattern HEGEL_LABEL_FEATURE_FLAG = (#const HEGEL_LABEL_FEATURE_FLAG)

-- $modes
--
-- @CInt@ values passed to 'hegel_settings_set_mode', which select whether a run
-- executes the full test loop ('HEGEL_MODE_TEST_RUN') or replays a single test
-- case ('HEGEL_MODE_SINGLE_TEST_CASE').

pattern HEGEL_MODE_TEST_RUN :: CInt
pattern HEGEL_MODE_TEST_RUN = (#const HEGEL_MODE_TEST_RUN)

pattern HEGEL_MODE_SINGLE_TEST_CASE :: CInt
pattern HEGEL_MODE_SINGLE_TEST_CASE = (#const HEGEL_MODE_SINGLE_TEST_CASE)

-- $backend
--
-- @CInt@ values passed to 'hegel_settings_set_backend', which select the
-- engine's source of randomness.

pattern HEGEL_BACKEND_AUTO :: CInt
pattern HEGEL_BACKEND_AUTO = (#const HEGEL_BACKEND_AUTO)

pattern HEGEL_BACKEND_DEFAULT :: CInt
pattern HEGEL_BACKEND_DEFAULT = (#const HEGEL_BACKEND_DEFAULT)

pattern HEGEL_BACKEND_URANDOM :: CInt
pattern HEGEL_BACKEND_URANDOM = (#const HEGEL_BACKEND_URANDOM)

-- $verbosity
--
-- @CInt@ levels passed to 'hegel_settings_set_verbosity', which control how
-- much diagnostic output the engine emits.

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

-- $runstatus
--
-- @CInt@ values written by 'hegel_run_result_status', the aggregate verdict of
-- a finished run:
--
-- * passed (the property held)
-- * failed (the property has counterexamples)
-- * error (the run itself failed and produced no verdict)

pattern HEGEL_RUN_STATUS_PASSED :: CInt
pattern HEGEL_RUN_STATUS_PASSED = (#const HEGEL_RUN_STATUS_PASSED)

pattern HEGEL_RUN_STATUS_FAILED :: CInt
pattern HEGEL_RUN_STATUS_FAILED = (#const HEGEL_RUN_STATUS_FAILED)

pattern HEGEL_RUN_STATUS_ERROR :: CInt
pattern HEGEL_RUN_STATUS_ERROR = (#const HEGEL_RUN_STATUS_ERROR)

-- $context
--
-- Allocate, query, and free a @hegel_context_t@: the error-reporting context
-- threaded through every fallible call.  Prefer 'withContext'.

-- | Allocate a new error-reporting context (never @NULL@).
foreign import ccall unsafe "hegel_context_new"
  hegel_context_new :: IO (Ptr HegelContext)

-- | Free a context; safe to call with @NULL@.
foreign import ccall unsafe "hegel_context_free"
  hegel_context_free :: Ptr HegelContext -> IO CInt

-- | The most recent error message recorded on the context, or the empty
-- string if the most recent call succeeded; @NULL@ only when the context
-- itself is @NULL@.
--
-- The returned pointer borrows the context's buffer and is invalidated by the
-- next libhegel call on the same context — copy the bytes before another call.
foreign import ccall unsafe "hegel_context_last_error"
  hegel_context_last_error :: Ptr HegelContext -> IO CString

-- $settings
--
-- Allocate, configure, and free a @hegel_settings_t@.
--
-- Setters are marked imported @unsafe@, as they only mutate the handle
-- in-memory and never block.
--
-- Prefer 'withSettings'.

-- | Allocate a settings handle initialised with @libhegel@ defaults, writing
-- it into @*out_settings@.
foreign import ccall unsafe "hegel_settings_new"
  hegel_settings_new :: Ptr HegelContext -> Ptr (Ptr HegelSettings) -> IO CInt

-- | Free a settings handle; safe to call with @NULL@.
foreign import ccall unsafe "hegel_settings_free"
  hegel_settings_free :: Ptr HegelContext -> Ptr HegelSettings -> IO CInt

-- | Set the run mode (full test loop or single test case).
foreign import ccall unsafe "hegel_settings_set_mode"
  hegel_settings_set_mode :: Ptr HegelContext -> Ptr HegelSettings -> CInt -> IO CInt

-- | Select the engine's randomness backend (one of the @HEGEL_BACKEND_*@
-- values).
foreign import ccall unsafe "hegel_settings_set_backend"
  hegel_settings_set_backend :: Ptr HegelContext -> Ptr HegelSettings -> CInt -> IO CInt

-- | Set the maximum number of valid test cases to run (default: 100).
foreign import ccall unsafe "hegel_settings_set_test_cases"
  hegel_settings_set_test_cases :: Ptr HegelContext -> Ptr HegelSettings -> Word64 -> IO CInt

-- | Set engine output verbosity.
foreign import ccall unsafe "hegel_settings_set_verbosity"
  hegel_settings_set_verbosity :: Ptr HegelContext -> Ptr HegelSettings -> CInt -> IO CInt

-- | Fix the RNG seed (@has_seed = 1@ to use @seed@, @0@ for fresh).
foreign import ccall unsafe "hegel_settings_set_seed"
  hegel_settings_set_seed :: Ptr HegelContext -> Ptr HegelSettings -> Word64 -> CBool -> IO CInt

-- | Derive the seed from a hash of the database key for reproducible CI runs.
foreign import ccall unsafe "hegel_settings_set_derandomize"
  hegel_settings_set_derandomize :: Ptr HegelContext -> Ptr HegelSettings -> CBool -> IO CInt

-- | Continue after first failure to surface additional distinct bugs.
foreign import ccall unsafe "hegel_settings_set_report_multiple_failures"
  hegel_settings_set_report_multiple_failures :: Ptr HegelContext -> Ptr HegelSettings -> CBool -> IO CInt

-- | Configure the on-disk example database.
--
-- Pass @\"\"@ to disable, @nullPtr@ to leave at the current value.
foreign import ccall unsafe "hegel_settings_set_database"
  hegel_settings_set_database :: Ptr HegelContext -> Ptr HegelSettings -> CString -> IO CInt

-- | Set the database key used to scope stored / replayed examples.
foreign import ccall unsafe "hegel_settings_set_database_key"
  hegel_settings_set_database_key :: Ptr HegelContext -> Ptr HegelSettings -> CString -> IO CInt

-- | Enable the phases listed in a @HEGEL_PHASE_*@ bitmask (default: all).
foreign import ccall unsafe "hegel_settings_set_phases"
  hegel_settings_set_phases :: Ptr HegelContext -> Ptr HegelSettings -> Word32 -> IO CInt

-- | Suppress health checks listed in a @HEGEL_HC_*@ bitmask.
foreign import ccall unsafe "hegel_settings_set_suppress_health_check"
  hegel_settings_set_suppress_health_check :: Ptr HegelContext -> Ptr HegelSettings -> Word32 -> IO CInt

-- $run
--
-- Start an engine run from a settings handle, pump test cases out of it,
-- read the aggregated result, and tear it down.
--
-- Blocking calls ('hegel_next_test_case', 'hegel_run_free') are declared @safe@.
--
-- Prefer 'withRun'.

-- | Spawn the engine worker thread and write a run handle into @*out_run@;
-- returns immediately.
--
-- Use 'withRun' rather than calling this directly.
foreign import ccall unsafe "hegel_run_start"
  hegel_run_start :: Ptr HegelContext -> Ptr HegelSettings -> Ptr (Ptr HegelRun) -> IO CInt

-- | Block until the engine produces the next test case, writing a borrowed
-- handle into @*out_test_case@ (or @NULL@ when the run is finished).
--
-- Declared @safe@ so it does not pin a GHC capability while blocked on the
-- Rust worker.
foreign import ccall safe "hegel_next_test_case"
  hegel_next_test_case :: Ptr HegelContext -> Ptr HegelRun -> Ptr (Ptr HegelTestCase) -> IO CInt

-- | Write the aggregated run result, borrowed from the run handle, into
-- @*out_result@.
--
-- Only valid once 'hegel_next_test_case' has reported completion.
foreign import ccall unsafe "hegel_run_result"
  hegel_run_result :: Ptr HegelContext -> Ptr HegelRun -> Ptr (Ptr HegelRunResult) -> IO CInt

-- | Join the worker thread and free the run handle.
--
-- Imported @safe@ to avoid pinning a capability during the join.
foreign import ccall safe "hegel_run_free"
  hegel_run_free :: Ptr HegelContext -> Ptr HegelRun -> IO CInt

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

-- | Draw one value using a CBOR-encoded schema.
--
-- Returns 'HEGEL_OK' and writes a /borrowed/ pointer into @*out_value_cbor@;
-- copy before the next @libhegel call@.
--
-- Returns 'HEGEL_E_STOP_TEST' when the choice budget is exhausted, in which
-- case the caller should mark the case 'HEGEL_STATUS_OVERRUN'.
foreign import ccall safe "hegel_generate"
  hegel_generate
    :: Ptr HegelContext
    -> Ptr HegelTestCase
    -> Ptr Word8         -- ^ schema CBOR bytes
    -> CSize             -- ^ schema byte length
    -> Ptr (Ptr Word8)   -- ^ out: borrowed value pointer
    -> Ptr CSize         -- ^ out: value byte length
    -> IO CInt

-- | Open a labeled span, where the given @label@ is one of the @HEGEL_LABEL_*@
-- constants.
foreign import ccall safe "hegel_start_span"
  hegel_start_span :: Ptr HegelContext -> Ptr HegelTestCase -> Word64 -> IO CInt

-- | Close the most-recently-opened span.
--
-- Pass @1@ for @discard@ to mark it rejected (e.g. a filter predicate failed).
foreign import ccall safe "hegel_stop_span"
  hegel_stop_span :: Ptr HegelContext -> Ptr HegelTestCase -> CBool -> IO CInt

-- | Start an engine-managed variable-length collection.
--
-- Writes the opaque
-- collection ID into @*out_collection_id@.
--
-- Pass @maxBound@ for @max_size@ when unbounded.
foreign import ccall safe "hegel_new_collection"
  hegel_new_collection
    :: Ptr HegelContext
    -> Ptr HegelTestCase
    -> Word64    -- ^ @min_size@
    -> Word64    -- ^ @max_size@ (@'maxBound' :: Word64@ for unbounded)
    -> Ptr Int64 -- ^ out: collection ID
    -> IO CInt

-- | Ask whether the engine wants another element; writes the answer into
-- @*out_more@.
foreign import ccall safe "hegel_collection_more"
  hegel_collection_more
    :: Ptr HegelContext
    -> Ptr HegelTestCase
    -> Int64      -- ^ @collection_id@
    -> Ptr CBool  -- ^ out: more?
    -> IO CInt

-- | Notify the engine the last element was rejected.
--
-- @why@ may be @NULL@.
foreign import ccall safe "hegel_collection_reject"
  hegel_collection_reject
    :: Ptr HegelContext
    -> Ptr HegelTestCase
    -> Int64   -- ^ @collection_id@
    -> CString -- ^ @why@ (optional, may be @NULL@)
    -> IO CInt

-- | Create a new variable pool for stateful testing; writes the pool ID
-- into @*out_pool_id@.
foreign import ccall safe "hegel_new_pool"
  hegel_new_pool :: Ptr HegelContext -> Ptr HegelTestCase -> Ptr Int64 -> IO CInt

-- | Register a new variable in the pool; writes its ID into
-- @*out_variable_id@.
foreign import ccall safe "hegel_pool_add"
  hegel_pool_add
    :: Ptr HegelContext
    -> Ptr HegelTestCase
    -> Int64     -- ^ @pool_id@
    -> Ptr Int64 -- ^ out: @variable_id@
    -> IO CInt

-- | Draw a variable from the pool.
--
-- Returns 'HEGEL_E_STOP_TEST' when the pool is empty.
foreign import ccall safe "hegel_pool_generate"
  hegel_pool_generate
    :: Ptr HegelContext
    -> Ptr HegelTestCase
    -> Int64     -- ^ @pool_id@
    -> CBool     -- ^ @consume@ (remove from pool)
    -> Ptr Int64 -- ^ out: @variable_id@
    -> IO CInt

-- | Draw a single boolean that is @true@ with probability @p@ (in @[0,1]@).
--
-- The @forced@ / @has_forced@ parameters (used by the engine to pin a draw for
-- replay and shrinking) are part of the C ABI but unused here: callers always
-- pass @has_forced = 0@.
--
-- Returns 'HEGEL_E_STOP_TEST' when the choice budget is exhausted.
foreign import ccall safe "hegel_primitive_boolean"
  hegel_primitive_boolean
    :: Ptr HegelContext
    -> Ptr HegelTestCase
    -> CDouble   -- ^ @p@ (probability of @true@, in @[0,1]@)
    -> CBool     -- ^ @forced@ value (used only when @has_forced@ is set)
    -> CBool     -- ^ @has_forced@
    -> Ptr CBool -- ^ out: drawn value
    -> IO CInt

-- | Record a numeric observation for the targeting phase to hill-climb toward.
--
-- @label@ must be non-@NULL@ valid UTF-8.
foreign import ccall safe "hegel_target"
  hegel_target :: Ptr HegelContext -> Ptr HegelTestCase -> CDouble -> CString -> IO CInt

-- | Mark the test case complete.
--
-- @origin@ is required (non-@NULL@) when @status == 'HEGEL_STATUS_INTERESTING'@.
--
-- __NOTE__: @origin@ must be a stable, draw-independent string (e.g. @\"file:line\"@),
-- so the shrinker can converge towards a target.
foreign import ccall safe "hegel_mark_complete"
  hegel_mark_complete
    :: Ptr HegelContext
    -> Ptr HegelTestCase
    -> CInt    -- ^ @status@ (@HEGEL_STATUS_*@)
    -> CString -- ^ @origin@ (@NULL@ unless 'HEGEL_STATUS_INTERESTING')
    -> IO CInt

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
-- @blob@, writing the caller-owned handle into @*out_test_case@.
--
-- Returns 'HEGEL_E_INVALID_ARG' (with a diagnostic in
-- 'hegel_context_last_error') when @blob@ is @NULL@, not valid UTF-8, corrupt,
-- or from an incompatible @libhegel@ version.
--
-- Caller-owned: free with 'hegel_test_case_free'.
--
-- A blob whose choice sequence no longer matches the caller's generators
-- returns 'HEGEL_E_STOP_TEST' on the overrunning draw.
foreign import ccall unsafe "hegel_test_case_from_blob"
  hegel_test_case_from_blob
    :: Ptr HegelContext
    -> Ptr HegelSettings
    -> CString -- ^ @blob@: base64 failure blob from 'hegel_failure_reproduction_blob'
    -> Ptr (Ptr HegelTestCase) -- ^ out: caller-owned test case
    -> IO CInt

-- | Free a __caller-owned__ test case returned by 'hegel_test_case_from_blob'.
--
-- __Do not call this on 'HegelRun'-owned handles__ (e.g. from
-- 'hegel_next_test_case'); @libhegel@ rejects it with 'HEGEL_E_INVALID_HANDLE'.
foreign import ccall unsafe "hegel_test_case_free"
  hegel_test_case_free :: Ptr HegelContext -> Ptr HegelTestCase -> IO CInt

-- $results
--
-- Read-only accessors over a completed run's result and its individual
-- failures (aggregate status, run-level error, origin, reproduction blob).
--
-- Returned strings and failure handles are borrowed and remain valid only
-- until 'hegel_run_free' is called.  To use a reproduction blob beyond the
-- run's lifetime, copy it (e.g. via 'failureReproductionBlob') before calling
-- 'hegel_run_free'.

-- | Write the run's aggregate status (one of the @HEGEL_RUN_STATUS_*@ values)
-- into @*out_status@.
foreign import ccall unsafe "hegel_run_result_status"
  hegel_run_result_status :: Ptr HegelContext -> Ptr HegelRunResult -> Ptr CInt -> IO CInt

-- | Write the run-level error message into @*out_error@ (or @NULL@ when the
-- run completed normally rather than erroring). Valid until 'hegel_run_free'.
foreign import ccall unsafe "hegel_run_result_error"
  hegel_run_result_error :: Ptr HegelContext -> Ptr HegelRunResult -> Ptr CString -> IO CInt

-- | Write the number of distinct failures (by origin) into @*out_count@.
foreign import ccall unsafe "hegel_run_result_failure_count"
  hegel_run_result_failure_count :: Ptr HegelContext -> Ptr HegelRunResult -> Ptr CSize -> IO CInt

-- | Write a borrowed pointer to the @i@-th failure (0-indexed) into
-- @*out_failure@ (or @NULL@ when out of range).
foreign import ccall unsafe "hegel_run_result_failure"
  hegel_run_result_failure :: Ptr HegelContext -> Ptr HegelRunResult -> CSize -> Ptr (Ptr HegelFailure) -> IO CInt

-- | Write the stable origin string passed to 'hegel_mark_complete' into
-- @*out_origin@.
foreign import ccall unsafe "hegel_failure_origin"
  hegel_failure_origin :: Ptr HegelContext -> Ptr HegelFailure -> Ptr CString -> IO CInt

-- | Write a base64-encoded string with a failing test's choice sequence
-- (a minimal counterexample) into @*out_blob@; this can be used to
-- deterministically replay the failure via 'hegel_test_case_from_blob'.
--
-- Writes @NULL@ when the engine produced no blob for this failure (e.g. a
-- health-check failure).
--
-- The written pointer is __borrowed__ from the parent @hegel_run_result_t@
-- and remains valid only until 'hegel_run_free' is called.
--
-- To preserve the blob beyond the run's lifetime, copy it before freeing the
-- run ('failureReproductionBlob' does this automatically).
foreign import ccall unsafe "hegel_failure_reproduction_blob"
  hegel_failure_reproduction_blob :: Ptr HegelContext -> Ptr HegelFailure -> Ptr CString -> IO CInt

-- $globals
--
-- Global queries: the static library version string.

-- | Write the static version string into @*out_version@; valid for the
-- program's lifetime.
foreign import ccall unsafe "hegel_version"
  hegel_version :: Ptr HegelContext -> Ptr CString -> IO CInt

-- $helpers
--
-- Idiomatic Haskell wrappers over the FFI bindings.

-- | Read the @libhegel@ error buffer recorded on the context.
lastErrorMessage :: Ptr HegelContext -> IO (Maybe Text)
lastErrorMessage ctx = do
  msgPtr <- hegel_context_last_error ctx
  if msgPtr == nullPtr
    then pure Nothing
    else do
      msg <- peekUtf8 msgPtr
      pure (if T.null msg then Nothing else Just msg)

-- | Decode a borrowed UTF-8 C string from @libhegel@.
--
-- @libhegel@ strings (origins, diagnostics, error messages) are UTF-8
-- regardless of the process locale, so decode them explicitly rather than via
-- the locale-sensitive 'Foreign.C.String.peekCString'.
--
-- Decoding is lenient: a malformed byte becomes U+FFFD, so reporting a failure
-- can never itself crash the runner.
--
-- A @NULL@ pointer decodes to @\"\"@.
peekUtf8 :: CString -> IO Text
peekUtf8 p
  | p == nullPtr = pure ""
  | otherwise = TE.decodeUtf8Lenient <$> BS.packCString p

-- | Check a @libhegel@ return code; throws 'HegelError' on any non-zero
-- value, attaching the diagnostic recorded on the context.
throwOnError :: Ptr HegelContext -> CInt -> IO ()
throwOnError ctx rc
  | rc == HEGEL_OK = pure ()
  | otherwise = do
      msg <- lastErrorMessage ctx
      throwIO HegelError {code = rc, message = msg}

-- | Acquire an error-reporting context, pass it to the action, and free it on
-- exit.
withContext :: (Ptr HegelContext -> IO a) -> IO a
withContext = bracket hegel_context_new (void . hegel_context_free)

-- | Acquire a settings handle, pass it to the action, and free it on exit.
withSettings :: Ptr HegelContext -> (Ptr HegelSettings -> IO a) -> IO a
withSettings ctx = bracket acquire release
  where
    acquire = alloca $ \out -> do
      throwOnError ctx =<< hegel_settings_new ctx out
      peek out
    release s = void (hegel_settings_free ctx s)

-- | Start a run with the given settings, run the action, then join the
-- worker thread and free the run handle.
--
-- Throws 'HegelError' if the engine fails to start.
withRun :: Ptr HegelContext -> Ptr HegelSettings -> (Ptr HegelRun -> IO a) -> IO a
withRun ctx s = bracket acquire release
  where
    acquire = alloca $ \out -> do
      rc <- hegel_run_start ctx s out
      if rc == HEGEL_OK
        then peek out
        else lastErrorMessage ctx >>= \msg -> throwIO HegelError {code = rc, message = msg}
    release run = void (hegel_run_free ctx run)

-- | Copy the reproduction blob for @f@ into a fresh 'ByteString', or return
-- 'Nothing' when the failure carries no blob (e.g. a health-check failure).
--
-- The underlying C pointer is borrowed from the run result and only valid
-- until 'hegel_run_free'; this function copies it immediately so the
-- 'ByteString' is safe to use after the run is freed.
--
-- The blob is ASCII base64 and can be passed directly to 'withTestCaseFromBlob'
-- via 'Data.ByteString.useAsCString'.
failureReproductionBlob :: Ptr HegelContext -> Ptr HegelFailure -> IO (Maybe ByteString)
failureReproductionBlob ctx f =
  alloca $ \out -> do
    throwOnError ctx =<< hegel_failure_reproduction_blob ctx f out
    ptr <- peek out
    if ptr == nullPtr
      then pure Nothing
      else Just <$> BS.packCString ptr

-- | Acquire a caller-owned test case that replays the counterexample encoded
-- in @blob@, pass it to @action@, and free it on exit.
--
-- @blob@ must be a base64 string obtained from 'failureReproductionBlob' (or
-- the underlying 'hegel_failure_reproduction_blob').
--
-- Throws 'HegelError' when @libhegel@ cannot decode the blob.
-- 
-- The bracket frees the handle. __Do not call 'hegel_test_case_free' or
-- 'hegel_run_free' on the handle.__
withTestCaseFromBlob
  :: Ptr HegelContext
  -> Ptr HegelSettings
  -> ByteString
  -- ^ Base64 failure blob (e.g. from 'failureReproductionBlob').
  -> (Ptr HegelTestCase -> IO a)
  -> IO a
withTestCaseFromBlob ctx s blob action =
  BS.useAsCString blob $ \blobPtr ->
    bracket (acquire blobPtr) release action
  where
    acquire blobPtr = alloca $ \out -> do
      rc <- hegel_test_case_from_blob ctx s blobPtr out
      if rc == HEGEL_OK
        then peek out
        else lastErrorMessage ctx >>= \msg -> throwIO HegelError {code = rc, message = msg}
    release tc = void (hegel_test_case_free ctx tc)

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
generate :: Ptr HegelContext -> Ptr HegelTestCase -> ByteString -> IO ByteString
generate ctx tc schema =
  BS.useAsCStringLen schema $ \(schemaPtr, schemaLen) ->
    alloca $ \outPtrPtr ->
      alloca $ \outLenPtr -> do
        rc <-
          hegel_generate
            ctx
            tc
            (castPtr schemaPtr)
            (fromIntegral schemaLen)
            outPtrPtr
            outLenPtr
        throwOnError ctx rc
        valuePtr <- peek outPtrPtr
        valueLen <- peek outLenPtr
        BS.packCStringLen (castPtr valuePtr, fromIntegral valueLen)
