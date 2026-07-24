{-# LANGUAGE CPP #-}

-- | Low-level FFI bindings to @libhegel@ (@hegeltest-c@).
--
-- Most @hegel_*@ functions from @hegel.h@ are exposed as a
-- 'foreign import ccall' declaration together with phantom types representing
-- handles to C constructs, error-code pattern synonyms, and bracket helpers.
--
-- Not yet bound: @hegel_generate_date@\/@_time@\/@_datetime@,
-- @hegel_generate_ipv4@\/@_ipv6@, and @hegel_string_generator_email@.
--
-- __Calling convention__: every @libhegel@ entry point takes a
-- @hegel_context_t*@ as its first argument and returns a @hegel_result_t@,
-- where 'HEGEL_OK' is zero and negatives are errors.
--
-- 'hegel_context_new' and 'hegel_context_last_error' are the exceptions.
--
-- Anything else a call produces is written through a trailing out-parameter.
--
-- __Error reporting__: a failed call records its diagnostic on the
-- caller-supplied 'HegelContext' rather than in thread-local state.
--
-- Read the most recent message with 'hegel_context_last_error', or let
-- 'throwOnError' read and raise it for you.
--
-- A single context must not be used concurrently from multiple threads, as each
-- fallible call overwrites the stored message.
--
-- The runner drives a whole run from one bound thread (see 'withContext').
module Hegel.Internal.Foreign.Raw
  ( -- * Opaque handle phantoms
    -- $handles
    HegelContext,
    HegelSettings,
    HegelRun,
    HegelTestCase,
    HegelRunResult,
    HegelFailure,
    HegelStringGenerator,

    -- * Typed-draw result structs
    -- $typedrawresults
    HegelBytesResult (..),
    HegelStringResult (..),

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
    pattern HEGEL_E_CONCURRENT_USE,

    -- * State-machine termination sentinel
    -- $statemachine
    pattern HEGEL_STATE_MACHINE_DONE,

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
    OutputSink,
    hegel_run_start,
    hegel_next_test_case,
    hegel_run_result,
    hegel_run_result_free,
    hegel_run_free,

    -- * Per-test-case primitives
    -- $pertestcase
    hegel_start_span,
    hegel_stop_span,
    hegel_new_collection,
    hegel_collection_more,
    hegel_collection_reject,
    hegel_new_pool,
    hegel_pool_add,
    hegel_pool_generate,
    hegel_new_state_machine,
    hegel_state_machine_next_rule,
    hegel_target,
    hegel_mark_complete,
    hegel_test_case_clone,

    -- * Typed draws
    -- $typeddraws
    hegel_generate_boolean,
    hegel_generate_integer,
    hegel_generate_integer_big,
    hegel_generate_float,
    hegel_generate_bytes,
    hegel_generate_bytes_result_free,
    hegel_generate_uuid,
    hegel_string_generator_text,
    hegel_string_generator_regex,
    hegel_string_generator_url,
    hegel_string_generator_domain,
    hegel_string_generator_free,
    hegel_generate_string,
    hegel_generate_string_result_free,

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
    hegel_failure_free,
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
    Slot,
    newSlot,
    withSlotOf,
    withSlotBytes,
    failureReproductionBlob,
    withTestCaseFromBlob,
  )
where

#include <hegel.h>

import Control.Exception (Exception (..), bracket, throwIO)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word32, Word64, Word8)
import Foreign (ForeignPtr, FunPtr, Ptr, Storable (..), alloca, castPtr, mallocForeignPtrBytes, nullFunPtr, nullPtr, withForeignPtr)
import Foreign.C.String (CString)
import Foreign.C.Types (CBool (..), CDouble (..), CInt (..), CSize (..))

-- $handles
--
-- Phantom parameters passed to 'Ptr', giving each @libhegel@ handle
-- (e.g. @hegel_settings_t*@, @hegel_run_t*@) a distinct, Haskell type.

-- | Marker type for @hegel_context_t*@ (error-reporting context).
data HegelContext

-- | Marker type for @hegel_settings_t*@.
data HegelSettings

-- | Marker type for @hegel_run_t*@.
data HegelRun

-- | Marker type for @hegel_test_case_t*@.
data HegelTestCase

-- | Marker type for @hegel_run_result_t*@.
data HegelRunResult

-- | Marker type for @hegel_failure_t*@.
data HegelFailure

-- | Marker type for @hegel_string_generator_t*@: a caller-owned, immutable
-- handle describing a string draw (text\/regex\/email\/url\/domain), built by
-- a 'hegel_string_generator_text'-family constructor and freed with
-- 'hegel_string_generator_free'.
data HegelStringGenerator

-- $typedrawresults
--
-- Caller-owned buffers filled in by 'hegel_generate_bytes' \/
-- 'hegel_generate_string'. Both are @{ pointer, length }@ pairs: the byte
-- buffer is never NUL-terminated (a text buffer may contain interior NULs —
-- the drawn alphabet can include U+0000), so always read exactly 'len' bytes
-- and never treat 'CString'\/'Ptr' 'Word8' fields as C strings. Each must be
-- released exactly once with its matching @_result_free@, which also zeroes
-- the struct in place.

-- | Mirrors @hegel_generate_bytes_result_t@.
data HegelBytesResult = HegelBytesResult
  { resultData :: !(Ptr Word8),
    resultLen :: !CSize
  }

instance Storable HegelBytesResult where
  sizeOf _ = (#size hegel_generate_bytes_result_t)
  alignment _ = (#alignment hegel_generate_bytes_result_t)
  peek p =
    HegelBytesResult
      <$> (#peek hegel_generate_bytes_result_t, data) p
      <*> (#peek hegel_generate_bytes_result_t, len) p
  poke p v = do
    (#poke hegel_generate_bytes_result_t, data) p v.resultData
    (#poke hegel_generate_bytes_result_t, len) p v.resultLen

-- | Mirrors @hegel_generate_string_result_t@. The buffer is UTF-8 (per the
-- engine's guarantee), not NUL-terminated; decode exactly 'resultLen' bytes.
data HegelStringResult = HegelStringResult
  { resultData :: !CString,
    resultLen :: !CSize
  }

instance Storable HegelStringResult where
  sizeOf _ = (#size hegel_generate_string_result_t)
  alignment _ = (#alignment hegel_generate_string_result_t)
  peek p =
    HegelStringResult
      <$> (#peek hegel_generate_string_result_t, data) p
      <*> (#peek hegel_generate_string_result_t, len) p
  poke p v = do
    (#poke hegel_generate_string_result_t, data) p v.resultData
    (#poke hegel_generate_string_result_t, len) p v.resultLen

-- $errortypes
--
-- 'HegelError' covers failures that occur before any test case is produced —
-- constructing a run ('hegel_run_start') or a replay test case
-- ('hegel_test_case_from_blob') — as well as per-call errors.

-- | Exception thrown when a @libhegel@ call returns a non-zero error code.
data HegelError = HegelError
  { -- | The raw @HEGEL_E_*@ error code.
    code :: !CInt,
    -- | Diagnostic from 'hegel_context_last_error', if any.
    message :: !(Maybe Text)
  }
  deriving stock (Show)

instance Exception HegelError where
#if __GLASGOW_HASKELL__ >= 912
  -- The control-flow codes arrive here first, so this is thrown on every
  -- stop and discard. The diagnostic that gets rendered is the engine's
  -- message, so a backtrace would never be seen.
  backtraceDesired _ = False
#endif

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
--
-- 'HEGEL_E_CONCURRENT_USE' is returned by the draw primitives when one
-- @hegel_test_case_t*@ handle is driven from two threads at once.
--
-- This library drives every test case from a single bound thread, so it never
-- triggers, yet the closed-world guard in @cbits/wire_enum_guard.c@ still
-- checks it against @hegel.h@.

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

pattern HEGEL_E_CONCURRENT_USE :: CInt
pattern HEGEL_E_CONCURRENT_USE = (#const HEGEL_E_CONCURRENT_USE)

-- $statemachine
--
-- The value 'hegel_state_machine_next_rule' writes into its @out_rule_index@
-- out-parameter to signal that a state machine has finished stepping, rather
-- than naming a rule to run. It shares that function's @int64_t@ domain
-- rather than the @hegel_result_t@ domain the error-code synonyms above cover,
-- since it is not a return code at all.

pattern HEGEL_STATE_MACHINE_DONE :: Int64
pattern HEGEL_STATE_MACHINE_DONE = (#const HEGEL_STATE_MACHINE_DONE)

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
--
-- Only the 16 values mirrored by 'Hegel.Internal.DataSource.Label' have a
-- synonym here: the client-side spans this library itself opens.
--
-- Labels 17 through 30 are spans the engine emits internally around its own
-- typed-draw primitives, so nothing here constructs or matches them.

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
-- @uint32_t@ values passed to 'hegel_settings_set_mode', which select whether a
-- run executes the full test loop ('HEGEL_MODE_TEST_RUN') or replays a single
-- test case ('HEGEL_MODE_SINGLE_TEST_CASE').

pattern HEGEL_MODE_TEST_RUN :: Word32
pattern HEGEL_MODE_TEST_RUN = (#const HEGEL_MODE_TEST_RUN)

pattern HEGEL_MODE_SINGLE_TEST_CASE :: Word32
pattern HEGEL_MODE_SINGLE_TEST_CASE = (#const HEGEL_MODE_SINGLE_TEST_CASE)

-- $backend
--
-- @uint32_t@ values passed to 'hegel_settings_set_backend', which select the
-- engine's source of randomness.

pattern HEGEL_BACKEND_AUTO :: Word32
pattern HEGEL_BACKEND_AUTO = (#const HEGEL_BACKEND_AUTO)

pattern HEGEL_BACKEND_DEFAULT :: Word32
pattern HEGEL_BACKEND_DEFAULT = (#const HEGEL_BACKEND_DEFAULT)

pattern HEGEL_BACKEND_URANDOM :: Word32
pattern HEGEL_BACKEND_URANDOM = (#const HEGEL_BACKEND_URANDOM)

-- $verbosity
--
-- @uint32_t@ levels passed to 'hegel_settings_set_verbosity', which control how
-- much diagnostic output the engine emits.

pattern HEGEL_VERBOSITY_QUIET :: Word32
pattern HEGEL_VERBOSITY_QUIET = (#const HEGEL_VERBOSITY_QUIET)

pattern HEGEL_VERBOSITY_NORMAL :: Word32
pattern HEGEL_VERBOSITY_NORMAL = (#const HEGEL_VERBOSITY_NORMAL)

pattern HEGEL_VERBOSITY_VERBOSE :: Word32
pattern HEGEL_VERBOSITY_VERBOSE = (#const HEGEL_VERBOSITY_VERBOSE)

pattern HEGEL_VERBOSITY_DEBUG :: Word32
pattern HEGEL_VERBOSITY_DEBUG = (#const HEGEL_VERBOSITY_DEBUG)

-- $status
--
-- @uint32_t@ values passed to 'hegel_mark_complete', which report a test case's
-- outcome:
--
-- * valid
-- * invalid (an assumption failed)
-- * overrun (the choice budget was exhausted)
-- * interesting (a failure worth shrinking)

pattern HEGEL_STATUS_VALID :: Word32
pattern HEGEL_STATUS_VALID = (#const HEGEL_STATUS_VALID)

pattern HEGEL_STATUS_INVALID :: Word32
pattern HEGEL_STATUS_INVALID = (#const HEGEL_STATUS_INVALID)

pattern HEGEL_STATUS_OVERRUN :: Word32
pattern HEGEL_STATUS_OVERRUN = (#const HEGEL_STATUS_OVERRUN)

pattern HEGEL_STATUS_INTERESTING :: Word32
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

-- | Allocate a settings handle initialized with @libhegel@ defaults, writing
-- it into @*out_settings@.
foreign import ccall unsafe "hegel_settings_new"
  hegel_settings_new :: Ptr HegelContext -> Ptr (Ptr HegelSettings) -> IO CInt

-- | Free a settings handle; safe to call with @NULL@.
foreign import ccall unsafe "hegel_settings_free"
  hegel_settings_free :: Ptr HegelContext -> Ptr HegelSettings -> IO CInt

-- | Set the run mode (full test loop or single test case).
foreign import ccall unsafe "hegel_settings_set_mode"
  hegel_settings_set_mode :: Ptr HegelContext -> Ptr HegelSettings -> Word32 -> IO CInt

-- | Select the engine's randomness backend (one of the @HEGEL_BACKEND_*@
-- values).
foreign import ccall unsafe "hegel_settings_set_backend"
  hegel_settings_set_backend :: Ptr HegelContext -> Ptr HegelSettings -> Word32 -> IO CInt

-- | Set the maximum number of valid test cases to run (default: 100).
foreign import ccall unsafe "hegel_settings_set_test_cases"
  hegel_settings_set_test_cases :: Ptr HegelContext -> Ptr HegelSettings -> Word64 -> IO CInt

-- | Set engine output verbosity.
foreign import ccall unsafe "hegel_settings_set_verbosity"
  hegel_settings_set_verbosity :: Ptr HegelContext -> Ptr HegelSettings -> Word32 -> IO CInt

-- | Fix the RNG seed (@has_seed = 1@ to use @seed@, @0@ for fresh).
foreign import ccall unsafe "hegel_settings_set_seed"
  hegel_settings_set_seed :: Ptr HegelContext -> Ptr HegelSettings -> Word64 -> CBool -> IO CInt

-- | When no explicit seed is set, derive it from a hash of the database key
-- for reproducible CI runs.
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
-- Prefer 'withRun'.

-- | The Haskell side of a @hegel_output_callback_t@: one line of engine output
-- per call, with the @user_data@ pointer threaded through verbatim.
--
-- @line@ is NUL-terminated UTF-8 whose 'CSize' length excludes the terminator,
-- has no trailing newline, and is valid only for the duration of the call.
--
-- The callback fires synchronously on whichever thread calls
-- 'hegel_next_test_case', the same thread a blob replay emits on. A sink
-- shared across concurrent 'check' calls may still want thread-safety for
-- its own reasons, but the engine itself no longer imposes it.
--
-- Currently always @NULL@ (output stays on stderr).
type OutputSink = Ptr () -> CString -> CSize -> IO ()

-- | Build the run and write a handle into @*out_run@; returns immediately. No
-- test case is generated until the first 'hegel_next_test_case' call.
-- @callback@ (with its @user_data@) receives engine output line by line; a
-- @NULL@ 'FunPtr' leaves output on stderr.
foreign import ccall unsafe "hegel_run_start"
  hegel_run_start
    :: Ptr HegelContext
    -> Ptr HegelSettings
    -> FunPtr OutputSink
    -> Ptr ()
    -> Ptr (Ptr HegelRun)
    -> IO CInt

-- | Run the engine inline on the calling thread up through the next test
-- case, writing a __caller-owned__ handle into @*out_test_case@ (or @NULL@
-- when the run is finished).
--
-- The caller must release the handle with 'hegel_test_case_free'.
--
-- Declared @safe@ since a call can take a full generation, mutation, or
-- shrink step; an @unsafe@ call would pin the calling capability for that
-- whole duration.
foreign import ccall safe "hegel_next_test_case"
  hegel_next_test_case :: Ptr HegelContext -> Ptr HegelRun -> Ptr (Ptr HegelTestCase) -> IO CInt

-- | Write a __caller-owned__ snapshot of the aggregated run result into
-- @*out_result@. The snapshot is independent of the run — it stays valid after
-- 'hegel_run_free' — and must be released with 'hegel_run_result_free'.
--
-- Only valid once 'hegel_next_test_case' has reported completion.
foreign import ccall unsafe "hegel_run_result"
  hegel_run_result :: Ptr HegelContext -> Ptr HegelRun -> Ptr (Ptr HegelRunResult) -> IO CInt

-- | Release a run-result snapshot from 'hegel_run_result', along with the
-- strings read off it.
--
-- Safe to call with @NULL@.
foreign import ccall unsafe "hegel_run_result_free"
  hegel_run_result_free :: Ptr HegelContext -> Ptr HegelRunResult -> IO CInt

-- | Free the run handle. Dropping a run mid-flight simply drops the rest of
-- its exploration; there is no worker to wind down.
--
-- Imported @safe@: on the early-exit path this may synchronously complete an
-- in-flight test case, which would otherwise pin the calling capability.
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
-- * marking the case complete
-- * cloning onto an independent stream
--
-- Nearly all are declared @unsafe@. Despite the request/reply framing, they
-- execute inline on the calling thread, never call back into Haskell, and
-- never touch disk. The engine does not run at all between
-- 'hegel_next_test_case' calls, so per-case calls never contend with the
-- database persistence and shrink bookkeeping those calls do. Each handle's
-- mutex is held only by the one thread driving that handle, so under that
-- one-thread-per-handle discipline it is never actually contended. That holds
-- whether a family is just the single handle 'hegel_next_test_case' hands out
-- or several more cloned off it.
--
-- The @safe@-call ceremony was a measurable fraction of the ~1.1µs per-draw
-- floor, since a capability release and reacquire costs on the order of a
-- tenth of a microsecond.
--
-- Cloning and completing are the two operations that reach across a family's
-- handles, and neither blocks on that account. 'hegel_test_case_clone' fails
-- fast with 'HEGEL_E_CONCURRENT_USE' rather than waiting when the source
-- handle is genuinely driven from two threads at once, and a family's
-- completion race resolves through a lock-free atomic rather than a lock.
--
-- Caveat, accepted: under the urandom backend (explicit 'HEGEL_BACKEND_URANDOM',
-- or auto-selected inside Antithesis) every entropy-consuming call reads
-- @\/dev\/urandom@ inside the call, pinning the capability for the syscall
-- pair. @\/dev\/urandom@ never blocks, the pin is µs-scale, and Antithesis
-- runs are not latency-sensitive.
--
-- One exception: 'hegel_generate_string' is @safe@. See its own haddock —
-- a regex-backed draw's bounded retry-on-failed-lookaround loop is a
-- meaningfully different (still bounded, just not µs-scale) cost profile
-- than every other draw here, which are each a single RNG-consuming
-- operation.

-- | Open a labeled span, where the given @label@ is one of the @HEGEL_LABEL_*@
-- constants.
foreign import ccall unsafe "hegel_start_span"
  hegel_start_span :: Ptr HegelContext -> Ptr HegelTestCase -> Word64 -> IO CInt

-- | Close the most-recently-opened span.
--
-- Pass @1@ for @discard@ to mark it rejected (e.g. a filter predicate failed).
foreign import ccall unsafe "hegel_stop_span"
  hegel_stop_span :: Ptr HegelContext -> Ptr HegelTestCase -> CBool -> IO CInt

-- | Start an engine-managed variable-length collection.
--
-- Writes the opaque
-- collection ID into @*out_collection_id@.
--
-- Pass @maxBound@ for @max_size@ when unbounded.
foreign import ccall unsafe "hegel_new_collection"
  hegel_new_collection
    :: Ptr HegelContext
    -> Ptr HegelTestCase
    -> Word64    -- ^ @min_size@
    -> Word64    -- ^ @max_size@ (@'maxBound' :: Word64@ for unbounded)
    -> Ptr Int64 -- ^ out: collection ID
    -> IO CInt

-- | Ask whether the engine wants another element; writes the answer into
-- @*out_more@.
foreign import ccall unsafe "hegel_collection_more"
  hegel_collection_more
    :: Ptr HegelContext
    -> Ptr HegelTestCase
    -> Int64      -- ^ @collection_id@
    -> Ptr CBool  -- ^ out: more?
    -> IO CInt

-- | Notify the engine the last element was rejected.
--
-- @why@ may be @NULL@.
foreign import ccall unsafe "hegel_collection_reject"
  hegel_collection_reject
    :: Ptr HegelContext
    -> Ptr HegelTestCase
    -> Int64   -- ^ @collection_id@
    -> CString -- ^ @why@ (optional, may be @NULL@)
    -> IO CInt

-- | Create a new variable pool for stateful testing; writes the pool ID
-- into @*out_pool_id@.
foreign import ccall unsafe "hegel_new_pool"
  hegel_new_pool :: Ptr HegelContext -> Ptr HegelTestCase -> Ptr Int64 -> IO CInt

-- | Register a new variable in the pool; writes its ID into
-- @*out_variable_id@.
foreign import ccall unsafe "hegel_pool_add"
  hegel_pool_add
    :: Ptr HegelContext
    -> Ptr HegelTestCase
    -> Int64     -- ^ @pool_id@
    -> Ptr Int64 -- ^ out: @variable_id@
    -> IO CInt

-- | Draw a variable from the pool.
--
-- Returns 'HEGEL_E_ASSUME' when the pool is empty, which
-- 'Hegel.Internal.DataSource.poolGenerate' turns into an
-- 'Hegel.Internal.Control.AssumeRejected' discard.
foreign import ccall unsafe "hegel_pool_generate"
  hegel_pool_generate
    :: Ptr HegelContext
    -> Ptr HegelTestCase
    -> Int64     -- ^ @pool_id@
    -> CBool     -- ^ @consume@ (remove from pool)
    -> Ptr Int64 -- ^ out: @variable_id@
    -> IO CInt

-- | Register an engine-owned state machine for swarm-based stateful testing;
-- writes the machine ID into @*out_state_machine_id@.
--
-- @rule_names@ and @invariant_names@ are arrays of NUL-terminated UTF-8
-- strings.
--
-- Returns 'HEGEL_E_INVALID_ARG' when @num_rules@ is zero or a name is not
-- valid UTF-8.
foreign import ccall unsafe "hegel_new_state_machine"
  hegel_new_state_machine
    :: Ptr HegelContext
    -> Ptr HegelTestCase
    -> Ptr CString  -- ^ @rule_names@
    -> CSize        -- ^ @num_rules@
    -> Ptr CString  -- ^ @invariant_names@
    -> CSize        -- ^ @num_invariants@
    -> Ptr Int64    -- ^ out: @state_machine_id@
    -> IO CInt

-- | Draw the next rule index in @[0, num_rules)@ for the given state machine,
-- honoring swarm-selected rule restrictions, or signal that the machine is
-- done stepping.
--
-- The engine owns the machine's step cap: once it decides to stop, this
-- writes 'HEGEL_STATE_MACHINE_DONE' into @*out_rule_index@ instead of a rule
-- index, rather than returning an error. Call this exactly once per loop
-- iteration, unconditionally, on generation and replay alike, since skipping
-- a call misaligns every later draw the same way skipping any other draw
-- would.
--
-- Returns 'HEGEL_E_STOP_TEST' when the choice budget is exhausted, which is
-- distinct from the machine finishing normally.
foreign import ccall unsafe "hegel_state_machine_next_rule"
  hegel_state_machine_next_rule
    :: Ptr HegelContext
    -> Ptr HegelTestCase
    -> Int64      -- ^ @state_machine_id@
    -> Ptr Int64  -- ^ out: @rule_index@
    -> IO CInt

-- $typeddraws
--
-- One typed draw per primitive (bool, integer, float, bytes, uuid, string).
-- There is no server-side compound generation: lists, sets, maps,
-- tuples, and choices are composed client-side from spans + 'hegel_new_collection'
-- (see "Hegel.Collection" and "Hegel.Gen.Internal").
--
-- String draws are two-step: build an immutable, caller-owned
-- 'HegelStringGenerator' once via a @hegel_string_generator_*@ constructor
-- (text\/regex\/email\/url\/domain), then draw from it any number of times with
-- 'hegel_generate_string'. Free the generator with 'hegel_string_generator_free'
-- once no more draws will use it.

-- | Draw a single boolean that is @true@ with probability @p@ (in @[0,1]@).
--
-- The @forced@ / @has_forced@ parameters (used by the engine to pin a draw for
-- replay and shrinking) are part of the C ABI but unused here: callers always
-- pass @has_forced = 0@.
--
-- Returns 'HEGEL_E_STOP_TEST' when the choice budget is exhausted.
foreign import ccall unsafe "hegel_generate_boolean"
  hegel_generate_boolean
    :: Ptr HegelContext
    -> Ptr HegelTestCase
    -> CDouble   -- ^ @p@ (probability of @true@, in @[0,1]@)
    -> CBool     -- ^ @forced@ value (used only when @has_forced@ is set)
    -> CBool     -- ^ @has_forced@
    -> Ptr CBool -- ^ out: drawn value
    -> IO CInt

-- | Draw an integer in @[min_value, max_value]@ (both inclusive, both
-- required). For bounds outside the @int64_t@ range use
-- 'hegel_generate_integer_big'.
--
-- Returns 'HEGEL_E_STOP_TEST' when the choice budget is exhausted. Returns
-- 'HEGEL_E_INVALID_ARG' for @min_value > max_value@.
foreign import ccall unsafe "hegel_generate_integer"
  hegel_generate_integer
    :: Ptr HegelContext
    -> Ptr HegelTestCase
    -> Int64     -- ^ @min_value@
    -> Int64     -- ^ @max_value@
    -> Ptr Int64 -- ^ out: drawn value
    -> IO CInt

-- | Draw an arbitrary-precision integer in @[min_value, max_value]@.
--
-- Bounds and result are two's-complement __little-endian__ signed byte
-- buffers. On success writes the drawn value's minimal-length two's-complement
-- LE bytes into @out_value@ (capacity @out_value_cap@), its length into
-- @*out_value_len@, and sign-fills the rest of the buffer up to
-- @out_value_cap@ so reading the whole buffer as a fixed-width two's-complement
-- integer also yields the drawn value. Passing
-- @out_value_cap >= max(min_value_len, max_value_len)@ always succeeds.
--
-- Returns 'HEGEL_E_STOP_TEST' when the choice budget is exhausted. Returns
-- 'HEGEL_E_INVALID_ARG' for NULL\/empty bounds, @min_value > max_value@, or an
-- @out_value@ buffer too small for the drawn value.
foreign import ccall unsafe "hegel_generate_integer_big"
  hegel_generate_integer_big
    :: Ptr HegelContext
    -> Ptr HegelTestCase
    -> Ptr Word8  -- ^ @min_value@ (LE two's-complement bytes)
    -> CSize      -- ^ @min_value_len@
    -> Ptr Word8  -- ^ @max_value@ (LE two's-complement bytes)
    -> CSize      -- ^ @max_value_len@
    -> Ptr Word8  -- ^ out: drawn value (LE two's-complement bytes)
    -> CSize      -- ^ @out_value_cap@
    -> Ptr CSize  -- ^ out: minimal length of the drawn value
    -> IO CInt

-- | Draw a float of the given @width@ (32 or 64) in @[min_value, max_value]@.
--
-- Pass @-INFINITY@\/@INFINITY@ for unbounded ends. NaN is drawn only when
-- @allow_nan@ is set; infinities only when @allow_infinity@ is set and the
-- relevant endpoint is unbounded. @exclude_min@\/@exclude_max@ make the
-- corresponding bound exclusive. Nonzero magnitudes below
-- @smallest_nonzero_magnitude@ are never drawn — it must be positive and
-- finite; pass @5e-324@ (width 64) or the smallest @float@ subnormal
-- (width 32) for no restriction.
--
-- Returns 'HEGEL_E_STOP_TEST' when the choice budget is exhausted. Returns
-- 'HEGEL_E_INVALID_ARG' for an unsupported width, NaN bounds, width-32 bounds
-- not exactly representable as @float@, an invalid
-- @smallest_nonzero_magnitude@, or an empty range.
foreign import ccall unsafe "hegel_generate_float"
  hegel_generate_float
    :: Ptr HegelContext
    -> Ptr HegelTestCase
    -> Word32     -- ^ @width@ (32 or 64)
    -> CDouble    -- ^ @min_value@
    -> CDouble    -- ^ @max_value@
    -> CBool      -- ^ @allow_nan@
    -> CBool      -- ^ @allow_infinity@
    -> CBool      -- ^ @exclude_min@
    -> CBool      -- ^ @exclude_max@
    -> CDouble    -- ^ @smallest_nonzero_magnitude@
    -> Ptr CDouble -- ^ out: drawn value
    -> IO CInt

-- | Draw a byte string with length in @[min_size, max_size]@ (both inclusive).
--
-- On success fills @*out_result@ with an engine-allocated buffer the caller
-- owns; release with 'hegel_generate_bytes_result_free'.
--
-- Returns 'HEGEL_E_STOP_TEST' when the choice budget is exhausted. Returns
-- 'HEGEL_E_INVALID_ARG' for @min_size > max_size@.
foreign import ccall unsafe "hegel_generate_bytes"
  hegel_generate_bytes
    :: Ptr HegelContext
    -> Ptr HegelTestCase
    -> Word64              -- ^ @min_size@
    -> Word64              -- ^ @max_size@
    -> Ptr HegelBytesResult -- ^ out: engine-allocated buffer
    -> IO CInt

-- | Release a buffer returned by 'hegel_generate_bytes' and reset the struct
-- to @{NULL, 0}@. Safe to call with an already-freed (zeroed) struct.
foreign import ccall unsafe "hegel_generate_bytes_result_free"
  hegel_generate_bytes_result_free :: Ptr HegelContext -> Ptr HegelBytesResult -> IO CInt

-- | Draw a UUID as 16 big-endian bytes written to @out_bytes@ (which must have
-- room for 16 bytes).
--
-- When @has_version@ is set, the RFC 4122 version nibble is forced to
-- @version@ (0..=15) and the variant nibble to the RFC 4122 variant.
--
-- Returns 'HEGEL_E_STOP_TEST' when the choice budget is exhausted. Returns
-- 'HEGEL_E_INVALID_ARG' for a @version > 15@.
foreign import ccall unsafe "hegel_generate_uuid"
  hegel_generate_uuid
    :: Ptr HegelContext
    -> Ptr HegelTestCase
    -> Word8    -- ^ @version@ (used only when @has_version@ is set)
    -> CBool    -- ^ @has_version@
    -> Ptr Word8 -- ^ out: 16 big-endian bytes
    -> IO CInt

-- | Build a __text__ string generator: strings with length in
-- @[min_size, max_size]@ whose characters are drawn from the described
-- alphabet (@codec@, codepoint bounds, Unicode general categories,
-- explicit include\/exclude character sets — see @hegel.h@ for the full
-- semantics). @codec@ may be @NULL@ (all of Unicode); the category and
-- character-set arguments may be @NULL@\/zero-length for \"no restriction\".
--
-- On success writes a caller-owned handle into @*out_generator@; release with
-- 'hegel_string_generator_free'. Returns 'HEGEL_E_INVALID_ARG' for
-- @min_size > max_size@, an unknown codec\/category, non-UTF-8 string
-- arguments, include\/exclude conflicts, or constraints leaving no characters
-- while @max_size > 0@.
foreign import ccall unsafe "hegel_string_generator_text"
  hegel_string_generator_text
    :: Ptr HegelContext
    -> Word64          -- ^ @min_size@
    -> Word64          -- ^ @max_size@
    -> CString         -- ^ @codec@ (nullable)
    -> Word32          -- ^ @min_codepoint@
    -> Word32          -- ^ @max_codepoint@
    -> Ptr CString     -- ^ @categories@ (nullable)
    -> CSize           -- ^ @categories_len@
    -> Ptr CString     -- ^ @exclude_categories@ (nullable)
    -> CSize           -- ^ @exclude_categories_len@
    -> Ptr Word8       -- ^ @include_characters@ (UTF-8 bytes, nullable)
    -> CSize           -- ^ @include_characters_len@
    -> Ptr Word8       -- ^ @exclude_characters@ (UTF-8 bytes, nullable)
    -> CSize           -- ^ @exclude_characters_len@
    -> Ptr (Ptr HegelStringGenerator) -- ^ out: caller-owned handle
    -> IO CInt

-- | Build a __regex__ string generator: strings matching @pattern@
-- (Python-@re@ syntax). When @fullmatch@ is true the whole string matches the
-- pattern; otherwise the match may be padded on either side. @alphabet@
-- (optional, @NULL@ for none) must be a __text__ generator constraining the
-- padding and wildcard characters.
--
-- On success writes a caller-owned handle into @*out_generator@; release with
-- 'hegel_string_generator_free'. Returns 'HEGEL_E_INVALID_ARG' for a
-- @NULL@\/non-UTF-8\/invalid @pattern@, or an @alphabet@ that is not a text
-- generator.
foreign import ccall unsafe "hegel_string_generator_regex"
  hegel_string_generator_regex
    :: Ptr HegelContext
    -> CString                        -- ^ @pattern@
    -> CBool                          -- ^ @fullmatch@
    -> Ptr HegelStringGenerator       -- ^ @alphabet@ (borrowed, nullable)
    -> Ptr (Ptr HegelStringGenerator) -- ^ out: caller-owned handle
    -> IO CInt

-- | Build a __URL__ string generator producing RFC 3986 @http@\/@https@ URLs.
foreign import ccall unsafe "hegel_string_generator_url"
  hegel_string_generator_url :: Ptr HegelContext -> Ptr (Ptr HegelStringGenerator) -> IO CInt

-- | Build a __domain-name__ string generator producing RFC 1035 FQDNs of
-- total length at most @max_length@ (4..=255).
--
-- Returns 'HEGEL_E_INVALID_ARG' for a @max_length@ that leaves no eligible
-- top-level domains.
foreign import ccall unsafe "hegel_string_generator_domain"
  hegel_string_generator_domain
    :: Ptr HegelContext
    -> Word64 -- ^ @max_length@
    -> Ptr (Ptr HegelStringGenerator) -- ^ out: caller-owned handle
    -> IO CInt

-- | Release a string generator built by a @hegel_string_generator_*@
-- constructor. Safe to call with @NULL@. Each generator must be freed exactly
-- once, and only after every draw using it has completed.
foreign import ccall unsafe "hegel_string_generator_free"
  hegel_string_generator_free :: Ptr HegelContext -> Ptr HegelStringGenerator -> IO CInt

-- | Draw a string described by @generator@.
--
-- On success fills @*out_result@ with an engine-allocated UTF-8 buffer the
-- caller owns; release with 'hegel_generate_string_result_free'.
--
-- Returns 'HEGEL_E_STOP_TEST' when the choice budget is exhausted, and
-- 'HEGEL_E_ASSUME' when the draw rejected itself (e.g. an email exceeding the
-- RFC length cap — discard the test case as invalid).
--
-- __The one per-test-case primitive imported @safe@, deliberately.__ Every
-- other draw in @$typeddraws@ (boolean\/integer\/float\/bytes\/uuid) is a
-- single bounded RNG-consuming operation, same as the rest of
-- @$pertestcase@ — that's what makes @unsafe@ safe to use there (see the
-- rationale on that section). A regex-backed draw is not: the engine
-- generates a candidate from the pattern's AST and, when a deferred check
-- (a lookaround, an anchor) fails, retries up to 5 times before giving up —
-- each retry is a full re-draw, so the worst case is measurably more than
-- the \~1.1µs floor the @unsafe@ calls are tuned for (still bounded, just
-- not µs-scale). An @unsafe@ call pins the calling capability for however
-- long that takes, with no GC or other Haskell thread able to run on it
-- meanwhile; @safe@'s ~0.1–0.5µs release\/reacquire overhead is a much
-- better trade against that tail than against the plain draws' floor.
foreign import ccall safe "hegel_generate_string"
  hegel_generate_string
    :: Ptr HegelContext
    -> Ptr HegelTestCase
    -> Ptr HegelStringGenerator -- ^ @generator@ (borrowed)
    -> Ptr HegelStringResult    -- ^ out: engine-allocated buffer
    -> IO CInt

-- | Release a buffer returned by 'hegel_generate_string' and reset the struct
-- to @{NULL, 0}@. Safe to call with an already-freed (zeroed) struct.
foreign import ccall unsafe "hegel_generate_string_result_free"
  hegel_generate_string_result_free :: Ptr HegelContext -> Ptr HegelStringResult -> IO CInt

-- | Record a numeric observation for the targeting phase to hill-climb toward.
--
-- @label@ must be non-@NULL@ valid UTF-8.
foreign import ccall unsafe "hegel_target"
  hegel_target :: Ptr HegelContext -> Ptr HegelTestCase -> CDouble -> CString -> IO CInt

-- | Mark the test case complete.
--
-- @origin@ is only read when @status == 'HEGEL_STATUS_INTERESTING'@. @NULL@ is
-- accepted there, but collapses every failure under one generic origin —
-- always pass one.
--
-- __NOTE__: @origin@ must be a stable, draw-independent string (e.g. @\"file:line\"@),
-- so the shrinker can converge towards a target.
foreign import ccall unsafe "hegel_mark_complete"
  hegel_mark_complete
    :: Ptr HegelContext
    -> Ptr HegelTestCase
    -> Word32  -- ^ @status@ (@HEGEL_STATUS_*@)
    -> CString -- ^ @origin@ (@NULL@ unless 'HEGEL_STATUS_INTERESTING')
    -> IO CInt

-- | Clone a test case, writing a caller-owned handle onto an independent
-- choice stream of the same case into @*out_test_case@. The clone shares the
-- source's outcome and budget but draws from its own stream, so it can be
-- driven concurrently from another thread without the two perturbing each
-- other. Free the clone with 'hegel_test_case_free' like any other handle.
--
-- Takes the source handle's lock like a draw, so returns
-- 'HEGEL_E_CONCURRENT_USE' if another thread is mid-operation on it, and
-- 'HEGEL_E_ALREADY_COMPLETE' once the family has completed.
--
-- __NOTE__: freeing a clone does not complete its family. Some handle in
-- the family must call 'hegel_mark_complete' before the last handle is
-- freed, or the run cannot advance.
foreign import ccall unsafe "hegel_test_case_clone"
  hegel_test_case_clone
    :: Ptr HegelContext
    -> Ptr HegelTestCase
    -> Ptr (Ptr HegelTestCase) -- ^ out: caller-owned clone
    -> IO CInt

-- $reproduction
--
-- Build and free a /standalone/ test case that replays a failure blob.
--
-- == Ownership model
--
-- Every @hegel_test_case_t@ is __caller-owned__ and freed with
-- 'hegel_test_case_free', whether it came from 'hegel_next_test_case',
-- 'hegel_test_case_from_blob', or 'hegel_test_case_clone'.
--
-- Prefer 'withTestCaseFromBlob' over these functions wherever possible.

-- | Build a standalone test case that replays the counterexample encoded in
-- @blob@, writing the caller-owned handle into @*out_test_case@.
--
-- Returns 'HEGEL_E_INVALID_ARG' (with a diagnostic in
-- 'hegel_context_last_error') when @blob@ is @NULL@, not valid UTF-8, corrupt,
-- or from an incompatible @libhegel@ version.
--
-- A blob whose choice sequence no longer matches the caller's generators
-- returns 'HEGEL_E_STOP_TEST' on the overrunning draw.
foreign import ccall unsafe "hegel_test_case_from_blob"
  hegel_test_case_from_blob
    :: Ptr HegelContext
    -> Ptr HegelSettings
    -> CString -- ^ @blob@: base64 failure blob from 'hegel_failure_reproduction_blob'
    -> FunPtr OutputSink -- ^ @callback@ (@NULL@ leaves output on stderr)
    -> Ptr ()  -- ^ @user_data@ threaded through to @callback@
    -> Ptr (Ptr HegelTestCase) -- ^ out: caller-owned test case
    -> IO CInt

-- | Free a __caller-owned__ test case, from 'hegel_next_test_case',
-- 'hegel_test_case_from_blob', or 'hegel_test_case_clone'. Safe to call with
-- @NULL@.
foreign import ccall unsafe "hegel_test_case_free"
  hegel_test_case_free :: Ptr HegelContext -> Ptr HegelTestCase -> IO CInt

-- $results
--
-- Read-only accessors over a completed run's result and its individual
-- failures (aggregate status, run-level error, origin, reproduction blob).
--
-- Strings read off the run-result snapshot are borrowed and live until
-- 'hegel_run_result_free'; a failure snapshot's strings live until
-- 'hegel_failure_free'. Copy anything you keep (e.g. a reproduction blob via
-- 'failureReproductionBlob') before freeing the snapshot it came from.

-- | Write the run's aggregate status (one of the @HEGEL_RUN_STATUS_*@ values)
-- into @*out_status@.
foreign import ccall unsafe "hegel_run_result_status"
  hegel_run_result_status :: Ptr HegelContext -> Ptr HegelRunResult -> Ptr CInt -> IO CInt

-- | Write the run-level error message into @*out_error@ (or @NULL@ when the
-- run completed normally rather than erroring). Valid until 'hegel_run_result_free'.
foreign import ccall unsafe "hegel_run_result_error"
  hegel_run_result_error :: Ptr HegelContext -> Ptr HegelRunResult -> Ptr CString -> IO CInt

-- | Write the number of distinct failures (by origin) into @*out_count@.
foreign import ccall unsafe "hegel_run_result_failure_count"
  hegel_run_result_failure_count :: Ptr HegelContext -> Ptr HegelRunResult -> Ptr CSize -> IO CInt

-- | Write a __caller-owned__ snapshot of the @i@-th failure (0-indexed) into
-- @*out_failure@ (or @NULL@ when out of range). Release it with
-- 'hegel_failure_free'; the strings read off it live until then.
foreign import ccall unsafe "hegel_run_result_failure"
  hegel_run_result_failure :: Ptr HegelContext -> Ptr HegelRunResult -> CSize -> Ptr (Ptr HegelFailure) -> IO CInt

-- | Release a failure snapshot from 'hegel_run_result_failure', along with the
-- strings read off it.
--
-- Safe to call with @NULL@.
foreign import ccall unsafe "hegel_failure_free"
  hegel_failure_free :: Ptr HegelContext -> Ptr HegelFailure -> IO CInt

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
-- The written pointer is borrowed (see the section notes above);
-- 'failureReproductionBlob' copies it out.
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
      rc <- hegel_run_start ctx s nullFunPtr nullPtr out
      if rc == HEGEL_OK
        then peek out
        else lastErrorMessage ctx >>= \msg -> throwIO HegelError {code = rc, message = msg}
    release run = void (hegel_run_free ctx run)

-- | Copy the reproduction blob for @f@ into a fresh 'ByteString', or return
-- 'Nothing' when the failure carries no blob (e.g. a health-check failure).
--
-- The underlying C pointer is borrowed from the failure snapshot and only valid
-- until 'hegel_failure_free'; this function copies it immediately so the
-- 'ByteString' is safe to use after the snapshot is freed.
--
-- The blob is ASCII base64 and can be passed directly to 'withTestCaseFromBlob'.
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
-- The bracket frees the handle; do not call 'hegel_test_case_free' on it
-- yourself.
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
      rc <- hegel_test_case_from_blob ctx s blobPtr nullFunPtr nullPtr out
      if rc == HEGEL_OK
        then peek out
        else lastErrorMessage ctx >>= \msg -> throwIO HegelError {code = rc, message = msg}
    release tc = void (hegel_test_case_free ctx tc)

-- | Reusable pinned block that a test case's per-call out-parameters write
-- through, in place of a fresh 'alloca' every call. Covers single-word
-- out-params (rule indices, collection\/pool ids, primitive booleans,
-- integers, floats), the two-word @{ptr, len}@ result structs
-- ('HegelBytesResult', 'HegelStringResult'), and the raw 16-byte UUID buffer.
-- Allocated once per test case ('Hegel.Internal.TestCase.mkTestCase') and
-- reused across every call.
--
-- Calls on one test case never overlap, so a single slot per case is safe:
-- each call's out-param write overwrites whatever the previous call left
-- behind, and every draw reads the out-param only after a successful return
-- code, or throws before reading it at all.
newtype Slot = Slot (ForeignPtr Word8)

-- | 'Slot's byte capacity. The two-word result structs are always
-- word-sized fields, so @2 * wordBytes@ alone would suffice for them, but
-- the UUID buffer's 16 bytes is a fixed wire size independent of the host's
-- word width. 'max' guarantees the slot fits both instead of relying on
-- 64-bit coincidence.
slotCapacity :: Int
slotCapacity = max (2 * wordBytes) 16

-- | Allocate a 'Slot'. Pinned and GC-managed; no finalizer needed.
newSlot :: IO Slot
newSlot = Slot <$> mallocForeignPtrBytes slotCapacity

-- | Use the slot as a single out-parameter of a 'Storable' type no larger
-- than 'slotCapacity'.
withSlotOf :: forall a b. (Storable a) => Slot -> (Ptr a -> IO b) -> IO b
withSlotOf (Slot slot) k
  | sizeOf (undefined :: a) <= slotCapacity = withForeignPtr slot (k . castPtr)
  | otherwise =
      error $
        "withSlotOf: "
          <> show (sizeOf (undefined :: a))
          <> "-byte pointee exceeds the "
          <> show slotCapacity
          <> "-byte Slot"

-- | Use the slot as a raw byte buffer of the given size, which must not
-- exceed 'slotCapacity'.
withSlotBytes :: Int -> Slot -> (Ptr a -> IO b) -> IO b
withSlotBytes n (Slot slot) k
  | n <= slotCapacity = withForeignPtr slot (k . castPtr)
  | otherwise =
      error $
        "withSlotBytes: " <> show n <> " bytes exceeds the " <> show slotCapacity <> "-byte Slot"

-- | The byte size of one machine word on this platform. A pointer and a
-- @size_t@ ('CSize') length are always this same width.
wordBytes :: Int
wordBytes = sizeOf (nullPtr :: Ptr Word8)
