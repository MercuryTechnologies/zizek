/*
 * Closed-world guard for the libhegel C enums that zizek mirrors as Haskell
 * ADTs (Hegel.Backend, Hegel.Verbosity, Hegel.Phase, Hegel.HealthCheck,
 * Hegel.Internal.DataSource's Label, Hegel.Internal.TestCase's Status,
 * Hegel.Runner's RunStatus, and hegel_result_t's error-code pattern synonyms
 * in Hegel.Internal.Foreign.Raw).
 *
 * Each function is an EXHAUSTIVE switch with no `default:`. Compiled with
 * `-Werror=switch-enum -Werror=switch`, the build FAILS if hegel-rust adds a
 * new enumerator we don't handle — naming the missing value — so a libhegel
 * bump can't silently widen an enum out from under the closed ADTs. Both
 * flags matter on clang: a missing case with no `default:` is diagnosed
 * under `-Wswitch`, not `-Wswitch-enum`.
 *
 * Parameters take the wire width our `Witch.into` produces (Word32 for
 * Backend/Verbosity/Phase/HealthCheck/Status, Word64 for Label; RunStatus and
 * hegel_result_t are read from the engine as a plain `int` and untested
 * here) and cast to the enum, so the FFI imports in
 * tests/ffi/WireEnumCoverage.hs line up and the switch still checks the
 * enum's members.
 */

#include <hegel.h>
#include <stdint.h>

int hegel_guard_backend(uint32_t x) {
  switch ((hegel_backend_t)x) {
    case HEGEL_BACKEND_AUTO:
    case HEGEL_BACKEND_DEFAULT:
    case HEGEL_BACKEND_URANDOM:
      return 0;
  }
  return -1;
}

int hegel_guard_verbosity(uint32_t x) {
  switch ((hegel_verbosity_t)x) {
    case HEGEL_VERBOSITY_QUIET:
    case HEGEL_VERBOSITY_NORMAL:
    case HEGEL_VERBOSITY_VERBOSE:
    case HEGEL_VERBOSITY_DEBUG:
      return 0;
  }
  return -1;
}

int hegel_guard_phase(uint32_t x) {
  switch ((hegel_phase_t)x) {
    case HEGEL_PHASE_EXPLICIT:
    case HEGEL_PHASE_REUSE:
    case HEGEL_PHASE_GENERATE:
    case HEGEL_PHASE_TARGET:
    case HEGEL_PHASE_SHRINK:
    case HEGEL_PHASE_ALL:
      return 0;
  }
  return -1;
}

int hegel_guard_health_check(uint32_t x) {
  switch ((hegel_health_check_t)x) {
    case HEGEL_HC_FILTER_TOO_MUCH:
    case HEGEL_HC_TOO_SLOW:
    case HEGEL_HC_TEST_CASES_TOO_LARGE:
    case HEGEL_HC_LARGE_INITIAL_TEST_CASE:
      return 0;
  }
  return -1;
}

int hegel_guard_label(uint64_t x) {
  switch ((hegel_label_t)x) {
    case HEGEL_LABEL_LIST:
    case HEGEL_LABEL_LIST_ELEMENT:
    case HEGEL_LABEL_SET:
    case HEGEL_LABEL_SET_ELEMENT:
    case HEGEL_LABEL_MAP:
    case HEGEL_LABEL_MAP_ENTRY:
    case HEGEL_LABEL_TUPLE:
    case HEGEL_LABEL_ONE_OF:
    case HEGEL_LABEL_OPTIONAL:
    case HEGEL_LABEL_FIXED_DICT:
    case HEGEL_LABEL_FLAT_MAP:
    case HEGEL_LABEL_FILTER:
    case HEGEL_LABEL_MAPPED:
    case HEGEL_LABEL_SAMPLED_FROM:
    case HEGEL_LABEL_ENUM_VARIANT:
    case HEGEL_LABEL_FEATURE_FLAG:
    case HEGEL_LABEL_REGEX:
    case HEGEL_LABEL_EMAIL:
    case HEGEL_LABEL_URL:
    case HEGEL_LABEL_DOMAIN:
    case HEGEL_LABEL_DATE:
    case HEGEL_LABEL_TIME:
    case HEGEL_LABEL_DATETIME:
    case HEGEL_LABEL_UUID:
    case HEGEL_LABEL_IP_ADDRESS:
    case HEGEL_LABEL_INTEGER:
    case HEGEL_LABEL_FLOAT:
    case HEGEL_LABEL_BOOLEAN:
    case HEGEL_LABEL_BYTES:
    case HEGEL_LABEL_STRING:
    case HEGEL_LABEL_STATEFUL_RULE:
      return 0;
  }
  return -1;
}

int hegel_guard_status(uint32_t x) {
  switch ((hegel_status_t)x) {
    case HEGEL_STATUS_VALID:
    case HEGEL_STATUS_INVALID:
    case HEGEL_STATUS_OVERRUN:
    case HEGEL_STATUS_INTERESTING:
      return 0;
  }
  return -1;
}

int hegel_guard_run_status(int x) {
  switch ((hegel_run_status_t)x) {
    case HEGEL_RUN_STATUS_PASSED:
    case HEGEL_RUN_STATUS_FAILED:
    case HEGEL_RUN_STATUS_ERROR:
      return 0;
  }
  return -1;
}

int hegel_guard_result(int x) {
  switch ((hegel_result_t)x) {
    case HEGEL_OK:
    case HEGEL_E_STOP_TEST:
    case HEGEL_E_ASSUME:
    case HEGEL_E_BACKEND:
    case HEGEL_E_INVALID_HANDLE:
    case HEGEL_E_INVALID_ARG:
    case HEGEL_E_ALREADY_COMPLETE:
    case HEGEL_E_NOT_COMPLETE:
    case HEGEL_E_INTERNAL:
    case HEGEL_E_CONCURRENT_USE:
      return 0;
  }
  return -1;
}
