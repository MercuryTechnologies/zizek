/*
 * Closed-world guard for the libhegel C enums that zizek mirrors as Haskell
 * ADTs (Hegel.Backend, Hegel.Verbosity, Hegel.Phase, Hegel.HealthCheck,
 * Hegel.Internal.DataSource's Label, Hegel.Internal.TestCase's Status, and
 * Hegel.Runner's RunStatus).
 *
 * Each function is an EXHAUSTIVE switch with no `default:`. Compiled with
 * `-Werror=switch-enum`, the build FAILS if hegel-rust adds a new enumerator
 * we don't handle — naming the missing value — so a libhegel bump can't
 * silently widen an enum out from under the closed ADTs.
 *
 * Parameters take the wire width our `Witch.into` produces (CInt / Word32 /
 * Word64) and cast to the enum, so the FFI imports in
 * tests/ffi/WireEnumCoverage.hs line up and the switch still checks the enum's
 * members.
 */

#include <hegel.h>
#include <stdint.h>

int hegel_guard_backend(int x) {
  switch ((hegel_backend_t)x) {
    case HEGEL_BACKEND_AUTO:
    case HEGEL_BACKEND_DEFAULT:
    case HEGEL_BACKEND_URANDOM:
      return 0;
  }
  return -1;
}

int hegel_guard_verbosity(int x) {
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
      return 0;
  }
  return -1;
}

int hegel_guard_status(int x) {
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
