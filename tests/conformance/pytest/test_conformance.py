import os
from pathlib import Path
from typing import Any

import pytest
from hypothesis import Phase, given
from hypothesis import settings as hyp_settings

from hegel.conformance import (
    BinaryConformance,
    BooleanConformance,
    ConformanceTest,
    DictConformance,
    EmptyTestConformance,
    ErrorResponseConformance,
    FloatConformance,
    IntegerConformance,
    ListConformance,
    OneOfConformance,
    OriginDeduplicationConformance,
    SampledFromConformance,
    StopTestOnCollectionMoreConformance,
    StopTestOnGenerateConformance,
    StopTestOnMarkCompleteConformance,
    StopTestOnNewCollectionConformance,
    TextConformance,
)

from local_tests import (
    FrequencyConformance,
    NativeTextConformance,
    RegexFeatureConformance,
)

BIN_DIR = Path(__file__).parent / "bin"

# Which backend the Haskell binaries are using.  The justfile sets this
# explicitly; when absent (e.g. ad-hoc runs) the binaries default to native.
_BACKEND = os.environ.get("HEGEL_BACKEND", "native")
_NATIVE = _BACKEND == "native"

INT32_MIN = -(2**31)
INT32_MAX = 2**31 - 1
INT64_MIN = -(2**63)
INT64_MAX = 2**63 - 1

# Tests that cannot pass under the native backend and are skipped there.
#
# TextConformance: the conformance harness samples codec names from ALL_CODECS
#   (all Python codec aliases recognised by `codecs`). libhegel only accepts
#   three codec strings — "ascii", "latin-1"/"iso-8859-1", "utf-8" — returning
#   HEGEL_E_INVALID_ARG for anything else (e.g. "arabic", "u8", "latin_1").
#   Since conformance.py is an external wheel we cannot narrow the codec
#   strategy there.  TODO: re-enable once libhegel widens its codec vocabulary
#   (hegeldev/hegel-rust).
_NATIVE_SKIP: frozenset[str] = frozenset({"TextConformance"})

_TESTS: list[ConformanceTest] = [
    BooleanConformance(BIN_DIR / "test-booleans", skip_server_metrics=_NATIVE),
    BinaryConformance(BIN_DIR / "test-binary", skip_server_metrics=_NATIVE),
    FloatConformance(BIN_DIR / "test-floats", skip_server_metrics=_NATIVE),
    IntegerConformance(BIN_DIR / "test-integers", min_value=INT64_MIN, max_value=INT64_MAX, skip_server_metrics=_NATIVE),
    IntegerConformance(BIN_DIR / "test-integers-narrow", min_value=INT32_MIN, max_value=INT32_MAX, skip_server_metrics=_NATIVE),
    ListConformance(BIN_DIR / "test-list", skip_unique=True, skip_server_metrics=_NATIVE),
    ListConformance(BIN_DIR / "test-set", skip_unique=False, skip_server_metrics=_NATIVE),
    DictConformance(
        BIN_DIR / "test-map",
        min_key=INT64_MIN,
        max_key=INT64_MAX,
        min_value=INT64_MIN,
        max_value=INT64_MAX,
        skip_server_metrics=True,  # always: map doesn't use server metrics
    ),
    OriginDeduplicationConformance(BIN_DIR / "test-origin-deduplication", skip_server_metrics=_NATIVE),
    SampledFromConformance(BIN_DIR / "test-sampled-from", skip_server_metrics=_NATIVE),
    OneOfConformance(BIN_DIR / "test-one-of", skip_server_metrics=_NATIVE),
    TextConformance(BIN_DIR / "test-text", no_surrogates=True, skip_server_metrics=_NATIVE),
    NativeTextConformance(BIN_DIR / "test-text", no_surrogates=True, skip_server_metrics=_NATIVE),
    StopTestOnCollectionMoreConformance(BIN_DIR / "test-list", skip_server_metrics=True),  # always
    StopTestOnNewCollectionConformance(BIN_DIR / "test-list", skip_server_metrics=True),  # always
    FrequencyConformance(BIN_DIR / "test-frequency", skip_server_metrics=_NATIVE),
    RegexFeatureConformance(BIN_DIR / "test-regex", skip_server_metrics=_NATIVE),
]

_SKIP_TESTS: list[type[ConformanceTest]] = [
    StopTestOnGenerateConformance,
    StopTestOnMarkCompleteConformance,
    ErrorResponseConformance,
    EmptyTestConformance,
]


def _cases() -> list[pytest.param]:
    # Count how many times each class appears so we can disambiguate with the binary name.
    from collections import Counter
    counts: Counter[str] = Counter(type(t).__name__ for t in _TESTS)

    cases = []
    for t in _TESTS:
        cls_name = type(t).__name__
        prefix = f"{cls_name}-{t.binary.name}" if counts[cls_name] > 1 else cls_name
        native_skip = _NATIVE and cls_name in _NATIVE_SKIP
        marks = (
            [pytest.mark.skip(reason="native backend: libhegel limitation; tracked as WIP")]
            if native_skip
            else []
        )
        for mode in t.modes or [None]:
            suffix = f"[{mode}]" if mode is not None else ""
            cases.append(pytest.param(t, mode, id=f"{prefix}{suffix}", marks=marks))
    for cls in _SKIP_TESTS:
        cases.append(
            pytest.param(
                None,
                None,
                id=cls.__name__,
                marks=pytest.mark.skip(reason="not yet implemented"),
            )
        )
    return cases


def test_registered_coverage() -> None:
    declared = {type(t).__name__ for t in _TESTS} | {c.__name__ for c in _SKIP_TESTS}
    registered = {c.__name__ for c in ConformanceTest.registered_tests}
    assert declared == registered


@pytest.mark.parametrize(("test", "mode"), _cases())
def test_conformance(test: ConformanceTest, mode: str | None) -> None:
    @hyp_settings(max_examples=5, deadline=None, phases=set(Phase) - {Phase.shrink}, database=None)
    @given(test.params_strategy())
    def run(params: dict[str, Any]) -> None:
        if mode is not None:
            params["mode"] = mode
        test.run(params)

    run()
