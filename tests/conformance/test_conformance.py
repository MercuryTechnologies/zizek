from pathlib import Path

from hegel.conformance import (
    BinaryConformance,
    BooleanConformance,
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
    run_conformance_tests,
)

BIN_DIR = Path(__file__).parent / "bin"

INT64_MIN = -(2**63)
INT64_MAX = 2**63 - 1


def test_conformance(subtests):
    run_conformance_tests(
        [
            BooleanConformance(BIN_DIR / "test-booleans"),
            BinaryConformance(BIN_DIR / "test-binary"),
            FloatConformance(BIN_DIR / "test-floats"),
            IntegerConformance(
                BIN_DIR / "test-integers",
                min_value=INT64_MIN,
                max_value=INT64_MAX,
            ),
            ListConformance(BIN_DIR / "test-list", skip_unique=True),
            OriginDeduplicationConformance(BIN_DIR / "test-origin-deduplication"),
            SampledFromConformance(BIN_DIR / "test-sampled-from"),
            OneOfConformance(BIN_DIR / "test-one-of"),
            TextConformance(BIN_DIR / "test-text", no_surrogates=True),
            StopTestOnCollectionMoreConformance(BIN_DIR / "test-list", skip_server_metrics=True),
            StopTestOnNewCollectionConformance(BIN_DIR / "test-list", skip_server_metrics=True),
        ],
        subtests,
        skip_tests=[
            DictConformance,
            StopTestOnGenerateConformance,
            StopTestOnMarkCompleteConformance,
            ErrorResponseConformance,
            EmptyTestConformance,
        ],
    )
