"""Local ConformanceTest subclasses.

These are extensions to the upstream ``hegel.conformance`` tests that have
been added in an attempt to characterize additional functionality that fallse
outside the purview of our unit test suite.

Some of these may be useful to upstream, after this library has been made
publicly available.
"""

import math
from typing import Any, ClassVar

import hypothesis.strategies as st
from hegel.conformance import ConformanceTest


def _wilson_score_interval(successes: int, n: int, z: float) -> tuple[float, float]:
    """Two-sided Wilson score interval for a binomial proportion.

    Preferred over the normal approximation because it stays well-behaved
    near p = 0 or p = 1 and at moderate sample sizes. See
    https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval
    """
    p_hat = successes / n
    denom = 1 + z * z / n
    center = (p_hat + z * z / (2 * n)) / denom
    half_width = (
        z * math.sqrt(p_hat * (1 - p_hat) / n + z * z / (4 * n * n)) / denom
    )
    return center - half_width, center + half_width


class FrequencyConformance(ConformanceTest):
    """Exercise Gen.frequency (weighted one-of).

    Branches are non-overlapping integer ranges with positive weights. The
    binary emits both ``value`` and ``branch`` (the chosen index) per case,
    so validation can confirm:

    1. Every generated value lies inside its declared branch's range (a
       wrong-branch-index bug would surface here).
    2. The observed per-branch proportion is within a Wilson score
       interval of the declared proportion ``weight / total_weight``.

    Branch ranges are pinned equal across branches. Hypothesis's
    novel-prefix exploration walks distinct choice sequences, so when
    branch ranges differ in size the empirical distribution tracks range
    size rather than weight (the same effect is observable on ``oneOf``).
    Pinning ranges removes that confound so the test cleanly exercises
    the weight mechanism.
    """

    default_test_cases = 300

    # Each branch covers the same number of values so range-size effects
    # don't confound the weight signal.
    _BRANCH_RANGE_SIZE = 100

    # Per-branch Wilson critical value.
    #
    # z = 4.5 corresponds to a per-check two-sided false-positive rate of
    # ~7e-6.
    #
    # Over up to 5 hypothesis examples * 3 branches = 15 checks per pytest run,
    # the Bonferroni-corrected family-wise rate stays under 1e-4.
    _WILSON_Z: ClassVar[float] = 4.5

    def params_strategy(self) -> st.SearchStrategy[dict[str, Any]]:
        @st.composite
        def strategy(draw: st.DrawFn) -> dict[str, Any]:
            n_branches = draw(st.integers(2, 3))
            branches = []
            for i in range(n_branches):
                base = i * 1000
                # Weights bounded so max/min ratio stays <= 4. With 300
                # cases this keeps the lightest branch's expected count
                # comfortably above 25.
                weight = draw(st.integers(1, 4))
                branches.append(
                    {
                        "weight": weight,
                        "min_value": base,
                        "max_value": base + self._BRANCH_RANGE_SIZE - 1,
                    }
                )
            return {"branches": branches}

        return strategy()

    def validate(
        self,
        metrics_list: list[dict[str, Any]],
        params: dict[str, Any],
    ) -> None:
        branches = params["branches"]
        total_weight = sum(b["weight"] for b in branches)
        n = len(metrics_list)

        observed = [0] * len(branches)
        for metrics in metrics_list:
            idx = metrics["branch"]
            value = metrics["value"]
            assert 0 <= idx < len(branches)
            b = branches[idx]
            assert b["min_value"] <= value <= b["max_value"]
            observed[idx] += 1

        for i, b in enumerate(branches):
            expected_prop = b["weight"] / total_weight
            lo, hi = _wilson_score_interval(observed[i], n, self._WILSON_Z)
            assert lo <= expected_prop <= hi, (
                f"branch {i} weight={b['weight']}/{total_weight}: "
                f"observed {observed[i]}/{n} = {observed[i] / n:.3f}, "
                f"expected proportion {expected_prop:.3f}, "
                f"Wilson CI [{lo:.3f}, {hi:.3f}] "
                f"(full counts: {observed})"
            )


_REGEX_FEATURE_PATTERNS: list[str] = [
    # anchors
    "^foo$",
    "^bar",
    "baz$",
    # character classes
    "[a-z]+",
    "[^0-9]{3}",
    r"\d{2,4}",
    r"\w+",
    # quantifiers
    "a*b",
    "a+b",
    "ab?c",
    "a{3}",
    "a{2,5}",
    # alternation
    "cat|dog|bird",
    # escapes
    r"\.+",
    r"\(\)",
    # grouping
    "(ab)+",
    "(?:ab)+",
]


class RegexFeatureConformance(ConformanceTest):
    """Exercise a curated regex feature matrix against test-regex.

    The shared upstream ``hegel.conformance`` package doesn't ship a regex
    test, so we drive ``test-regex`` ourselves with patterns covering
    anchors, character classes, quantifiers, alternation, escapes, and
    grouping. Always uses fullmatch so validation can use ``re.fullmatch``.
    """

    default_test_cases = 25

    def params_strategy(self) -> st.SearchStrategy[dict[str, Any]]:
        return st.fixed_dictionaries(
            {
                "pattern": st.sampled_from(_REGEX_FEATURE_PATTERNS),
                "fullmatch": st.just(True),
            }
        )

    def validate(
        self,
        metrics_list: list[dict[str, Any]],
        params: dict[str, Any],
    ) -> None:
        import re

        pattern = params["pattern"]
        compiled = re.compile(pattern)
        for metrics in metrics_list:
            value = metrics["value"]
            assert compiled.fullmatch(value) is not None, (
                f"pattern {pattern!r} did not fullmatch generated string {value!r}"
            )
