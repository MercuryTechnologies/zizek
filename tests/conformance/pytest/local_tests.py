"""Local ConformanceTest subclasses.

These live in-repo rather than in the upstream ``hegel.conformance`` package
because they cover gaps unique to the Haskell SDK (or just not yet shared with
other SDKs). If any prove useful cross-SDK, lift them upstream and drop the
local copy.
"""

from typing import Any, ClassVar

import hypothesis.strategies as st
from hegel.conformance import ConformanceTest


class FrequencyConformance(ConformanceTest):
    """Exercise Gen.frequency (weighted one-of).

    Branches are non-overlapping integer ranges with positive weights. The
    binary emits both ``value`` and ``branch`` (the chosen index) per case,
    so validation can confirm every value lies inside its declared branch
    and that more than one branch is exercised across the run.
    """

    def params_strategy(self) -> st.SearchStrategy[dict[str, Any]]:
        @st.composite
        def strategy(draw: st.DrawFn) -> dict[str, Any]:
            n_branches = draw(st.integers(2, 4))
            branches = []
            for i in range(n_branches):
                base = i * 1000
                lo = base + draw(st.integers(0, 100))
                hi = lo + draw(st.integers(1, 100))
                weight = draw(st.integers(1, 10))
                branches.append({"weight": weight, "min_value": lo, "max_value": hi})
            return {"branches": branches}

        return strategy()

    def validate(
        self,
        metrics_list: list[dict[str, Any]],
        params: dict[str, Any],
    ) -> None:
        branches = params["branches"]
        seen: set[int] = set()
        for metrics in metrics_list:
            idx = metrics["branch"]
            value = metrics["value"]
            assert 0 <= idx < len(branches)
            b = branches[idx]
            assert b["min_value"] <= value <= b["max_value"]
            seen.add(idx)
        # With 50 cases over 2-4 branches at positive weights, at least two
        # branches should be exercised.
        assert len(seen) >= 2


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
