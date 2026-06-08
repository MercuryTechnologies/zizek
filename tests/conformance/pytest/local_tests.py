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
from hypothesis import assume
from hypothesis.errors import InvalidArgument
from hegel.conformance import ALL_CATEGORIES, ConformanceTest, TextConformance



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


class NativeTextConformance(TextConformance):
    """TextConformance restricted to the codec values libhegel recognises.

    The upstream ``TextConformance`` samples ``codec`` from ``ALL_CODECS``
    (~100+ Python codec aliases). libhegel's native schema interpreter only
    accepts ``"ascii"``, ``"latin-1"``/``"iso-8859-1"``, and ``"utf-8"``
    (mapped to codepoint ranges [0,127], [0,255], and [0,0x10FFFF]
    respectively); anything else returns ``HEGEL_E_INVALID_ARG``, causing
    every test case to be rejected with "no valid examples found".

    This subclass covers the same parameter space for size bounds, codepoint
    ranges, Unicode category filters, and include/exclude characters — just
    with the codec choice narrowed to ``["ascii", "latin-1", "utf-8"]``
    (the only values libhegel's ``build_intervals_uncached`` recognises;
    anything else returns ``HEGEL_E_INVALID_ARG``).  The inherited
    ``validate()`` method is unchanged, so the same per-codepoint assertions
    apply.

    Runs under both backends.  Under the server backend it exercises a useful
    subset of the text parameter space on top of the (skipped-under-native)
    upstream ``TextConformance``.  Surrogates are always excluded since
    Haskell ``Text`` cannot represent them.
    """

    def params_strategy(self) -> st.SearchStrategy[dict[str, Any]]:
        @st.composite
        def strategy(draw: st.DrawFn) -> dict[str, Any]:
            params: dict[str, Any] = {}

            use_codec = draw(st.booleans())
            use_min_codepoint = draw(st.booleans())
            use_max_codepoint = draw(st.booleans())
            use_categories = draw(st.booleans())
            use_exclude_categories = draw(st.booleans())
            use_exclude_chars = draw(st.booleans())
            use_include_chars = draw(st.booleans())

            # categories and exclude_categories are mutually exclusive
            assume(not (use_categories and use_exclude_categories))

            if use_codec:
                # libhegel only accepts "ascii" → [0,127], "latin-1" → [0,255],
                # "utf-8" → [0,0x10FFFF]; anything else → HEGEL_E_INVALID_ARG.
                params["codec"] = draw(st.sampled_from(["ascii", "latin-1", "utf-8"]))
            if use_min_codepoint:
                # Restrict to the BMP: libhegel's category filtering only
                # covers U+0000–U+FFFF, so a min_codepoint above U+FFFF
                # combined with a category filter produces an empty interval
                # set inside build_intervals_uncached, causing every draw to
                # return HEGEL_E_INVALID_ARG even when Hypothesis considers
                # the combination valid (it has a full supplementary-plane
                # Unicode database).
                params["min_codepoint"] = draw(st.integers(0, 0xFFFF))
            if use_max_codepoint:
                lo = params.get("min_codepoint", 0)
                params["max_codepoint"] = draw(st.integers(lo, 0xFFFF))
            if use_categories:
                params["categories"] = draw(
                    st.lists(st.sampled_from(ALL_CATEGORIES))
                )
            if use_exclude_categories:
                params["exclude_categories"] = draw(
                    st.lists(st.sampled_from(ALL_CATEGORIES))
                )

            # Always exclude surrogates — Haskell Text cannot represent them.
            if use_categories:
                params["categories"] = [
                    c for c in params["categories"] if c != "Cs"
                ]
            else:
                excl = set(params.get("exclude_categories", [])) | {"Cs"}
                params["exclude_categories"] = list(excl)

            # libhegel only applies Unicode category filtering within the BMP
            # (U+0000–U+FFFF); above that it emits codepoints of any category —
            # including unassigned (Cn) and other excluded categories — so a
            # category filter with an unbounded max produces out-of-category
            # codepoints. When a user-driven category filter is active and no
            # max is set, cap the range to the BMP so generation stays within
            # libhegel's filtering coverage. Mirrors the min_codepoint cap
            # above; tracked upstream as a libhegel bug.
            if (use_categories or use_exclude_categories) and "max_codepoint" not in params:
                params["max_codepoint"] = 0xFFFF

            if use_exclude_chars:
                params["exclude_characters"] = draw(st.text())
            if use_include_chars:
                params["include_characters"] = draw(st.text())

            # Reject combinations that produce an empty alphabet (e.g.
            # codec="ascii" with min_codepoint=200).
            try:
                st.characters(**params).validate()
            except (InvalidArgument, ValueError):
                assume(False)

            min_size = draw(st.integers(0, 20))
            max_size = draw(st.none() | st.integers(min_size, 20))
            result: dict[str, Any] = {"min_size": min_size, **params}
            if max_size is not None:
                result["max_size"] = max_size
            return result

        return strategy()
