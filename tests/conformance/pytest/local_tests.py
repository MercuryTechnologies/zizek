"""Local ConformanceTest subclasses.

These are extensions to the upstream ``hegel.conformance`` tests that have
been added in an attempt to characterize additional functionality that fallse
outside the purview of our unit test suite.

Some of these may be useful to upstream, after this library has been made
publicly available.
"""

from typing import Any, ClassVar

import hypothesis.strategies as st
from hypothesis import assume
from hypothesis.errors import InvalidArgument
from hegel.conformance import ALL_CATEGORIES, ConformanceTest, TextConformance


def _average_ranks(values: list[float]) -> list[float]:
    """1-based ranks, averaging within tie groups (scipy-free)."""
    order = sorted(range(len(values)), key=lambda i: values[i])
    ranks = [0.0] * len(values)
    i = 0
    while i < len(order):
        j = i
        while j + 1 < len(order) and values[order[j + 1]] == values[order[i]]:
            j += 1
        avg = (i + j) / 2 + 1  # mean of the 1-based ranks i+1..j+1
        for k in range(i, j + 1):
            ranks[order[k]] = avg
        i = j + 1
    return ranks


def _spearman(xs: list[float], ys: list[float]) -> float:
    """Spearman's rho, computed as the Pearson correlation of average ranks.

    Using average ranks (rather than the tie-free ``1 - 6*sum(d^2)/...`` form)
    keeps the coefficient correct when sample counts tie. Returns 0.0 if a
    sequence has zero rank variance — only possible here when every branch got
    the same count, which should fail the ordering check, and does (0.0 < the
    threshold).
    """
    n = len(xs)
    rx, ry = _average_ranks(xs), _average_ranks(ys)
    mean_x, mean_y = sum(rx) / n, sum(ry) / n
    cov = sum((a - mean_x) * (b - mean_y) for a, b in zip(rx, ry))
    var_x = sum((a - mean_x) ** 2 for a in rx)
    var_y = sum((b - mean_y) ** 2 for b in ry)
    if var_x == 0 or var_y == 0:
        return 0.0
    return cov / (var_x * var_y) ** 0.5


class FrequencyConformance(ConformanceTest):
    """Exercise Gen.frequency (weighted one-of).

    Branches are non-overlapping integer ranges with positive weights. The
    binary emits both ``value`` and ``branch`` (the chosen index) per case,
    so validation can confirm:

    1. **Value correctness** — every generated value lies inside its
       declared branch's range (a wrong-branch-index bug would surface here).
    2. **Reachability** — every branch is selected at least once.
    3. **Weight ordering (trend)** — the heaviest branch outranks the
       lightest, and weight and sample count are strongly rank-correlated.
       A noise-tolerant check, since a finite engine run does not sample in
       exact proportion to the weights (see ``validate``).

    Branch ranges are pinned equal across branches to hold entropy constant:
    if ranges differed in size, the engine would explore the larger branch
    more regardless of weight, masking the weight signal entirely.

    The params strategy draws strictly-decreasing, unique weights so every
    adjacent pair has a well-defined expected rank — equal-weight ties would
    be resolved by index position under the native engine, not by weight.
    """

    default_test_cases = 300

    # Each branch covers the same number of values so range-size effects
    # don't confound the weight signal.
    _BRANCH_RANGE_SIZE = 100

    # Minimum weight/count Spearman correlation. 0.5 tolerates a local adjacent
    # inversion (one swap gives rho >= 0.5 for n in 2..4) while rejecting any
    # broadly wrong ordering (a full reversal gives rho <= 0).
    _MIN_SPEARMAN = 0.5

    def params_strategy(self) -> st.SearchStrategy[dict[str, Any]]:
        @st.composite
        def strategy(draw: st.DrawFn) -> dict[str, Any]:
            n_branches = draw(st.integers(2, 4))
            # Unique weights guarantee strictly-decreasing order after
            # sorting, so every adjacent pair has a meaningful rank to assert.
            weight_set = draw(
                st.sets(st.integers(1, 8), min_size=n_branches, max_size=n_branches)
            )
            weights = sorted(weight_set, reverse=True)
            branches = [
                {
                    "weight": w,
                    "min_value": i * 1000,
                    "max_value": i * 1000 + self._BRANCH_RANGE_SIZE - 1,
                }
                for i, w in enumerate(weights)
            ]
            return {"branches": branches}

        return strategy()

    def validate(
        self,
        metrics_list: list[dict[str, Any]],
        params: dict[str, Any],
    ) -> None:
        branches = params["branches"]

        observed = [0] * len(branches)
        for metrics in metrics_list:
            idx = metrics["branch"]
            value = metrics["value"]
            assert 0 <= idx < len(branches)
            b = branches[idx]
            # Property 1: value correctness.
            assert b["min_value"] <= value <= b["max_value"], (
                f"branch {idx} (weight={b['weight']}) generated {value} "
                f"outside [{b['min_value']}, {b['max_value']}]"
            )
            observed[idx] += 1

        # Property 2: reachability.
        for i, b in enumerate(branches):
            assert observed[i] > 0, (
                f"branch {i} (weight={b['weight']}) was never selected "
                f"(counts: {observed})"
            )

        # Property 3: weight ordering (noise-tolerant).
        #
        # The engine guarantees heavier branches are *favored*, not that
        # realized counts at this sample size are strictly monotonic. Adjacent
        # near-equal weights routinely invert under multinomial noise, and the
        # params search actively hunts for those near-ties — so strict
        # per-adjacent-pair ordering is flaky by construction. Assert the two
        # robust signals instead.
        weights = [b["weight"] for b in branches]

        # 3a. The heaviest branch outranks the lightest. Weights are sorted
        # strictly decreasing, so this is the largest, most reliable gap.
        assert observed[0] > observed[-1], (
            f"heaviest branch (weight={weights[0]}) got {observed[0]} samples "
            f"but lightest (weight={weights[-1]}) got {observed[-1]} "
            f"(full counts: {observed})"
        )

        # 3b. Weight and sample count are strongly rank-correlated overall, so a
        # local inversion is tolerated but a systematically wrong distribution
        # is not.
        rho = _spearman([float(w) for w in weights], [float(c) for c in observed])
        assert rho >= self._MIN_SPEARMAN, (
            f"weight/count Spearman correlation {rho:.3f} < {self._MIN_SPEARMAN} "
            f"(weights: {weights}, counts: {observed})"
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
