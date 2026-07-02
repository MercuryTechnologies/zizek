# Eval harness: zizek vs QuickCheck vs hedgehog

> **Roadmap: farthest out.** Scheduled after the stateful trace-rendering
> work (`notes/roadmap/01-stateful-trace-rendering.md`).

## Context

We need empirical evidence that `zizek` is competitive with the incumbent Haskell PBT libraries on the three things that actually matter: shrinking quality, error messages, and performance.

This note specifies the **scaffolding** to build (cabal stanzas, deps, a shared property-spec language, an initial worked example) and the **methodology** for growing the corpus. The generator vocabulary is broad enough for a full evaluation now; `DInt` is the minimum foothold, not the ceiling.

Relevant existing pieces:
- `Gen a` is a GADT in `library/Hegel/Gen/Internal.hs` (not a typeclass).
- `check` (`library/Hegel/Runner.hs`) returns `Report` with `result :: Result`; `prop` is the ergonomic top-level wrapper.
- Generators span scalars (`Bool`, `Int`/`Integer`, `Float`/`Double`, `Binary`, `Text`, `Char`, `UUID`, `URI`, `Domain`, `Regex`), collections (`List`, `Set`, `Map`), and combinators (`oneOf`, `frequency`, `filtered`, `maybe`, `either`). The `DInt` worked example is a minimal start; `DList`/`DText`/etc. are straightforward follow-ons.
- No benchmark stanza, no QC/hedgehog deps today.

## Approach

Add a single **internal sublibrary** `zizek-eval` plus a new `test-suite eval` and `benchmark eval`. The internal lib keeps `QuickCheck`/`hedgehog` out of the main `zizek` build graph but lets both the test and bench drivers share spec + adapter code without duplication.

Properties are defined as **plain data** (`PropertySpec`): a `Domain` describing input shape, an optional `KnownMinimum`, and a pure `Predicate`. Three backend adapters (zizek / QuickCheck / hedgehog) interpret the spec into their native generator and runner. `Eval.Run` drives a spec across all backends and produces a `BackendResult` (counterexample, shrink-step count when available, distance-from-known-minimum, verbatim error text).

Benchmarks use `tasty-bench` with `bcompare` groups; each spec is one `bgroup`, each backend a `bench`. Perf data goes to stdout (and optionally `--csv` when we want to crunch it offline) — no custom metrics file. The test driver prints counterexamples + error text to stdout and asserts on counterexample correctness + `distance_from_min == 0`. All three backends run sequentially for fair wall-clock comparison.

If structured per-run failure-shape data becomes load-bearing later (corpus grows, regression tracking matters), revisit then — and consider sqlite over JSONL so we can join runs over time.

## Module layout

New sublibrary rooted at `eval/`:

- `eval/Eval/Spec.hs` — `PropertySpec`, `Domain` (initially just `DInt { lo, hi }`, with placeholders documented for `DList`/`DTuple`/`DOptional`), `Predicate (a -> Maybe FailureNote)`.
- `eval/Eval/Result.hs` — `BackendResult` (counterexample text, error text, shrink steps `Maybe Int`, `distance_from_min :: Maybe Integer`, status).
- `eval/Eval/Backend/Zizek.hs` — adapter; interprets `DInt` via `Gen.integral @Int & Gen.min lo & Gen.max hi & Gen.build`, runs `check defaultSettings { phases = [...,Shrink] } (forEach gen body)`, maps `Result.Counterexample` → `BackendResult`. Zizek doesn't expose shrink-step counts today → record `Nothing` (follow-up: add a counter in `runEventLoop`).
- `eval/Eval/Backend/QuickCheck.hs` — `chooseBoundedIntegral`, `quickCheckWithResult`, extract `failingTestCase` + `numShrinks` from `Result`.
- `eval/Eval/Backend/Hedgehog.hs` — `Gen.integral (Range.linear ...)`, `checkReport`, pull counterexample + `reportShrinks` from `Report`.
- `eval/Eval/Run.hs` — `runSpecAllBackends`, sequential execution; `distance :: Domain a -> a -> a -> Integer` (numeric for `DInt`; future length/depth metrics for structured domains).
- `eval/Eval/Specs/IntegerSpecs.hs` — exports the one worked example `notFortyTwo`.

Drivers:
- `eval/test/Main.hs` — `exitcode-stdio` test; runs `notFortyTwo` on all backends, prints results, asserts counterexample discovery + `distance_from_min == 0`.
- `eval/bench/Main.hs` — `tasty-bench` driver with one `bgroup` per spec.

## Cabal changes (`zizek.cabal`)

Append:

```cabal
library zizek-eval
  import: all
  visibility: public
  hs-source-dirs: eval
  exposed-modules:
    Eval.Spec
    Eval.Result
    Eval.Backend.Zizek
    Eval.Backend.QuickCheck
    Eval.Backend.Hedgehog
    Eval.Run
    Eval.Specs.IntegerSpecs
  build-depends:
    zizek,
    text          ^>=2.1,
    QuickCheck    ^>=2.15 || ^>=2.16,
    hedgehog      ^>=1.5,

test-suite eval
  import: all
  type: exitcode-stdio-1.0
  hs-source-dirs: eval/test
  main-is: Main.hs
  build-depends: zizek, zizek:zizek-eval

benchmark eval
  import: all
  type: exitcode-stdio-1.0
  hs-source-dirs: eval/bench
  main-is: Main.hs
  build-depends:
    zizek,
    zizek:zizek-eval,
    tasty         ^>=1.5,
    tasty-bench   ^>=0.4,
```

Skip `tasty-quickcheck`/`tasty-hedgehog` — adapters call each library directly so we control failure-shape extraction precisely.

## Worked example: `int.not_forty_two`

Spec on `[-100, 100]`, predicate `x /= 42`, `knownMin = Just 42`.

- **Zizek**: `Gen.integral @Int & Gen.min (-100) & Gen.max 100 & Gen.build` + property body that throws on `x == 42` → `Result.Counterexample` with the rendered value `42`. `distance_from_min = 0`. `shrink_steps = null` (not exposed yet).
- **QuickCheck**: `forAll (chooseBoundedIntegral (-100, 100)) (/= 42)` → `Result.Failure` exposes `failingTestCase` + `numShrinks`.
- **hedgehog**: `Gen.integral (Range.linear (-100) 100)` + `assert (x /= 42)` → `Report` with `reportShrinks` and rendered counterexample.

## Methodology for extension

- **Growing the spec language**: turn `Domain` into a small GADT; add `DList`, `DTuple`, `DOptional` as zizek generators land. When the zizek adapter has no generator for a domain, return `BackendResult { status = Skipped, ... }` so QC/hedgehog comparisons remain useful — don't fail the whole harness.
- **Shrink metric for structured types**: extend `distance :: Domain a -> a -> a -> Integer` per-domain (list length delta + elementwise; tree node count or depth). Where no natural numeric distance exists, log only counterexample text and tag `distance_from_min = null`; comparison is offline + manual.
- **Sequential execution**: all three bench groups run sequentially for fair wall-clock comparison.
- **Phase attribution**: run zizek twice per spec — once with `phases = [Generate]` and once with `[..., Shrink]` — to attribute wall-time to the Shrink phase specifically.
- **Error-message normalization**: capture verbatim `Text` from each library; the adapter additionally classifies into an `errorKind` enum (`Predicate | TypeMismatch | Discard | HealthCheck | Crash`). No lexical cross-library comparison in the harness — that happens offline if/when we need it.
- **Shrink-step parity**: QC (`numShrinks`) and hedgehog (`reportShrinks`) expose shrink counts directly; zizek does not. Follow-up: add a counter inside `Hegel.Runner.runEventLoop` / `replayFinalCases`. Until then, judge zizek shrink quality by `distance_from_min` alone.
- **Perf data for offline analysis**: when we need to crunch numbers, run `cabal bench eval -- --csv eval.csv`. No bespoke metrics file format until tasty-bench's CSV proves insufficient.

## Verification

```
cabal build zizek:zizek-eval
cabal test eval
cabal bench eval
```

A passing first iteration means:

1. `cabal test eval` exits 0; all three backends returned `Failed` with counterexample `42`, `distance_from_min == 0`, non-empty `error` (printed to stdout for inspection).
2. `cabal bench eval` prints a `tasty-bench` table with `bcompare` lines showing zizek-vs-QC-vs-hedgehog wall-time on the integer property.

## Follow-ups out of scope here

- Expose a shrink-step counter from `Hegel.Runner` so zizek can report `shrink_steps` alongside QC/hedgehog.
- Grow the spec corpus past `Integer` as `Bool`/`List`/`Text`/`Dict` generators ship.
- Revisit structured per-run result storage (sqlite, probably) once corpus and run history warrant it.
