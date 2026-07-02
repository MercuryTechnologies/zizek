# AGENTS.md

This file provides guidance to Claude Code (claude.ai/code) and other coding agents when working with code in this repository.

## Overview

This is the Haskell library for Hegel, a universal property-based testing framework. The library drives a Hypothesis-style engine in-process via FFI to the `libhegel` C library.

```bash
just check                                           # run check-format + build + test (CI gate)
just test                                            # run tests
just test <name>                                     # run a specific test suite (e.g. just test ffi)
just lint                                            # STUB: run linters (add hlint to flake.nix first)
just format                                          # run formatters (cabal, Haskell, Nix)
just check-format                                    # check formatting without modifying files
just docs                                            # build API docs via haddock
just check-conformance                               # run python conformance tests (builds binaries, then runs pytest tests/conformance/)
just check-coverage                                  # STUB: check coverage (add hpc-codecov to flake.nix first)
just profile-run <scenario>                          # smoke-run a profiling scenario on the dev build
just profile-space <scenario>                        # capture .prof/heap/eventlog into profiles/O<n>/ (prof_opt=0 for -O0)
just profile-time                                    # hyperfine wall-clock of all scenarios on the default -O1 build
just profile-time-compare                            # per-scenario -O1 vs -O0 A/B into profiles/compare/
cabal test zizek:unit --test-options='--pattern "name"'  # run a single test (tasty --pattern glob)
```

Minimum supported GHC version is 9.10 (enforced in CI and `zizek.cabal`). If you bump it, also bump `ci.yml`.

## Package Structure

- `library/Hegel.hs` — Public API: `prop`/`forEach`/`forEachWith`; re-exports `Gen`, settings, database, reports, phases, and assertions
- `library/Hegel/Property.hs` — Property monad public API: `PropertyT`/`Property`, `forAll`/`forAllWith`/`forAllSilent`, `annotate`/`footnote`, `assume`/`discard`, `check`/`check_`, `assert`/`failure`, `(===)`/`(/==)`. Internals in `library/Hegel/Property/Internal.hs`
- `library/Hegel/Stateful.hs` — stateful (model-based) testing: `Machine`/`Rule`/`Invariant` and `run`, layered on `PropertyT` (see Stateful Testing below)
- `library/Hegel/Pool.hs` — engine-managed pools of values for stateful rules to draw from; an empty-pool draw discards the case
- `library/Hegel/Report.hs` — `Report`/`Result`/`Stats` plus the plain/ANSI renderers: what a property run produces
- `library/Hegel/Report/*.hs` — the rich source-splicing renderer: `Ann` (annotations/styles), `Discovery` (declaration lookup), `Source` (splicing/layout), `Span`, `Note` (journal entries), `Journal` (depth regrouping + structured journal rendering), `Stateful` (splices the failing step's journal into source; eyeball via `cabal run demo-stateful-rich`)
- `library/Hegel/Diff.hs` — structural and line-level diffs backing `(===)` failures
- `library/Hegel/Assertion.hs` — `assert`/`failure` (`MonadIO`-polymorphic, call-stack-aware), failure-origin formatting
- `library/Hegel/Hspec.hs`, `library/Hegel/Tasty.hs` — framework integrations with automatic database keying (see Framework Integrations below)
- `library/Hegel/Settings.hs` (with `Backend`, `Database`, `HealthCheck`, `Phase`, `Verbosity`) — run configuration
- `library/Hegel/Runner.hs` — `check`: drives the `libhegel` engine, applies `Settings`, pumps test cases, replays reproduction blobs
- `library/Hegel/Gen.hs` — Umbrella re-export; designed for `import Hegel.Gen qualified as Gen`
- `library/Hegel/Gen/Internal.hs` — `Gen` GADT, `BasicGenerator`, combinators (`oneOf`, `filtered`, `assume`, `draw`)
- `library/Hegel/Gen/Builder.hs` — `Build`, `HasMin`, `HasMax`, `HasSize` typeclasses
- `library/Hegel/Gen/*.hs` — per-category builders (bool, integer, float, binary, char, text, regex, uri, uuid, list, set, map, …)
- `library/Hegel/Collection.hs` — `libhegel`-managed variable-length collection handle, used by the list/set/map generators
- `library/Hegel/Internal/FFI.hsc` — raw `foreign import ccall` bindings to `libhegel`: all `hegel_*` C functions, opaque handle types, `HEGEL_*` pattern synonyms, and bracket helpers
- `library/Hegel/Internal/TestCase.hs` — the `TestCase` handle (context + `hegel_test_case_t*` pointer) plus `markComplete`/`Status`
- `library/Hegel/Internal/DataSource.hs` — the generator-facing engine channel: `generate`, spans (`startSpan`/`stopSpan`, `Label`), collections, pools, state machines
- `library/Hegel/Internal/Control.hs` — control signals (`AssumeRejected`/`TestStopped`) and the exception-discipline helpers (`catchControl`/`onFailure`/`isFailure`/`tryProperty`)
- `library/Hegel/Internal/{Schema,CBOR,CString,DatabaseKey}.hs` — CBOR schema types, encoding helpers, C-string marshalling, database-key derivation

## Module Style

Prefer a module structure that allows functions to be imported fully qualified, with standalone types that are meant to be imported on their own.

For example:

```haskell
import Hegel.Collection (Collection)
import Hegel.Collection qualified as Collection
```

...which brings `Collection.new :: TestCase -> Collection` into scope.

### Generator builder pattern

Generators are built via a fluent builder API. `Gen.integral`, `Gen.double`, etc. are *builders* that accumulate constraints via `&`-chained modifiers and materialise with `& Gen.build`. The integral builder has type-pinned aliases — `Gen.int`, `Gen.int8`–`Gen.int64`, `Gen.word`–`Gen.word64` — so element types are usually fixed by alias rather than by type application (`Gen.int`, not `Gen.integral @Int`):

```haskell
import Data.Function ((&))
import Hegel.Gen qualified as Gen

g1 = Gen.int    & Gen.min 0 & Gen.max 100            & Gen.build
g2 = Gen.double & Gen.min 0 & Gen.max 1              & Gen.build
g3 = Gen.double & Gen.disallowNan                    & Gen.build
g4 = Gen.binary & Gen.minSize 4 & Gen.maxSize 64     & Gen.build
g5 = Gen.bool                                        & Gen.build
```

The `Build`, `HasMin`, `HasMax`, and `HasSize` typeclasses in `Hegel.Gen.Builder` provide the shared modifier vocabulary; builder-specific modifiers are plain functions on their builder type (float: `exclusiveMin`/`exclusiveMax`/`disallowNan`/`disallowInfinity`; char: `minCodepoint`/`categories`/…; regex: `fullMatch`/`alphabet`; uuid: `version`; bool: `weighted`). Applying an inapplicable modifier (e.g. `Gen.uuid & Gen.min 0`) is a type error. There are no `*Options` records on the public API.

Builder families beyond the sample above: `text`, `char`, `regex`, `uuid`, `uri`/`uriText`, `domain`, and the collections (`list` with `unique`, `set`/`hashSet`/`intSet`, `map`/`hashMap`/`intMap`). Choice and conditional combinators (`oneOf`, `element`, `frequency`, `filtered`, `enum`/`enumBounded`, `maybe`, `either`) are not builders — they produce `Gen` values directly, with no `& Gen.build`.

## Architecture

### How It Works

`zizek` drives the Hypothesis engine in-process via FFI to `libhegel`. The engine owns sampling, choice-sequence bookkeeping, and integrated shrinking; `zizek` describes what to generate using CBOR schemas and interprets the engine's replies.

There are two property-writing surfaces, both yielding a `Report`: the simple `prop settings gen body` API (sugar over `forEach`), and `check settings property`, where a `Property` interleaves `forAll` draws, effects, and assertions (see `Hegel.Property`). Stateful testing is not a third surface: `Stateful.run machine` is an ordinary `PropertyT` action run via `check`.

### Protocol

CBOR is the wire vocabulary between `zizek` and `libhegel`. For each test case:
1. `generate` sends a CBOR schema to the engine and receives back a CBOR value
2. `startSpan`/`stopSpan` bracket groups of related draws so the engine can shrink them as a unit
3. `markComplete` reports the outcome (VALID, INVALID, or INTERESTING) at the end of each test case

All three are FFI calls into `libhegel` via `Hegel.Internal.FFI`, wrapped by `Hegel.Internal.DataSource`/`Hegel.Internal.TestCase`.

### `Gen` GADT and `BasicGenerator`

`Gen a` is a GADT (not a typeclass) defined in `Hegel.Gen.Internal`. Key operations:
- `draw :: TestCase -> Gen a -> IO a` — Produce a value from a live test case
- `toBasic :: Gen a -> Maybe (BasicGenerator a)` — Returns a CBOR schema + parse function when the generator can be satisfied in a single round-trip

When `toBasic` returns `Just`, generation uses a single request with the schema. When `Nothing` (after `>>=` on non-basic generators, or `filtered`), it falls back to multiple requests wrapped in spans for shrinking.

`fmap` on a `BasicGenerator` preserves the schema by composing the transform into the parse function, rather than promoting to a non-basic generator.

### Span System

Spans (`start_span`/`stop_span`) group related generation calls so the engine can shrink them as a unit. The `Label` type in `Hegel.Internal.DataSource` identifies span types (LIST, TUPLE, ONE_OF, FILTER, etc.).

### Collections

`libhegel`-managed collections (`Collection.new`/`Collection.more`/`Collection.reject` in `Hegel.Collection`) drive variable-length generation; the list/set/map generators are built on them. Rejecting duplicates requires variable-size mode — see Note [Variable-size mode required for reject] in `Hegel.Collection`.

### Stateful Testing

`Hegel.Stateful.run` drives a `Machine` (initial state, `Rule`s, `Invariant`s) inside an ordinary property. The engine owns rule selection (including swarm testing: per-test-case rule subsets), the step cap, and shrinking; invariants are checked after every successful step, and a failing assertion is journaled in-band at the step that produced it. Replay alignment is load-bearing: every draw (including the step cap) is part of the choice sequence and happens unconditionally, on replay too — skipping one misaligns every later draw and the counterexample stops reproducing. `Hegel.Pool` provides engine-managed value pools for rules to draw from.

### Framework Integrations

`Hegel.Hspec.prop` and `Hegel.Tasty.testProperty` derive a stable example-database key from the module plus the test's describe/name path, and enable database persistence (plain `defaultSettings`/`def` leave it off). Renaming a test or its group orphans its stored failures. Caveat: a tasty leaf cannot see its enclosing `testGroup`, so identically-named `testProperty` leaves in one module collide on the same key. Stored replays only reproduce against deterministic fixtures.

### Test Suites

- `tests/unit/` — the `unit` cabal suite (tasty wrapping hspec specs): generators, schemas, property checks, report/source rendering, control signals, stateful, database replay, framework integrations
- `tests/ffi/` — the `ffi` cabal suite: wire-level checks, plus a closed-world guard (`cbits/wire_enum_guard.c`, compiled with `-Werror=switch-enum`) that fails the build if `libhegel` adds an enum variant
- `tests/conformance/` — Haskell binaries (one per generator category, plus `stateful` and `origin-deduplication`) invoked by the Python runner in `tests/conformance/pytest/test_conformance.py`, which validates generators produce values matching their declared constraints. Built binaries are symlinked into `tests/conformance/pytest/bin/`
- `tests/profile/` — the `profile-hegel` executable: deterministic named workloads for profiling the Haskell-side hot paths, driven by the `just profile-*` recipes. Not a test suite — a completed run always exits 0. Scenario table and interpretation guide: `notes/decisions/profiling-harness.md`

## Miscellaneous Conventions

- Use jujutsu (`jj`) for version control.
- **Prototype loose, land tight**: while a workflow's design is still moving, driving `cabal` (or other tools) by hand is fine. Once it solidifies, fold the surviving invocations into `scripts/` + `justfile` recipes — the justfile is the discoverable surface, and one-off invocations in a transcript force the next session (human or agent) to rediscover them.
- **Exception discipline**: Hegel's control signals (`AssumeRejected`, `TestStopped`) are async exceptions precisely so user catch-alls pass them through. Never hand-roll a `catch @SomeException` (or a base `try @SomeException`) around code that draws or asserts — it would swallow the discard/stop signals and corrupt the run. Use `Hegel.Internal.Control` (`catchControl`, `onFailure`, `tryProperty`) instead.
- Design work is planned in `notes/` (e.g. `notes/01-stateful-test-reporting.md`). Read the relevant note before starting work it covers, and keep it current as decisions change.
- `references/hegel-rust/` vendors the Rust/C engine reference (`hegel-c/include/hegel.h`, `src/stateful.rs`, …). It is the ground truth for engine semantics when Haskell-side documentation and behavior disagree.
