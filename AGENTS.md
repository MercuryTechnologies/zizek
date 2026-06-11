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
cabal test zizek:unit --test-options='--pattern "name"'  # run a single test (tasty --pattern glob)
```

Minimum supported GHC version is 9.10 (enforced in CI and `zizek.cabal`). If you bump it, also bump `ci.yml`.

## Package Structure

- `library/Hegel.hs` — Public API: `prop`/`forEach`/`forEachWith`; re-exports `Gen`, settings, database, reports, phases, and assertions
- `library/Hegel/Property.hs` — Property monad public API: `PropertyT`/`Property`, `forAll`/`forAllWith`/`forAllSilent`, `annotate`/`footnote`, `assume`/`discard`, `check`/`check_`, `assert`/`failure`, `(===)`/`(/==)`. Internals in `library/Hegel/Property/Internal.hs`
- `library/Hegel/Report.hs` — `Report`/`Result`/`Note`/`Stats`: what a property run produces
- `library/Hegel/Assertion.hs` — `assert`/`failure` (`MonadIO`-polymorphic, call-stack-aware), failure-origin formatting
- `library/Hegel/TestCase.hs` — thin newtype around a `hegel_test_case_t*` pointer plus the `generate`/`startSpan`/`stopSpan`/`markComplete` operations generators use to talk to `libhegel`
- `library/Hegel/FFI.hsc` — raw `foreign import ccall` bindings to `libhegel`: all `hegel_*` C functions, opaque handle types, `HEGEL_*` pattern synonyms, and bracket helpers
- `library/Hegel/Runner.hs` — `check`: drives the `libhegel` engine, applies `Settings`, pumps test cases, replays reproduction blobs
- `library/Hegel/Gen.hs` — Umbrella re-export; designed for `import Hegel.Gen qualified as Gen`
- `library/Hegel/Gen/Internal.hs` — `Gen` GADT, `BasicGenerator`, combinators (`oneOf`, `filtered`, `assume`, `draw`)
- `library/Hegel/Gen/Builder.hs` — `Build`, `HasMin`, `HasMax`, `HasSize` typeclasses
- `library/Hegel/Gen/*.hs` — per-category builders (bool, integer, float, binary, char, text, regex, uri, uuid, list, set, map, …)
- `library/Hegel/Collection.hs` — `libhegel`-managed variable-length collection handle, used by the list/set/map generators

## Module Style

Prefer a module structure that allows functions to be imported fully qualified, with standalone types that are meant to be imported on their own.

For example:

```haskell
import Hegel.Collection (Collection)
import Hegel.Collection qualified as Collection
```

...which brings `Collection.new :: TestCase -> Collection` into scope.

### Generator builder pattern

Generators are built via a fluent builder API. `Gen.integer`, `Gen.double`, etc. are *builders* that accumulate constraints via `&`-chained modifiers and materialise with `& Gen.build`:

```haskell
import Data.Function ((&))
import Hegel.Gen qualified as Gen

g1 = Gen.integer @Int & Gen.min 0 & Gen.max 100      & Gen.build
g2 = Gen.double       & Gen.min 0 & Gen.max 1        & Gen.build
g3 = Gen.double       & Gen.disallowNan              & Gen.build
g4 = Gen.binary       & Gen.minSize 4 & Gen.maxSize 64 & Gen.build
g5 = Gen.bool                                        & Gen.build
```

The `Build`, `HasMin`, `HasMax`, and `HasSize` typeclasses in `Hegel.Gen.Builder` provide the modifier vocabulary. Float-only modifiers (`exclusiveMin`, `exclusiveMax`, `disallowNan`, `disallowInfinity`) are plain functions on `FloatBuilder a`. Applying an inapplicable modifier (e.g. `Gen.uuid & Gen.min 0`) is a type error. There are no `*Options` records on the public API.

## Architecture

### How It Works

`zizek` drives the Hypothesis engine in-process via FFI to `libhegel`. The engine owns sampling, choice-sequence bookkeeping, and integrated shrinking; `zizek` describes what to generate using CBOR schemas and interprets the engine's replies.

There are two property-writing surfaces, both yielding a `Report`: the simple `prop settings gen body` API (sugar over `forEach`), and `check settings property`, where a `Property` interleaves `forAll` draws, effects, and assertions (see `Hegel.Property`).

### Protocol

CBOR is the wire vocabulary between `zizek` and `libhegel`. For each test case:
1. `generate` sends a CBOR schema to the engine and receives back a CBOR value
2. `startSpan`/`stopSpan` bracket groups of related draws so the engine can shrink them as a unit
3. `markComplete` reports the outcome (VALID, INVALID, or INTERESTING) at the end of each test case

All three are direct FFI calls into `libhegel` via `Hegel.FFI`.

### `Gen` GADT and `BasicGenerator`

`Gen a` is a GADT (not a typeclass) defined in `Hegel.Gen.Internal`. Key operations:
- `draw :: TestCase -> Gen a -> IO a` — Produce a value from a live test case
- `toBasic :: Gen a -> Maybe (BasicGenerator a)` — Returns a CBOR schema + parse function when the generator can be satisfied in a single round-trip

When `toBasic` returns `Just`, generation uses a single request with the schema. When `Nothing` (after `>>=` on non-basic generators, or `filtered`), it falls back to multiple requests wrapped in spans for shrinking.

`fmap` on a `BasicGenerator` preserves the schema by composing the transform into the parse function, rather than promoting to a non-basic generator.

### Span System

Spans (`start_span`/`stop_span`) group related generation calls so the engine can shrink them as a unit. The `Label` type in `Hegel.TestCase` identifies span types (LIST, TUPLE, ONE_OF, FILTER, etc.).

### Collections

`libhegel`-managed collections (`Collection.new`/`Collection.more`/`Collection.reject` in `Hegel.Collection`) drive variable-length generation; the list/set/map generators are built on them. Rejecting duplicates requires variable-size mode — see Note [Variable-size mode required for reject] in `Hegel.Collection`.

### Conformance Tests

Located in `tests/conformance/`. The Haskell test binaries (one per generator category, e.g. `tests/conformance/integers/Main.hs`) are invoked by the Python test runner in `tests/conformance/pytest/test_conformance.py`, which validates generators produce values matching their declared constraints. Built binaries are symlinked into `tests/conformance/pytest/bin/`.

## Miscellaneous Conventions

Use jujutsu (`jj`) for version control.
