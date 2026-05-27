# AGENTS.md

This file provides guidance to Claude Code (claude.ai/code) and other coding agents when working with code in this repository.

## Overview

This is the Haskell library for Hegel, a universal property-based testing framework. The library communicates with a Python server (powered by Hypothesis) over stdin/stdout pipes to a child process to generate test data.

```bash
just check                                           # run format-check + build + test (CI gate)
just test                                            # run tests
just test suite=<name>                               # run a specific test suite
just lint                                            # STUB: run linters (add hlint to flake.nix first)
just format                                          # run formatters (cabal, Haskell, Nix)
just format-check                                    # check formatting without modifying files
just docs                                            # build API docs via haddock
just check-conformance                               # run python conformance tests (builds binaries, then runs pytest tests/conformance/)
just check-coverage                                  # STUB: check coverage (add hpc-codecov to flake.nix first)
cabal test zizek:unit --test-options='--pattern "name"'  # run a single test (tasty --pattern glob)
```

Minimum supported GHC version is 9.10 (enforced in CI and `zizek.cabal`). If you bump it, also bump `ci.yml`.

## Package Structure

- `library/Hegel.hs` — Public API
- `library/Hegel/Protocol.hs` — Binary protocol: packet encoding/decoding, stream multiplexing
- `library/Hegel/Runner.hs` — Spawns hegel CLI, manages the child process connection
- `library/Hegel/Gen.hs` — Umbrella re-export; designed for `import Hegel.Gen qualified as Gen`
- `library/Hegel/Gen/Internal.hs` — `Generator` GADT, `BasicGenerator`, `pattern Schema`, combinators (`oneOf`, `filtered`, `assume`, `draw`)
- `library/Hegel/Gen/Builder.hs` — `Build`, `HasMin`, `HasMax`, `HasSize` typeclasses
- `library/Hegel/Gen/Bool.hs` — `bool :: BoolBuilder`
- `library/Hegel/Gen/Integer.hs` — `integer :: IntegerBuilder a` (implements `HasMin`, `HasMax`, `Build`)
- `library/Hegel/Gen/Float.hs` — `float :: FloatBuilder Float`, `double :: FloatBuilder Double`; `exclusiveMin`, `exclusiveMax`, `disallowNan`, `disallowInfinity` modifiers
- `library/Hegel/Gen/Binary.hs` — `binary :: BinaryBuilder` (implements `HasSize`, `Build`)

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

The library spawns the `hegel` CLI as a child process and communicates over its stdin/stdout handles. A single persistent connection is maintained for the program run, supporting multiple test executions.

### Protocol

CBOR-encoded binary protocol over multiplexed streams. For each test:
1. Client sends `run_test` request on control stream (stream 0)
2. Server sends `test_case` events with stream IDs for each test case
3. Client runs the test function, sending `generate`/`start_span`/`stop_span` requests on the test stream
4. Client sends `mark_complete` with status (VALID, INVALID, or INTERESTING)
5. After all test cases, server sends `test_done` with results

### `Generator` GADT and `BasicGenerator`

`Generator a` is a GADT (not a typeclass) defined in `Hegel.Gen.Internal`. Key operations:
- `draw :: Generator a -> TestCase -> IO a` — Produce a value from a live test case
- `asBasic :: Generator a -> Maybe (BasicGenerator a)` — Returns a CBOR schema + parse function when the generator can be satisfied in a single round-trip

When `asBasic` returns `Just`, generation uses a single request with the schema. When `Nothing` (after `>>=` on non-basic generators, or `filtered`), it falls back to multiple requests wrapped in spans for shrinking.

`fmap` on a `BasicGenerator` preserves the schema by composing the transform into the parse function, rather than promoting to a non-basic generator.

### Server Session

The test suite initializes a global session variable that holds `hegel-core` server & control stream references as well as a handle to the child process that spawned `hegel-core`.

### Span System

Spans (`start_span`/`stop_span`) group related generation calls so Hypothesis can shrink effectively. Labels in `Hegel.Labels` identify span types (LIST, TUPLE, ONE_OF, FILTER, etc.).

### Collections

Server-managed collections use `Collection.new`/`Collection.more`/`Collection.reject` protocol commands. The Collection protocol is not yet implemented in this library; it is planned for a future phase.

### Conformance Tests

Located in `tests/conformance/`. Haskell test binaries in `tests/conformance/haskell/` are invoked by a Python test runner (`tests/conformance/test_conformance.py`) that validates generators produce values matching their declared constraints.

## Miscellaneous Conventions

Use jujutsu (`jj`) for version control.
