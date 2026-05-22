# AGENTS.md

This file provides guidance to Claude Code (claude.ai/code) and other coding agents when working with code in this repository.

## Overview

This is the Haskell library for Hegel, a universal property-based testing framework. The library communicates with a Python server (powered by Hypothesis) over stdin/stdout pipes to a child process to generate test data.

```bash
just check                                          # UNIMPLEMENTED: run full CI checks
just test                                           # run tests
just lint                                           # UNIMPLEMENTED: run linters
just format                                         # run formatters
just docs                                           # UNIMPLEMENTED: build and open docs
just check-conformance                              # UNIMPLEMENTED: run python conformance tests
just check-coverage                                 # UNIMPLEMENTED: check coverage (requires cargo-llvm-cov + llvm-tools-preview)
cabal test hegel --test-options="--match test_name" # UNIMPLEMENTED: run a single test
```

Minimum supported GHC version is 9.10 (enforced in CI and hegel.cabal). If you bump it, also bump `ci.yml`.

## Package Structure

- `library/Hegel.hs` — Public API: 
- `library/Hegel/Protocol.hs` — Binary protocol: packet encoding/decoding, stream multiplexing
- `library/Hegel/Runner.hs` — Spawns hegel CLI, manages the child process connection
- `library/Hegel/Generators/` — All generator implementations
- `library/Hegel/Generators.hs` — `Generate` typeclass + `TestCaseData`
- `library/Hegel/TH.hs` — Template Haskell macros for deriving `Generate` instances

## Module Style

Prefer a module structure that allows functions to be imported fully qualified, with standalone types that are meant to be imported on their own.

For example:

```haskell
import Hegel.Collection (Collection)
import Hegel.Collection qualified as Collection
```

...which brings `Collection.new :: TestCase -> Collection` into scope.

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

### `Generator` Typeclass and `BasicGenerator`

Generators implement `Generator a`:
- `draw :: Generator a => a -> TestCase -> Output a` — Produce a value, where `Output` is an associated type of `Generator`
- `asBasic` — Returns `Maybe (BasicGenerator a)` with a CBOR schema + parse function

When `asBasic` returns `Just`, generation uses a single request with the schema. When `Nothing` (after `map`/`filter` on non-basic generators, or `>>=`), it falls back to multiple requests wrapped in spans for shrinking.

NOTE: `map` on a `BasicGenerator` preserves the schema by composing the transform function, rather than losing it.

### Server Session

The test suite initializes a global session variable that holds `hegel-core` server & control stream references as well as a handle to the child process that spawned `hegel-core`.

### Span System

Spans (`start_span`/`stop_span`) group related generation calls so Hypothesis can shrink effectively. Labels in `Hegel.Labels` identify span types (LIST, TUPLE, ONE_OF, FILTER, etc.).

### Collections

Server-managed collections use `Collection.new`/`Collection.more`/`Collection.reject` protocol commands. The `Collection` record in `Hegel.Collection` handles dynamic sizing via the `more()` protocol with lazy initialization.

### Conformance Tests

Located in `tests/conformance/`. Haskell test binaries in `tests/conformance/haskell/` are invoked by a Python test runner (`tests/conformance/test_conformance.py`) that validates generators produce values matching their declared constraints.

## Miscellaneous Conventions

Use jujutsu (`jj`) for version control.
