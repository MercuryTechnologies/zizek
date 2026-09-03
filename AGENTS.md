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
- `library/Hegel/Property.hs` — Property monad public API: `PropertyT`/`Property`, `forAll`/`forAllWith`/`forAllWithLabel`/`forAllSilent`, `annotate`/`footnote`, `assume`/`discard`, `registerFinalizer`, `check`/`check_`, `assert`/`failure`, `(===)`/`(/==)`. Internals in `library/Hegel/Property/Internal.hs`. `registerFinalizer act` queues per-case cleanup drained (LIFO) at the case boundary on every exit and replay; a throwing finalizer aborts the run as `Errored`. `forAllWithLabel "qty" g` labels a draw so the report reads `restock item="apple" qty=5` instead of a bare positional value — the fix for cryptic rule rows (no source parsing).
- `library/Hegel/Property/Fork.hs` — an escaping fork of a property body running concurrently on its own cloned choice stream: `spawn`/`join`/`cancel`/`poll`, plus `scoped` for a fork whose lifetime is bounded to one block. An unjoined-or-cancelled fork aborts the run as a malformed test.
- `library/Hegel/Property/Branch.hs` — fixed-arity and list-shaped concurrent branches of a property, each on its own cloned choice stream and joined before the combinator returns: `concurrently`/`mapConcurrently`/`forConcurrently`/`replicateConcurrently` and their `_`/`Bounded` variants. A branch failure surfaces as an ordinary shrinkable counterexample, and every failing branch's message/location/diff renders in the report, not only the one that wins the shrink target.
- `library/Hegel/Stateful.hs` — stateful (model-based) testing: `Machine`/`Rule`/`Invariant` and `run`, layered on `PropertyT` (see Stateful Testing below). `run` drives `Hegel.Internal.StatefulRound`'s round protocol with a single worker, bridging `Rule`'s by-value state onto that module's `IO`-shaped dispatch through a mutable cell
- `library/Hegel/Stateful/Concurrent.hs` — concurrent stateful testing: `Rule`/`Machine`/`run` (re-exporting `Hegel.Stateful.Invariant` rather than duplicating it) and `Concurrency`, built with `fixed`/`upTo`/`between`. A separate `Rule` type from sequential `Hegel.Stateful.Rule`, since `apply :: s -> PropertyT m ()` shares the model by reference across workers rather than threading an updated value back. `Rule.group :: Maybe Text` partitions which rules may run concurrently; `Nothing` places a rule in the anonymous group shared by every other ungrouped rule, itself never overlapping a named one. A failure from a run whose bounds ever allow more than one worker reports the engine's failure origin and an `Unreproducible` footer, not the step history `Hegel.Stateful.run` gives (see Stateful Testing below)
- `library/Hegel/Pool.hs` — engine-managed pools of values for stateful rules to draw from; an empty-pool draw is equivalent to `assume False`, which skips just the step it's drawn in inside a stateful rule's body, and discards the whole case anywhere else. `named` labels a pool's values for the report; `transfer` moves a value between pools with the identity link declared (one lifeline across pools)
- `library/Hegel/Report.hs` — `Report`/`Result`/`Stats`/`Reproduction` plus the plain/ANSI renderers: what a property run produces; for step-structured (stateful) failures the rich path composes the report (event log + splice + footer). `Reproduction` names where a failure from the run can be found again — `Stored key`, `Unstored`, or `Unreproducible` — and its footer renders on every failure render path, not only the stateful one
- `library/Hegel/Report/*.hs` — the rich renderers: `Ann` (annotations/styles), `Discovery` (declaration lookup), `Source` (splicing/layout), `Span`, `Note` (journal entries; also `renderValue`), `Journal` (depth regrouping + structured rendering), `Stateful` (the failing step's source splice), `Style` (glyphs, phrases, and layout budgets in one record; `HEGEL_GLYPHS` + encoding detection pick ascii); and `Trace` (the IR zipping journal + pool events on their shared `Tick`) with `Layout` (the flat chronological event log: one row per step, a bare `✗`/blank gutter, touch-irrelevant runs collapsed into a single elision row, and a concurrent stateful step's round/worker tag in its own dim right-aligned column; also owns `displayName`). Eyeball via `just gallery`, eleven scenarios spanning plain, branch/fork, sequential stateful, and concurrent stateful reports
- `library/Hegel/Diff.hs` — structural and line-level diffs backing `(===)` failures
- `library/Hegel/Assertion.hs` — `assert`/`failure` (`MonadIO`-polymorphic, call-stack-aware), failure-origin formatting
- `library/Hegel/Hspec.hs`, `library/Hegel/Tasty.hs` — framework integrations with automatic database keying (see Framework Integrations below)
- `library/Hegel/Settings.hs` (with `Backend`, `Database`, `HealthCheck`, `Phase`, `Verbosity`) — run configuration
- `library/Hegel/Runner.hs` — `check`: drives the `libhegel` engine, applies `Settings`, pumps test cases, replays reproduction blobs. A run the engine declared nondeterministic carries no reproduce blob and never replays, so `runTestCase` polls `hegel_test_case_is_nondeterministic` per case and, once flagged, runs it under a live `Recording` journal (`Hegel.Property.Internal.newRecordingJournal`) instead of `Silent`; a case classified `Interesting` stashes its exception together with whatever it recorded as one `LiveFailure`, written only on that classification and read back only if the run concludes `RunNondeterministic`, so the reported message, location, diff, and journal always come from the one case that actually failed rather than a bare dedup key or a different, unrelated case's capture
- `library/Hegel/Gen.hs` — Umbrella re-export; designed for `import Hegel.Gen qualified as Gen`
- `library/Hegel/Gen/Internal.hs` — `Gen` GADT, combinators (`oneOf`, `filtered`, `assume`, `draw`), `enumerate`
- `library/Hegel/Gen/Builder.hs` — `Build`, `HasMin`, `HasMax`, `HasSize` typeclasses
- `library/Hegel/Gen/*.hs` — per-category builders (bool, integer, float, binary, char, text, regex, uri, uuid, list, set, map, …); `Hegel.Gen.Recursive` builds recursively defined data over an engine-owned depth cap, leaf budget, and retry protocol instead of a client-side loop
- `library/Hegel/Collection.hs` — `libhegel`-managed variable-length collection handle, used by the list/set/map generators
- `library/Hegel/Internal/Tick.hs` — the recording substrate: a monotonic per-case sequence stamp (`Tick`) plus the `Silent`/`Active` toggle and the generic gated `record`/`drain`/`drainAndReset`, shared by the note journal and the pool-event stream. Domain-agnostic (knows nothing of pools, notes, or state machines); records only when a case's story must survive past the case itself — the final reconstruction replay, or a live case `Hegel.Runner.check` has recognized as belonging to a run the engine already declared nondeterministic. `drainAndReset` additionally clears the buffer, for a caller (`Hegel.Stateful.Concurrent.run`'s round fold) that harvests a live buffer more than once per case rather than once at the end
- `library/Hegel/Internal/Event.hs` — the per-case pool-event stream (`Event`/`Operation`/`Var`), stamped via `Tick`
- `library/Hegel/Internal/Foreign/*.hs(c)` — the `libhegel` interop: `Raw` (raw `foreign import ccall` bindings — all `hegel_*` C functions, opaque handle types, `HEGEL_*` pattern synonyms, bracket helpers) and `CString` (C-string marshalling) feeding it
- `library/Hegel/Internal/TestCase.hs` and `library/Hegel/Internal/DataSource.hs` — the per-test-case engine interaction: `TestCase` (the handle — context + `hegel_test_case_t*` pointer — carrying the recording toggle, plus `markComplete`/`Status`, `withClone`/`withClonePair`/`withClones` for acquiring one or more clones in a fixed order) and `DataSource` (the generator-facing channel: `generate`, spans (`startSpan`/`stopSpan`, `Label`), collections, pools, state machines — `newStateMachine` fixes concurrency at 1,1 without consuming entropy; `newConcurrentStateMachine` generalizes it to explicit rule groups and concurrency bounds)
- `library/Hegel/Internal/Control.hs` — control signals (`AssumeRejected`/`TestStopped`/`LeafBudgetExceeded`/`AttemptMispriced`) and the exception-discipline helpers (`catchControl`/`onFailure`/`isFailure`/`tryProperty`)
- `library/Hegel/Internal/StatefulRound.hs` — the engine's round-based state-machine protocol, generalized over any number of workers: `Worker` bundles a test case with an `IO`-shaped rule dispatch; `runRound` fans every worker's `runWorkerRound` out concurrently over already-acquired clones and resolves the round via `resolveRound`'s precedence (a control error outranks an overrun or an invalid conclusion, which outrank a panic, lowest worker index wins among panics). `Hegel.Stateful.run` is the one-worker caller today; nothing public drives more than one worker yet
- `library/Hegel/Internal/DatabaseKey.hs` — database-key derivation

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

## Documentation & Comment Style

Haddocks and comments describe what a thing is *for* — its contract — not what the code does. These rules are ordered; earlier ones win.

1. **State the contract, not the mechanism.** What a caller must satisfy and what they get back. Function-internal reasoning ("marshals to an unsigned wire type where a negative `Int` wraps", "NaN compares `False`, so the ordering check would accept it", "GHC folds the check away") belongs in the code, not the Haddock — the reader opens the source for the how.

2. **Never narrate the motivating incident.** The code's present purpose is the whole story. Cut "the exact mistake the UUID near-miss caught", "since libhegel 0.28.0 … rather than clamping", "before this coverage existed, nothing tested…".

3. **Define positively, not by contrast.** State what a thing *is*, not what it isn't. If a contrast isn't load-bearing, the opening positive sentence stands alone — delete the rest.
   - Bad: `The module defining the builder, not the user-facing @Gen.text@ spelling.`
   - Good: `The fully-qualified module of the builder that raised it, e.g. @"Hegel.Gen.Text"@.`

4. **Avoid parentheses.** An aside listing examples or caveats reads better as a plain clause, or cut. Keep foreign syntax out (no Rust `4..=255` in a Haskell Haddock — write `in @[4, 255]@`).

5. **Avoid em-dashes; don't launder them.** Swapping an em-dash for a colon or semicolon is not a fix. Rephrase so the sentence runs without the interruption. A comma-offset mid-sentence interjection ("but only when both bounds are set, for builders where…") reads badly — rewrite it to flow.

6. **Write sentences, not compressed fragments.** LLM prose compresses for density; a human writes a plain sentence with a subject and a verb. The tells: a dropped-subject, verb-first fragment ("Hoisted out of the closure: …", "Confirms the guard fires…"); a fronted noun phrase with a colon carrying a stack of clauses; semicolons chaining facts into one telegraphic line; a stock flourish standing in for the actual reason ("a real win", "earns its place/keep"). Say who does what and why.
   - Bad: `Hoisted out of the 'Draw' closure: a real per-draw allocation win at @-O0@; full laziness already does this at @-O1@.`
   - Good: `Bound here, not inside the lambda, so this isn't reallocated on every draw.`
   - Bad: `Confirms the guard fires on misuse, not just that ordinary in-bounds usage still works.`
   - Good: `These cases cover ordinary in-bounds usage and confirm the guard fires on misuse.`

7. **Prefer one sentence.** Condense before adding a second.

8. **Visual weight matches content weight.** One idea, one line-group. A second sentence that carries real weight gets its own paragraph — set it off with a blank `--` line. One that doesn't gets cut.
   - Bad: `Require @n >= 0@, throwing 'GenValidationError' otherwise. Size bounds marshal to unsigned wire types, where a negative 'Int' silently wraps to a huge value.`
   - Good: `Require @n >= 0@ for a size or codepoint bound, throwing 'GenValidationError' otherwise.`

9. **No internal references leak into shipped docs.** Never cite `notes/` paths, `AGENTS.md`, or other repo-internal conventions from a Haddock or comment. Point at code and public API only.

10. **Don't restate the module path.** A module under `Hegel.Internal.*` is already marked internal by its name, so it needs no `__Internal module.__` banner or "may change without notice" boilerplate. Lead with the one-line summary of what it provides.

Genuine invariants and warnings stay: why `finally` must guard a buffer free, why an exclusive bound has to be strictly ordered. The test is whether the reader needs it to *use* the thing correctly, not to understand how it works inside.

## Architecture

### How It Works

`zizek` drives the Hypothesis engine in-process via FFI to `libhegel`. The engine owns sampling, choice-sequence bookkeeping, and integrated shrinking; `zizek` calls a dedicated FFI entry point per generator kind and interprets the result directly, with no intermediate schema or encoding step.

There are two property-writing surfaces, both yielding a `Report`: the simple `prop settings gen body` API (sugar over `forEach`), and `check settings property`, where a `Property` interleaves `forAll` draws, effects, and assertions (see `Hegel.Property`). Stateful testing is not a third surface: `Stateful.run machine` is an ordinary `PropertyT` action run via `check`.

### Protocol

Each generator kind has its own FFI entry point, with parameters marshalled directly rather than through an intermediate wire encoding. For each test case:
1. a draw asks the engine for a value: a scalar draw (`drawBool`, `drawInteger`, `drawFloat`, `drawBytes`, `drawUuid`, `drawDate`, `drawTime`, `drawDatetime`) returns it directly, while a string draw first builds a native string-generator handle (`buildTextGen`, `buildRegexGen`, `buildEmailGen`, `buildUrlGen`, `buildDomainGen`) that the engine samples and shrinks against internally
2. `startSpan`/`stopSpan` bracket groups of related draws so the engine can shrink them as a unit
3. `markComplete` reports the outcome (VALID, INVALID, or INTERESTING) at the end of each test case

All of these are FFI calls into `libhegel` via `Hegel.Internal.Foreign.Raw`, wrapped by `Hegel.Internal.DataSource`/`Hegel.Internal.TestCase`.

### `Gen` GADT

`Gen a` is a GADT (not a typeclass) defined in `Hegel.Gen.Internal`, with constructors `Pure`, `Draw` (an opaque `TestCase -> IO a` leaf — every leaf generator and combinators like `filtered`/`frequency` bottom out here), `Map`, `Ap`, `Bind`, and `OneOf`. `draw :: TestCase -> Gen a -> IO a` produces a value from a live test case.

The GADT structure is interpreted, not just executed: `runInteractive` walks the constructors to decide span nesting for shrinking — `Map` opens MAPPED, `Ap` opens TUPLE (only with ≥2 non-`Pure` leaves), `Bind` opens FLAT_MAP, `OneOf` opens ONE_OF.

`enumerate :: Gen a -> Maybe [a]` walks `Pure`/`Map`/`Ap`/`OneOf` to return a generator's finite value set when statically knowable (`Nothing` at any `Draw` or `Bind`). `filtered`/`mapMaybe` use it as a single-round-trip fast path over finite sources, falling back to a bounded retry loop otherwise.

### Span System

Spans (`start_span`/`stop_span`) group related generation calls so the engine can shrink them as a unit. The `Label` type in `Hegel.Internal.DataSource` identifies span types (LIST, TUPLE, ONE_OF, FILTER, etc.).

### Collections

`libhegel`-managed collections (`Collection.new`/`Collection.more`/`Collection.reject` in `Hegel.Collection`) drive variable-length generation; the list/set/map generators are built on them. Rejecting duplicates requires variable-size mode — see Note [Variable-size mode required for reject] in `Hegel.Collection`.

### Recursive Generation

`Gen.recursive` (`Hegel.Gen.Recursive`) generates recursively defined data, such as trees or JSON documents, from a leaf generator and a branch function over sub-values. The engine owns branch probability, the depth cap (`maxDepth`), the leaf budget (`maxLeaves`), and the per-value target size, driven through `hegel_new_recursion`/`hegel_recursion_branch`/`hegel_recursion_leaf`/`hegel_recursion_finish`/`hegel_recursion_retry`. Two distinct situations both signal through `HEGEL_E_RETRY`: outgrowing the leaf budget (from `hegel_recursion_leaf`) throws `LeafBudgetExceeded`, and a completed value the engine discarded as mispriced (from `hegel_recursion_finish`) throws `AttemptMispriced`; both are control signals in `Hegel.Internal.Control`, caught only by the retry loop that opened the recursion scope. The `RECURSIVE` span around each sub-value is opened and closed in plain sequence, never under a `bracket`-style guarantee, so either retry's unwind skips the closing `stopSpan` instead of closing a span the engine already discarded.

### Stateful Testing

`Hegel.Stateful.run` drives a `Machine` (initial state, `Rule`s, `Invariant`s) inside an ordinary property, on top of `libhegel`'s round-based state-machine protocol: the engine owns rule selection (including swarm testing: per-test-case rule subsets), the round and step caps, and shrinking. A round polls the engine for the next concurrency group, then pulls rules for that group until the engine signals the round's own budget is exhausted; a sequential machine has one group and today hands out one rule per round, so invariants are checked once per round, which is once per step. A round's draws share one `LabelStatefulRule` span, discarded when one of its rules was rejected, and a rejected rule is reported back to the engine so it does not count toward the step budget. A failing assertion is journaled in-band at the step that produced it. Replay alignment is load-bearing: every draw, and every poll for the next round or the next rule, is part of the choice sequence and happens unconditionally, on replay too — skipping one misaligns every later draw and the counterexample stops reproducing. `Hegel.Pool` provides engine-managed value pools for rules to draw from, safe under concurrent access from several workers sharing one pool: every read or mutation of a pool's mirror holds an `MVar` across the paired engine call, so a concurrent lookup can never observe a variable id the engine has assigned but the mirror hasn't recorded yet. `Hegel.Internal.StatefulRound` factors the round\/rule-pull loop itself out into machinery generalized over any number of workers, fanned out concurrently over their own persistent clones; `Hegel.Stateful.run` is its one-worker caller, and `Hegel.Stateful.Concurrent.run` is its multi-worker one.

`Hegel.Stateful.Concurrent.run` drives the same round protocol with more than one worker: the root test case still drives `next_group` and, once a round concludes, the invariant checks, while each worker dispatches rules against its own clone, acquired once and held for the whole case. Each worker opens its own `LabelStatefulRule` span around its own pull for the round, on its own clone, rather than sharing one span on the root: a round's rules draw on different clones, so a root span would not enclose any of them, and each clone's span stack is untouched by any other worker's. `Hegel.Internal.StatefulRound.Worker.roundSpan` (`Own` here, `Caller` for the sequential one-worker case, which keeps its own root-level span open across the `next_group` draw `runWorkerRound` never sees) is what tells `runWorkerRound` which of the two it's in. Each worker's `Env` gets its own private `Recording` journal into a per-clone buffer whenever the case itself is recording, mirroring `Hegel.Property.Internal.runBranch` rather than sharing one sink: a worker's clone runs its own `Tick` clock, so a shared sink would race on the buffer and interleave two independently-clocked streams into an order that means nothing; under an ordinary `Silent` case every worker stays `Silent` too, at no extra cost. `foldWorkerRound`, run by the root driver after every round regardless of its verdict (so a panicking worker's own step is captured before its exception concludes the case), drains each worker's notes and pool events in ascending worker order, restamps them onto the root's own clock, and gives each `StepHeader` its true global index; the round, worker, and concurrency group that ran it is carried structurally too, in a companion `StepOrigin` note the fold emits right after the header, since only the fold knows it, depending as it does on every worker's dispatches for the round being in hand and, for the group, a rule name → group lookup built from `machine.rules`. `Trace.Step.origin` lifts that note into the IR, `Nothing` for a sequential step, and `Hegel.Report.Layout` renders it in the event log's own dim, right-aligned column rather than folding it into the call text, e.g. `round 2, worker 2 (writers)`, with the group suffix omitted for an ungrouped rule. A rule's group id is interned to a dense `Int64` by first appearance and handed to `hegel_new_state_machine`'s `rule_groups` array; two rules in the same group may share a round, rules in different groups never do. Creating a machine whose bounds ever allow `max_concurrency > 1` declares the whole run nondeterministic per `libhegel`'s own contract, and the run's first such creation always discards that case; a failure from such a run reports the failing assertion's own message/location/diff (via `Hegel.Runner`'s live capture) plus, once any rule has dispatched, the same step-by-step trace `Hegel.Stateful.run` gives, and an `Unreproducible` reproduction footer. The fold is unconditional on the round's verdict, so a round that folds in steps after the failing one is expected: a sequential machine's failing step is always the log's last row, but a concurrent fold gives up that guarantee. Before the post-round invariant check, the root emits a `RoundBoundary` note carrying a fresh step index and the round number, which `Trace.segment` treats as a header exactly like `StepHeader`, so the invariant check runs in its own segment, e.g. rendered as `Step 11: round 2 invariant check`, rather than trailing whichever worker step the fold happened to place last; an invariant failure attaches to that boundary, not to an arbitrary worker step. `examples/gallery/Main.hs` scenarios 9 and 10 demonstrate both shapes; scenario 11 demonstrates named concurrency groups, a `"writers"` group and a `"readers"` group that never share a round.

### Framework Integrations

`Hegel.Hspec.prop` and `Hegel.Tasty.testProperty` derive a stable example-database key from the module plus the test's describe/name path, and enable database persistence (plain `defaultSettings`/`def` leave it off). Renaming a test or its group orphans its stored failures. Caveat: a tasty leaf cannot see its enclosing `testGroup`, so identically-named `testProperty` leaves in one module collide on the same key. Stored replays only reproduce against deterministic fixtures.

### Test Suites

- `tests/unit/` — the `unit` cabal suite (tasty wrapping hspec specs): generators, property checks, report/source rendering, control signals, stateful, pool events, trace/blame IR, ledger/verdict rendering, database replay, framework integrations
- `tests/ffi/` — the `ffi` cabal suite: wire-level checks, plus a closed-world guard (`cbits/wire_enum_guard.c`, compiled with `-Werror=switch-enum`) that fails the build if `libhegel` adds an enum variant
- `tests/string-gen-handles/` — the `string-gen-handles` cabal suite: asserts unreferenced `HegelStringGenerator` handles (see `Hegel.Internal.DataSource`) actually get GC-reclaimed. Isolated in its own process deliberately — the same assertion is flaky inside the shared, hundreds-of-tests `unit` binary (see `settleStringGenerators`'s haddock for why)
- `tests/profile/` — the `profile-hegel` executable: deterministic named workloads for profiling the Haskell-side hot paths, driven by the `just profile-*` recipes. Not a test suite — a completed run always exits 0. Scenario table lives in `tests/profile/` alongside the workloads.

## Miscellaneous Conventions

- Use jujutsu (`jj`) for version control.
- **Prototype loose, land tight**: while a workflow's design is still moving, driving `cabal` (or other tools) by hand is fine. Once it solidifies, fold the surviving invocations into `scripts/` + `justfile` recipes — the justfile is the discoverable surface, and one-off invocations in a transcript force the next session (human or agent) to rediscover them.
- **Exception discipline**: Hegel's control signals (`AssumeRejected`, `TestStopped`) are async exceptions precisely so user catch-alls pass them through. Never hand-roll a `catch @SomeException` (or a base `try @SomeException`) around code that draws or asserts — it would swallow the discard/stop signals and corrupt the run. Use `Hegel.Internal.Control` (`catchControl`, `onFailure`, `tryProperty`) instead.
- `references/hegel-rust/` vendors the Rust/C engine reference (`hegel-c/include/hegel.h`, `src/stateful.rs`, …). It is the ground truth for engine semantics when Haskell-side documentation and behavior disagree.
