# AGENTS.md

This file provides guidance to Claude Code (claude.ai/code) and other coding agents when working with code in this repository.

## Overview

This is the Haskell library for Hegel, a universal property-based testing framework. The library drives a Hypothesis-style engine in-process via FFI to the `libhegel` C library.

```bash
just check                                           # CI gate: check-format, build, the unit suite, and gallery-check
just build [target]                                  # build (default: all)
just test                                            # run the unit suite
just test <name>                                     # run another suite (e.g. just test ffi; ffi and string-gen-handles aren't in just check)
just lint                                            # STUB: run linters (add hlint to flake.nix first)
just format                                          # run formatters (cabal, Haskell, Nix)
just check-format                                    # check formatting without modifying files
just docs                                            # build API docs via haddock
just check-coverage                                  # STUB: check coverage (add hpc-codecov to flake.nix first)
just gallery [scenario...]                           # render the failure-report gallery
just gallery-check                                   # confirm every gallery scenario still renders its pinned shape
just repl                                            # open a cabal repl on the library
just clean                                           # remove build artifacts
just loc                                             # count lines of code
just profile-run <scenario>                          # smoke-run a profiling scenario on the dev build
just profile-space <scenario>                        # capture .prof/heap/eventlog into profiles/O<n>/ (prof_opt=0 for -O0)
just profile-time                                    # hyperfine wall-clock of all scenarios on the default -O1 build
just profile-time-compare                            # per-scenario -O1 vs -O0 A/B into profiles/compare/
cabal test zizek:unit --test-options='--pattern "name"'  # run a single test (tasty --pattern glob)
```

The minimum supported GHC version is 9.10, enforced by the `base` bound in `zizek.cabal`.

## Package Structure

- `library/Hegel.hs` is the umbrella API. It defines `prop`, which is `check_ def . forEach`, and re-exports the `Property` vocabulary, `check`/`replay`/`sample`/`samples`, the `Gen` type, settings and their option types, `Hegel.Pool`, `Hegel.Replay`, `Hegel.Exception`, reports, and assertions.
- `library/Hegel/Property.hs` is the property monad's public API: `PropertyT`/`Property`, `hoist`, `check`/`check_`, `forEach`/`forEachWith`, `forAll`/`forAllWith`/`forAllWithLabel`/`forAllSilent`, `annotate`/`annotateShow`/`footnote`, `event`/`eventValue`, `assume`/`discard`, `registerFinalizer`/`resource`/`resource_`, `assert`/`failure`, and `(===)`/`(/==)`. Its internals live in `library/Hegel/Property/Internal.hs`.
  - `event` and `eventValue` record labels and observations for the end-of-run statistics block that `Settings.showStatistics` prints into `Report.engineOutput`.
  - `registerFinalizer act` queues per-case cleanup, drained last-in first-out at the case boundary on every exit and replay. A throwing finalizer aborts the run as `Errored`.
  - `forAllWithLabel "qty" g` labels a draw so a stateful report reads `restock item="apple" qty=5` instead of showing a bare positional value.
- `library/Hegel/Property/Fork.hs` runs an escaping fork of a property body concurrently on its own cloned choice stream: `spawn`/`join`/`cancel`/`poll`, plus `scoped` for a fork whose lifetime is bounded to one block. A fork that is neither joined nor cancelled aborts the run as a malformed test.
- `library/Hegel/Property/Branch.hs` runs fixed-arity and list-shaped concurrent branches of a property, each on its own cloned choice stream and joined before the combinator returns: `concurrently`, `mapConcurrently`, `forConcurrently`, and `replicateConcurrently`, their `_` variants, and `replicateConcurrentlyBounded`. A branch failure surfaces as an ordinary shrinkable counterexample, and the report renders every failing branch's message, location, and diff.
- `library/Hegel/Stateful.hs` provides stateful, model-based testing: `Machine`, `Rule`, `Invariant`, `run`, and `respond`/`respondShow`, layered on `PropertyT` (see Stateful Testing below). A `Machine` needs a `stepCount` of at least 1, usually `defaultStepCount` (50). `rule` builds a rule of weight 1 and `weighted` changes it. `run` drives `Hegel.Internal.StatefulRound` with a single worker, bridging a `Rule`'s by-value state onto that module's `IO`-shaped dispatch through a mutable cell.
- `library/Hegel/Stateful/Concurrent.hs` provides concurrent stateful testing: its own `Rule`, `Machine`, and `run`, plus `Concurrency` built with `fixed`/`upTo`/`between`. It re-exports `Hegel.Stateful`'s `Invariant`.
  - Its `Rule` is a separate type because `apply :: s -> PropertyT m ()` shares the model by reference across workers instead of threading an updated value back.
  - `rule` places a rule in the anonymous group, which every other ungrouped rule shares and which never overlaps a named group. `grouped` places it in a named group. `weighted` sets its weight.
  - A concurrent machine's `stepCount` bounds rounds, not individual rule steps.
  - A failure reports the same step history `Hegel.Stateful.run` gives, with each step tagged by the round and worker that ran it.
- `library/Hegel/Pool.hs` provides engine-managed pools of values for stateful rules to draw from. Drawing from an empty pool is equivalent to `assume False`: inside a stateful rule's body it skips just that step, and anywhere else it discards the whole case. `named` labels a pool's values for the report, and `transfer` moves a value between pools while keeping its identity, so the report shows one lifeline across pools.
- `library/Hegel/Report.hs` defines what a property run produces and the plain and ANSI renderers.
  - `Report` holds a `Result` (`Ok`, `Failures` of one or more `FailureOutcome`s, `GaveUp`, or `Aborted`), `Stats`, a `Reproduction`, and `engineOutput`.
  - Each `FailureOutcome` carries its origin, an optional replay token and caveat, and evidence that is `Captured`, `Uncaptured`, or `Diverged`.
  - `Reproduction` says where a failure can be found again: `Stored key`, `Unstored`, or `Unreproducible`. Its footer renders on every failure render path.
  - `engineOutput` holds the lines `libhegel` printed during the run, collected through the run's output callback. Every renderer appends them after the report.
  - For stateful failures, the rich path composes an event log, a source splice, and a footer.
- `library/Hegel/Report/*.hs` are the rich renderers:
  - `Ann` defines annotations and styles, `Discovery` looks up declarations, `Source` splices and lays out source, and `Span` handles source spans.
  - `Note` defines journal entries and `renderValue`. `Journal` regroups entries by depth and renders them.
  - `Stateful` renders the failing step's source splice. `Concurrent` splices branch and fork groups into their source declarations.
  - `Style` holds glyphs, phrases, and layout budgets in one record. `Encoding` picks the output preference, reading `HEGEL_GLYPHS` and the handle's encoding, and does 7-bit cleaning.
  - `Trace` is the IR that zips journal notes and pool events on their shared `Tick`. `Layout` renders it as a flat chronological event log: one row per step, a bare `✗` or blank gutter, irrelevant runs collapsed into a single elision row, and a concurrent step's round and worker in its own dim right-aligned column. `Layout` also owns `displayName`.
- `examples/gallery/` is the `gallery` executable. Its six scenarios under `examples/gallery/Gallery/` each show one report shape: plain with multiple failures and event statistics, sequential stateful, pool lineage with elision, branches, fork, and concurrent stateful. `just gallery` renders them, and `just gallery-check`, part of `just check`, confirms each still has its pinned shape.
- `library/Hegel/Diff.hs` provides the structural and line-level diffs behind `(===)` failures.
- `library/Hegel/Assertion.hs` provides `assert` and `failure`, which are `MonadIO`-polymorphic and call-stack-aware, and formats failure origins.
- `library/Hegel/Hspec.hs` and `library/Hegel/Tasty.hs` are the framework integrations (see Framework Integrations below).
- `library/Hegel/Settings.hs` holds run configuration as a right-biased `Monoid` of `Maybe` overrides. Its option types are top-level modules: `Hegel.Backend`, `Hegel.Database`, `Hegel.HealthCheck`, `Hegel.Nondeterminism`, `Hegel.Phase`, `Hegel.Seed`, and `Hegel.Verbosity`.
  - `libhegel` resolves each run's settings profile from `development`, `ci`, `workload`, `hegel.toml`, and its own `HEGEL_*` variables. `Hegel.Runner` applies only the fields a `Settings` sets on top, so code wins over the environment. `profile` selects a named profile.
  - zizek itself reads only `HEGEL_REPLAY`/`HEGEL_REPLAY_KEY`, `HEGEL_GLYPHS`, `NO_COLOR`, and Tasty's `--hegel-*` options with their `TASTY_HEGEL_*` forms.
- `library/Hegel/Exception.hs` defines the structured `Diagnostic` and the exceptions built on it, such as `ValidationError`, `SettingsError`, and `HegelError`.
- `library/Hegel/Profile.hs` is process-wide profile control, designed for qualified import. `register` saves a `Settings` as a named profile, `setDefault`/`clearDefault` choose the profile `default` resolves to, and `resolve` fills every engine field of a `Settings` in from the engine.
- `library/Hegel/Replay.hs` encodes and decodes the replay tokens a failure report prints, and `library/Hegel/Internal/Replay.hs` defines the token type itself.
- `library/Hegel/Alphabet.hs` provides curated character alphabets, such as hex digits, for `HasAlphabet` builders.
- `library/Hegel/Internal/Settings.hs` manages settings handles. `withResolvedSettings` resolves a profile, applies a `Settings`' overrides, and reads back what the runner acts on as `Resolved`. An unknown profile or a malformed `hegel.toml` or environment variable becomes a `SettingsError`.
- `library/Hegel/Internal/RunnerConfig.hs` resolves the replay request and the Tasty option overrides that sit on top of a property's `Settings`.
- `library/Hegel/Runner.hs` drives the engine: `check`, `checkWithProgress`, `replay` of a `ReplayToken`, `sample`, and `samples`.
  - `runTestCase` reads `hegel_test_case_should_capture` once at the start of each case. A case the engine stamped runs under a live `Recording` journal, from `Hegel.Property.Internal.newRecordingJournal`, and pool-event stream instead of `Silent`.
  - A failing case files a `Capture` holding its exception, notes, and events under the failure's origin. A stamped capture always replaces an unstamped one, and otherwise the newer capture wins, so the engine's final replay supplies the report.
  - Each engine `Failure` is paired with the capture for its own origin, so a report's message, location, diff, and journal come from a case that failed with that origin.
  - A run whose primary failure has no reproduce blob reports `Unreproducible`.
- `library/Hegel/Gen.hs` is the generator umbrella, designed for `import Hegel.Gen qualified as Gen`.
- `library/Hegel/Gen/Internal.hs` defines the `Gen` GADT and the combinators (`oneOf`, `element`, `frequency`, `filtered`, `mapMaybe`, `defer`, `assume`, `draw`, …).
- `library/Hegel/Gen/Builder.hs` defines the `Build`, `HasMin`, `HasMax`, `HasSize`, and `HasYear` typeclasses. `HasAlphabet` lives in `Hegel.Gen.Char`.
- `library/Hegel/Gen/*.hs` are the per-category builders. `Hegel.Gen.Recursive` builds recursively defined data over an engine-owned depth cap, leaf budget, and retry protocol.
- `library/Hegel/Gen/Internal/String.hs` is the shared construction pattern for string-generator leaves, which build one engine string-generator handle per `Gen` value.
- `library/Hegel/Collection.hs` is the `libhegel`-managed variable-length collection handle that the list, set, and map generators use.
- `library/Hegel/Internal/Tick.hs` is the recording substrate: a monotonic per-case stamp, `Tick`, plus the `Silent`/`Active` toggle and the gated `record`/`drain`/`drainAndReset`, shared by the note journal and the pool-event stream. It knows nothing of pools, notes, or state machines. It records only in a case the engine stamped for capture, whose failure may become the report. `drainAndReset` also clears the buffer, for callers that harvest a live buffer more than once per case: `newChildJournal`, behind every branch, fork, and concurrent worker, and `Hegel.Stateful.Concurrent.run`'s round fold.
- `library/Hegel/Internal/Event.hs` is the per-case pool-event stream (`Event`/`Operation`/`Var`), stamped via `Tick`.
- `library/Hegel/Internal/Foreign/*.hs(c)` is the `libhegel` interop. `Raw` holds the `foreign import ccall` bindings for the `hegel_*` functions zizek uses, the opaque handle types, the `HEGEL_*` pattern synonyms, and bracket helpers. Date and time calls go through `cbits/datetime_shim.c`. `CString` handles C-string marshalling.
- `library/Hegel/Internal/TestCase.hs` and `library/Hegel/Internal/DataSource.hs` are the per-test-case engine interaction.
  - `TestCase` is the handle, a context plus a `hegel_test_case_t*`, carrying the recording toggle. It also provides `markComplete`/`Status` and `withClone`/`withClonePair`/`withClones`, which acquire one or more clones in a fixed order.
  - `DataSource` is the generator-facing channel. It provides the draws (`drawBool`, `drawInteger`, …, `drawString`), the string-generator builders (`buildTextGen`, …), spans (`startSpan`/`stopSpan`, `Label`), collections, pools, events, recursion, and state machines. `newStateMachine` fixes concurrency at 1 without consuming entropy, and `newConcurrentStateMachine` takes explicit rule groups and concurrency bounds. Both take each rule's weight.
- `library/Hegel/Internal/Control.hs` defines the control signals (`AssumeRejected`, `TestStopped`, `LeafBudgetExceeded`, `AttemptMispriced`) and the exception-discipline helpers `catchControl`, `onFailure`, and `isFailure`. `tryProperty` lives in `Hegel.Property.Internal`.
- `library/Hegel/Internal/StatefulRound.hs` is the engine's round-based state-machine protocol, generalized over any number of workers.
  - `Worker` bundles a test case with an `IO`-shaped rule dispatch. `runRound` runs every worker's `runWorkerRound` concurrently over already-acquired clones and resolves the round by `resolveRound`'s precedence: a control error outranks an overrun or an invalid conclusion, which outrank a panic, and the lowest worker index wins among panics.
  - `Hegel.Stateful.run` is its one-worker caller and `Hegel.Stateful.Concurrent.run` its multi-worker one. Both also use its `selectInvariants` for join-point invariant selection and `checkRuleWeights` for weight validation.
- `library/Hegel/Internal/DatabaseKey.hs` derives a test's database key (`propKey`) and source location (`testLocationOf`) from its call site and describe path.

## Module Style

Prefer a module structure that allows functions to be imported fully qualified, with standalone types that are meant to be imported on their own.

For example:

```haskell
import Hegel.Collection (Collection)
import Hegel.Collection qualified as Collection
```

...which brings `Collection.with`, `Collection.more`, and `Collection.reject` into scope.

### Generator builder pattern

Generators are built via a fluent builder API. `Gen.integral`, `Gen.double`, etc. are *builders* that accumulate constraints via `&`-chained modifiers and materialise with `& Gen.build`. The integral builder has type-pinned aliases, `Gen.int`, `Gen.int8` to `Gen.int64`, and `Gen.word` to `Gen.word64`, so element types are usually fixed by alias rather than by type application (`Gen.int`, not `Gen.integral @Int`):

```haskell
import Data.Function ((&))
import Hegel.Gen qualified as Gen

g1 = Gen.int    & Gen.min 0 & Gen.max 100            & Gen.build
g2 = Gen.double & Gen.min 0 & Gen.max 1              & Gen.build
g3 = Gen.double & Gen.disallowNan                    & Gen.build
g4 = Gen.binary & Gen.minSize 4 & Gen.maxSize 64     & Gen.build
g5 = Gen.bool                                        & Gen.build
```

The `Build`, `HasMin`, `HasMax`, `HasSize`, and `HasYear` typeclasses in `Hegel.Gen.Builder`, plus `HasAlphabet` in `Hegel.Gen.Char` for the text and regex builders, provide the shared modifier vocabulary. Builder-specific modifiers are plain functions on their builder type:

- float: `exclusiveMin`, `exclusiveMax`, `disallowNan`, `disallowInfinity`
- char: `codec`, `minCodepoint`, `maxCodepoint`, `categories`, `excludeCategories`, `includeCharacters`, `excludeCharacters`
- regex: `fullMatch`
- uuid: `version`
- domain: `maxLength`
- datetime: `onDay`
- bool: `weighted`
- list: `unique`
- recursive: `maxDepth`, `maxLeaves`

The duration builder takes its bounds through `min` and `max`, and `milliseconds`, `seconds`, `minutes`, and `hours` build those bound values, as in `Gen.duration & Gen.max (Gen.minutes 5)`.

Applying an inapplicable modifier (e.g. `Gen.uuid & Gen.min 0`) is a type error. There are no `*Options` records on the public API.

Builder families beyond the sample above: `text`, `char`, `regex`, `uuid`, `uri`/`uriText`, `domain`, `email`, `date`, `time`, `datetime`, `duration`, `recursive`, and the collections (`list`, `nonEmpty`, `set`/`hashSet`/`intSet`, `map`/`hashMap`/`intMap`). Choice and conditional combinators (`oneOf`, `element`, `frequency`, `filtered`, `mapMaybe`, `just`, `defer`, `enum`/`enumBounded`, `maybe`, `either`) are not builders. They produce `Gen` values directly, with no `& Gen.build`.

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

`zizek` drives the Hypothesis engine in-process via FFI to `libhegel`. The engine owns sampling, choice-sequence bookkeeping, and integrated shrinking. `zizek` calls a dedicated FFI entry point per generator kind and interprets the result directly, with no intermediate schema or encoding step.

There are two property-writing surfaces. `Hegel.prop gen body` is the shortest: it runs `check_ def (forEach gen body)` and throws on failure, for use inside a test framework's example. `check settings property` returns a `Report`, where a `Property` interleaves `forAll` draws, effects, and assertions (see `Hegel.Property`). Stateful testing is not a third surface: `Stateful.run machine` is an ordinary `PropertyT` action run via `check`.

### Protocol

Each generator kind has its own FFI entry point, with parameters marshalled directly rather than through an intermediate wire encoding. For each test case:
1. A draw asks the engine for a value. A scalar draw (`drawBool`, `drawInteger`, `drawFloat`, `drawBytes`, `drawUuid`, `drawDate`, `drawTime`, `drawDatetime`) returns it directly. A string draw first builds a native string-generator handle (`buildTextGen`, `buildRegexGen`, `buildEmailGen`, `buildUrlGen`, `buildDomainGen`) that the engine samples and shrinks against internally.
2. `startSpan`/`stopSpan` bracket groups of related draws so the engine can shrink them as a unit.
3. `markComplete` reports the outcome (`Valid`, `Invalid`, `Overrun`, or `Interesting`) at the end of each test case.

All of these are FFI calls into `libhegel` via `Hegel.Internal.Foreign.Raw`, wrapped by `Hegel.Internal.DataSource` and `Hegel.Internal.TestCase`.

### `Gen` GADT

`Gen a` is a GADT defined in `Hegel.Gen.Internal`, with constructors `Pure`, `Draw`, `Map`, `Ap`, `Bind`, and `OneOf`. `Draw` is an opaque `TestCase -> IO a` leaf, and every leaf generator and combinators like `filtered`/`frequency` bottom out there. Every constructor but `Pure` carries its span label (`labelOf`), computed strictly at construction from its components. The package enables `StrictData`, so never tie a knot through a label field. `draw :: TestCase -> Gen a -> IO a` produces a value from a live test case.

The GADT structure is interpreted, not just executed. `draw` opens one span per generator level, labelled `labelOf g`, and draws every component through `draw` so each gets its own span, mirroring upstream `TestCase::draw_silent`. `Pure` opens nothing, and an `Ap` spine with fewer than two non-`Pure` leaves forwards without a tuple span. `drawInline` runs a body inside the caller's span, for forwarding generators such as `defer` and the validating `Text`/`Domain` builders.

`filtered`/`mapMaybe` make up to three attempts, each in its own discardable span, and discard the case once all three miss. An exhausted filter surfaces as the engine's `FilterTooMuch` health check or an unsatisfiable run.

### Span System

Spans (`start_span`/`stop_span`) group related generation calls so the engine can shrink them as a unit. The `Label` type in `Hegel.Internal.DataSource` names each generator kind (`zizek.integer`, `zizek.list`, …). `spanLabel` gives its wire value, and `combineLabels`, equal to `hegel_label_combine`, folds in component labels so `list int` and `list text` differ. A filter opens a discardable `zizek.filter.attempt` span per attempt inside its own `zizek.filter`-derived span. Each `TestCase` counts its open spans: `Hegel.Internal.StatefulRound` closes the spans a rejected rule unwound past (`discardSpansTo`) before the round span, and the recursion retry loop forgets the ones the engine closed itself (`forgetSpansTo`).

### Collections

`libhegel`-managed collections (`Collection.with`/`Collection.more`/`Collection.reject` in `Hegel.Collection`) drive variable-length generation, and the list, set, and map generators are built on them. Rejecting duplicates requires variable-size mode. See Note [Variable-size mode required for reject] in `Hegel.Collection`.

### Recursive Generation

`Gen.recursive` (`Hegel.Gen.Recursive`) generates recursively defined data, such as trees or JSON documents, from a leaf generator and a branch function over sub-values. The engine owns branch probability, the depth cap (`maxDepth`), the leaf budget (`maxLeaves`), and the per-value target size, driven through `hegel_new_recursion`/`hegel_recursion_branch`/`hegel_recursion_leaf`/`hegel_recursion_finish`/`hegel_recursion_retry`.

Two distinct situations both signal through `HEGEL_E_RETRY`. Outgrowing the leaf budget, from `hegel_recursion_leaf`, throws `LeafBudgetExceeded`. A completed value the engine discarded as mispriced, from `hegel_recursion_finish`, throws `AttemptMispriced`. Both are control signals in `Hegel.Internal.Control`, caught only by the retry loop that opened the recursion scope.

Every span a recursive value opens carries the generator's own label, so the shrinker can swap a tree for a subtree: one around the whole value, retries included, and one around each child sub-value. They are opened and closed in plain sequence, never under a `bracket`-style guarantee, so either retry's unwind skips the closing `stopSpan` instead of closing a span the engine already discarded.

### Stateful Testing

`Hegel.Stateful.run` drives a `Machine` inside an ordinary property, on top of `libhegel`'s round-based state-machine protocol. A machine has an initial state, `Rule`s, `Invariant`s, and a `stepCount` of at least 1. The engine owns rule selection, including swarm testing's per-test-case rule subsets, enforcement of the machine's `stepCount`, and shrinking. Among the rules swarm testing enabled, the engine picks in proportion to each `Rule.weight`, which `run` rejects as a malformed test unless it is finite and positive.

A round polls the engine for the next concurrency group, then pulls rules for that group until the engine signals that the round's budget is exhausted. A sequential machine has one group and runs one rule per round. A round's draws share one `LabelStatefulRule` span, discarded when one of its rules was rejected. A rejected rule is reported back to the engine, which at concurrency 1 keeps it from counting toward the step count.

Every invariant runs on the initial and final states. At each round's join point the engine decides which invariants run: every invariant marked `always`, and a sample of the rest, about one check per case. The `should_check_invariant` draw happens for every invariant at every join point, before any of them runs.

A failing assertion is journaled in-band at the step that produced it. Replay alignment is load-bearing. Every draw, and every poll for the next round or the next rule, is part of the choice sequence and happens unconditionally, on replay too. Skipping one misaligns every later draw, and the counterexample stops reproducing.

`Hegel.Pool` provides engine-managed value pools for rules to draw from, safe under concurrent access from several workers sharing one pool. Every read or mutation of a pool's mirror holds an `MVar` across the paired engine call, so a concurrent lookup never observes a variable id the engine has assigned but the mirror hasn't recorded yet.

`Hegel.Internal.StatefulRound` factors the round and rule-pull loop out into machinery generalized over any number of workers, each running concurrently over its own persistent clone. `Hegel.Stateful.run` is its one-worker caller, and `Hegel.Stateful.Concurrent.run` is its multi-worker one.

#### Concurrent machines

`Hegel.Stateful.Concurrent.run` drives the same round protocol with more than one worker. The root test case still drives `next_group` and, once a round concludes, the invariant checks. Each worker dispatches rules against its own clone, acquired once and held for the whole case.

Each worker opens its own `LabelStatefulRule` span around its pull for the round, on its own clone. A round's rules draw on different clones, so one span on the root would enclose none of them, and each clone's span stack is untouched by any other worker's. `Worker.roundSpan` tells `runWorkerRound` which case it's in: `Own` for a concurrent worker, and `Caller` for the sequential driver, which keeps its own root-level span open across the `next_group` draw.

Each worker's `Env` gets a private `Recording` journal from `newChildJournal` whenever the case itself is recording. A worker's clone runs its own `Tick` clock, so a shared sink would race on the buffer and interleave independently clocked streams into a meaningless order. Under an ordinary `Silent` case every worker stays `Silent` too, at no extra cost.

After every round, whatever its verdict, the root driver runs `foldWorkerRound`, so a panicking worker's step is captured before its exception concludes the case. The fold drains each worker's notes and pool events in ascending worker order, restamps them onto the root's clock, and gives each `StepHeader` its global index. Right after each header it emits a `StepOrigin` note naming the round, worker, and concurrency group, which only the fold knows. `Trace.Step.origin` lifts that note into the IR, and is `Nothing` for a sequential step. `Hegel.Report.Layout` renders it in the event log's dim right-aligned column, e.g. `round 2, worker 2 (writers)`, omitting the group for an ungrouped rule.

Because the fold runs whatever the round's verdict, a round can fold in steps after the failing one. A sequential machine's failing step is always the log's last row, but a concurrent log gives up that guarantee.

A rule's group id is interned to a dense `Int64` by first appearance and passed in `hegel_new_state_machine`'s `rule_groups` array. Two rules in the same group may share a round, and rules in different groups never do. The gallery's `bank` scenario shows a `"tellers"` group and an `"auditors"` group that never share a round.

When at least one invariant runs at a round's join point, the root first emits a `RoundBoundary` note carrying a fresh step index and the round number. `Trace.segment` treats it as a header exactly like `StepHeader`, so the check renders as its own segment, e.g. `Step 11: round 2 invariant check`, and an invariant failure attaches to that boundary. The final-state check gets the same treatment through a `FinalBoundary` note, in both the sequential and concurrent drivers.

Concurrency alone does not make a run nondeterministic. The engine switches to nondeterministic handling only when it observes a verdict flip or a replay miss, as for any other test. A concurrent failure reports the failing assertion's message, location, and diff from `Hegel.Runner`'s per-origin capture, plus the same step-by-step trace `Hegel.Stateful.run` gives once any rule has dispatched. Its reproduction footer is `Unreproducible` only when the engine attached no reproduce blob.

### Framework Integrations

`Hegel.Hspec.prop` and its variants derive a stable example-database key from the module plus the test's describe and name path, and persist failures wherever the resolved profile's database points: `.hegel/` under `development`, nowhere under `ci`. Renaming a test or its group orphans its stored failures, and stored replays only reproduce against deterministic fixtures.

Native `Hegel.Tasty.testProperty` runs without a key, so nothing persists unless `testPropertyWith` supplies an explicit `databaseKey`. Use `tasty-hspec` to get Hspec's automatic keys inside Tasty.

Both integrations fill in `Settings.testLocation` from the call site, which the engine uses to report each run's verdict inside Antithesis. An explicit replay request comes from `HEGEL_REPLAY` with `HEGEL_REPLAY_KEY`, or from Tasty's `--hegel-replay` with `--hegel-replay-key`.

### Test Suites

- `tests/unit/` is the `unit` cabal suite, tasty wrapping hspec specs. It covers generators, property checks, report, source, and event-log rendering, control signals, stateful and concurrent stateful testing, pool events, the trace IR, branches and forks, finalizers and resources, sampling, database replay, runner configuration, and the framework integrations. Its `Main` sets `HEGEL_DATABASE=disabled`, so tests that don't override the database store nothing.
- `tests/ffi/` is the `ffi` cabal suite: wire-level checks, plus a closed-world guard, `cbits/wire_enum_guard.c`, compiled with `-Werror=switch` and `-Werror=switch-enum`, that fails the build if `libhegel` adds an enum variant.
- `tests/string-gen-handles/` is the `string-gen-handles` cabal suite. It asserts that unreferenced string-generator handles get GC-reclaimed. Run it with `cabal test zizek:string-gen-handles --flag census`; without the flag every case is pending. It runs in its own process because the assertion is flaky inside the shared `unit` binary, as its module header explains.
- `tests/profile/` is the `profile-hegel` executable: deterministic named workloads for profiling the Haskell-side hot paths, driven by the `just profile-*` recipes. It is not a test suite, and a completed run always exits 0. The scenario table is `scenarios` in `tests/profile/Main.hs`, which `profile-hegel --list` prints.

## Miscellaneous Conventions

- Use jujutsu (`jj`) for version control.
- **Prototype loose, land tight**: while a workflow's design is still moving, driving `cabal` (or other tools) by hand is fine. Once it solidifies, fold the surviving invocations into `scripts/` + `justfile` recipes — the justfile is the discoverable surface, and one-off invocations in a transcript force the next session (human or agent) to rediscover them.
- **Exception discipline**: Hegel's control signals (`AssumeRejected`, `TestStopped`) are async exceptions precisely so user catch-alls pass them through. Never hand-roll a `catch @SomeException` (or a base `try @SomeException`) around code that draws or asserts — it would swallow the discard/stop signals and corrupt the run. Use `Hegel.Internal.Control`'s `catchControl`/`onFailure`, or `Hegel.Property.Internal.tryProperty`, instead.
- `references/hegel-rust/` vendors the Rust/C engine reference (`hegel-c/include/hegel.h`, `src/stateful.rs`, …). It is the ground truth for engine semantics when Haskell-side documentation and behavior disagree.
