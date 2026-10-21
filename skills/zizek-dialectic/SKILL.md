---
name: zizek-dialectic
description: "Build model-based property-testing suites with the zizek Haskell library to find correctness bugs, then reduce failures to deterministic regression tests and minimal fixes. Use for broad property-testing coverage of Haskell data structures, algorithms, and stateful systems, including random operation sequences run against both the implementation and an independent reference model."
---

# Agents and property tests with zizek

Adapted from Inanna Malick's [proptest-praxis](https://github.com/inanna-malick/agent-skills/tree/main/skills/proptest-praxis), which accompanies [Property Testing with Agent Swarms](https://recursion.wtf/posts/agents-and-property-tests/). Her method carries over intact, and this version maps it onto zizek's API and its Hypothesis-style engine.

Use these instructions in the target repository. Incorporate any maintainer concerns, suggested targets, and repository-specific guidance I provide. Own the investigation and keep exploring as findings suggest new targets.

## 1. Objective and deliverables

Find correctness bugs across the repository by building and running property-testing machinery. Use `Hegel.prop` for a single generator inside an Hspec or Tasty example, `Hegel.check` with a `Property` when a test interleaves draws, effects, and assertions, and `Hegel.Stateful.run` when the behavior under test is a history of operations. Prefer executable checks that explore many inputs and histories without spending LLM tokens on each case.

Maintain a discovery branch containing reference implementations, generators, `Hegel.Stateful` machines, invariants, and investigative tests. It can be large, repetitive, and unoptimized, but it must be correct. Give it its own cabal test suite, linked with `-threaded` since zizek runs properties on bound threads, and retain reproducible failures.

For each confirmed bug, produce a focused change containing a deterministic regression test and a minimal fix, reviewable without the discovery suite, and keep it local as a commit or the repository's equivalent. Bugs found by inspection qualify too. Keep ambiguous findings separate from established contract violations.

## 2. Direct the investigation

Recognize testing opportunities from code behavior, not just names: derived state, cache invalidation, incremental updates, secondary indexes, graph reachability, state transitions, aliasing, and ordering dependencies across module boundaries.

Use these lenses throughout:

- **History-dependent behavior:** identical logical contents can conceal different caches, allocation histories, sharing, and deferred work, so vary how states are reached and when they are observed. Laziness is deferred work too, and a pure `error` surfaces where a value is forced rather than where it was built.
- **Generator support and sampling distribution:** omitted interactions are unreachable, and possible ones may still be vanishingly rare. Inspect generated behavior before relying on run volume: record what each case exercised with `event` and `eventValue` and read the distribution `showStatistics` prints. When an interaction is reachable but rare, score how close each case came with `target` so the engine climbs toward it.
- **Oracle independence and common-mode failures:** express the intended behavior independently. Shared algorithms, production helpers, copied assumptions, and shared `Eq`, `Ord`, or `Hashable` instances let the implementation and model agree on the same wrong answer.
- **Search feedback:** treat no findings as a coverage problem first. Look for missing operations, transitions, histories, and interactions, and test the harness's sensitivity to plausible defects. For failures or suspicious behavior, question both the implementation and the test's assumptions, then explore the same mechanism in related operations and structurally similar code.

Prioritize complex implementations with simple correctness models: an optimized structure checked against a list or `Data.Map`, incremental metadata against full recomputation, a planner against a small interpreter or exhaustive search over small instances. Choose and combine techniques by judgment, consider interactions between subsystems and equivalent behavior exposed through different APIs, propose approaches the list below misses, and prioritize by maintainer concerns, complexity, suspicious assumptions, and how strong a check is feasible.

If your harness supports sub-agents, split planning, coordination, and implementation into roles, and give each worker a bounded investigation in its own isolated working copy with the relevant contract, candidate techniques, and expected deliverables. Whether delegating or working alone, track active targets, findings, and follow-up leads, and consolidate useful machinery into the discovery branch.

**Adaptive search allocation:** get promising harnesses running early and keep them running while developing others. Choose among new targets, stronger oracles, broader generators, and longer runs by expected information gain against cost, keeping breadth across independent mechanisms while deepening productive ones. Passing harnesses remain reusable search machinery. Record the commands, revisions tested, coverage observed, and blind spots so work can be resumed.

Own the investigation. Bring me findings, consequential ambiguities, and questions that need repository or organizational context.

## 3. Recognize testing opportunities

Use these as starting points and infer more from each implementation's responsibilities. A target whose correctness depends on its history of calls becomes a `Hegel.Stateful` machine, built as section 4 describes.

**Partial oracles and solution checking:** exploit outputs that are cheaper to validate than to compute, such as path edges, dependency ordering, preserved fields, and index consistency. Distinguish soundness from completeness: every emitted node being reachable does not mean every reachable node was emitted. Combine these with reference models, metamorphic relations, and small-instance exhaustive oracles.

- **Custom collections, indexes, arenas, interning, deduplication — model-based differential testing:** compare against a list, `Data.Map`, or `Data.Set` model. Exercise overwrite, deletion, reinsertion, collisions, index reuse, duplicates, and empty and singleton states, and check contents, lookups, multiplicity, and agreement between primary and secondary indexes.
- **Caches, incremental summaries, derived metadata — recomputation oracles:** compare maintained results with full recomputation. Warm caches, mutate through every relevant API, and query again, including transitive dependencies and changes that keep size but alter contents.
- **Graphs, traversals, dependency analysis:** compare reachability and membership with a plain traversal or small-instance enumeration. Generate cycles, diamonds, shared descendants, disconnected components, multiple roots, redundant edges, and deletion holes by drawing nodes first and then edges between existing node indices with `>>=`. Check paths, ordering, and payload preservation under rewrites.
- **Query planners, optimizers, AST rewrites — semantic preservation:** evaluate original and transformed forms with a small interpreter, and enumerate candidates on small instances to check promised feasibility or optimality. Equivalent plans need not have identical structure.
- **State machines, transactions, staged builders — fault injection, failure atomicity, recovery invariants:** generate failure schedules as ordinary data, such as the call number on which an `IORef`-counted test double throws. Exercise rollback, retry, partial progress, and continued use after errors, and check recovery against the actual contract. Catch only the injected exception's type, never `SomeException`, as `references/exceptions.md` explains.
- **Concurrent structures, shared mutable state, locks, STM — concurrent stateful testing and small linearizability checks:** run long histories as a concurrent machine with conservation invariants, and put operations that must never overlap in different rule groups. For short bursts, run two to four operations with `Branch.mapConcurrently` and check that some sequential ordering explains the results. See `references/stateful.md`.
- **Cancellation and asynchronous exceptions — failure atomicity under interruption:** `Fork.spawn` an operation, `Fork.cancel` it, then check the structure is consistent and use it again. `references/exceptions.md` lists what to look for.
- **Parsers, printers, serializers, encoders — grammar-aware generation and round trips:** round-trip structured values, and separately generate malformed, truncated, boundary-sized, and unusual input to find crashes and incorrect acceptance. `Gen.regex` without `Gen.fullMatch` makes near-valid input easy. Don't normalize away meaningful differences.
- **Representation boundaries — boundary-value analysis:** exercise empty and singleton inputs, extrema, narrowing conversions such as `fromIntegral`, signedness, overflow, sentinels, and inclusive and exclusive ranges. Model size and offset arithmetic with `Integer`, and compare range behavior against explicit sequences. For text, distinguish bytes, code points, and grapheme clusters, and generate combining marks, zero-width characters, and bidirectional controls with `Hegel.Alphabet`.
- **Equality, hashing, ordering, normalization:** build equivalent values through different histories and representations, deriving the second from the first since random pairs seldom compare equal. Check that equal values hash equally, ordering matches its promise, and canonicalization is meaning-preserving and idempotent.
- **Bulk APIs, alternate implementations, execution modes — metamorphic testing:** compare batch with individual operations, incremental construction with rebuilding, and alternate paths that promise equal results. Vary order with `Gen.permutation`, subsets with `Gen.subsequence`, and chunking and configuration where they shouldn't matter.
- **Clone, snapshot, sharing, copy-on-write:** branch from shared state, mutate each branch, and check isolation or propagation as promised, including nested objects and mutations after reads initialized lazy state.
- **Validators and invariant auditors:** inject a targeted defect into a valid object and check the auditor catches it, check it accepts valid objects, and vary where the corruption sits.

The engine samples rather than exhausts a space and never reports full coverage, so add an ordinary unit test that enumerates any domain small enough. Keep checks cheap, and separate expensive small-instance oracles from larger stress cases.

## 4. Build effective generators and operation-sequence tests

**Model-based stateful differential testing:** write each interaction as a `Hegel.Stateful` rule that applies one operation to both the system and an independent model and compares the results, with invariants checking their overall agreement. Cover mutations, reads, iteration, bulk operations, reset, and snapshots, and generate configuration and initial state as well as histories. The engine re-runs `initial` for every case and shrink attempt, so acquire resources there with `resource` and keep setup repeatable. Start sequential, and move to `Hegel.Stateful.Concurrent` only once that passes and the system promises concurrent safety. `references/stateful.md` covers both.

**Constructive generation and near-valid inputs:** generate valid structures to reach deep behavior, then violate one constraint at a time, such as a dangling reference, a wrong length, a duplicate identifier, or a truncated encoding. Use `Gen.recursive` with `Gen.maxDepth` and `Gen.maxLeaves` for trees. Keep unconstrained external-input tests too. `references/generators.md` covers builder defaults, recursion, and targeting.

**Input-space partitioning and combinatorial interaction coverage:** define state, operand, operation, and configuration classes, such as overwrite, warmed cache, shared snapshot, and capacity boundary, and cross them deliberately. Draw each class as a small type with `Gen.enumBounded` and build the case to match. For a small product, loop over every combination outside the property with the classes fixed, and target pairwise or higher-strength combinations as dimensions grow. Record each combination as one `event` label to see which occur, and test just below, at, and above thresholds.

Generate interacting operands from small domains, such as `Gen.int & Gen.min 0 & Gen.max 7`, to force overwrites, repeated access, and duplicates. Mix existing, absent, removed, and fresh identifiers, drawing existing ones from a `Hegel.Pool` of keys already inserted when the key space is too wide for collisions, make replacement values differ so stale results show, and cover large domains separately for growth.

Keep returned handles in a `Hegel.Pool` and draw them in later rules with `forAll (Pool.reuse pool)` or `forAll (Pool.consume pool)`, putting a logical identity in the pool when the system and model use different handles. Select operands from the model, not from whatever the system last returned. Keep stale handles for targeted tests where the API allows them, and don't confuse identity with allocation order. Shrinking deletes earlier steps, so a counterexample made mostly of skipped steps means rules depend on removable state. Have such a rule check `Pool.isEmpty` and create what it needs.

Combine random histories with targeted patterns embedded among other operations: warm, mutate, query; remove, reinsert; allocate, free, reuse; populate, clear, repopulate; snapshot, mutate either side, inspect both; fail, then continue. A rule that runs a whole pattern makes it common. The same contents reached by different histories may expose different bugs.

**Swarm testing and operation-mix diversification:** the engine already enables a different subset of rules per case, and `Stateful.weighted` shapes the mix. Where frequent clears prevent growth or frequent reads keep lazy state initialized, add machines with growth-, mutation-, read-, or reuse-heavy weights alongside a broad mix and occasional longer `stepCount` runs.

Preserve coverage of missing keys, duplicates, malformed inputs, and rejection paths. Don't limit tests to current caller habits. Distinguish a real API restriction, such as a documented precondition, from an incidental calling pattern.

Prefer constructing related inputs to rejecting most cases: draw a dependent value from what it depends on with `>>=` rather than filtering with `Gen.filtered` or `assume`, since a generator that rejects most draws trips `FilterTooMuch`. Every draw must depend only on earlier draws and deterministic state so replays match, with no hidden randomness in the test body.

**Fault activation and observability, RIPR (reachability, infection, propagation, revealability):** a defect is caught only when a history reaches the faulty code, corrupts state, propagates it to an observable result, and checks that result. Treat reads as operations, since they can warm caches and force thunks, and keep extra checking from forcing everything. An `always` invariant runs every step, while a plain `invariant` is sampled about once per case and leaves room for delayed reads.

**Behavioral coverage:** measure meaningful events with `event` and `eventValue`, such as overwrites, stale-handle attempts, cache hits after mutation, and size reached. Many `invalid` cases relative to `valid` in a report's `stats` mean generators or assumptions are discarding the run. Look for empty states, no-ops, and unexercised variants, and use `cabal test --enable-coverage` to find missed branches. More test cases won't fix a generator that rarely builds the interaction.

**Mutation testing as harness calibration:** temporarily drop a cache invalidation, skip an index update, or mishandle an overwrite, and check the harness catches it. A surviving mutation points at a missing RIPR link. Revert the mutations afterward.

Label ambiguous draws with `forAllWithLabel` and record intermediate values with `annotateShow` so a shrunk report is readable. A failure is stored for replay only when the test has a database key, as `references/reproduction.md` describes.

## 5. Investigate failures

Reproduce each failure and find the first divergence. A stored failure replays on the next run, and a replay token reruns one case, as `references/reproduction.md` describes. A stateful report marks the failing step and shows which earlier steps created or moved each pooled value. Inspect both implementation and oracle, including adapters, handle mappings, normalization, and comparisons that could hide or invent differences.

Establish the concrete violation: stale data, lost or duplicated values, wrong dependencies, inconsistent equality, an unexpected exception or `error`, or non-termination. A data structure or algorithm violation suffices without an end-to-end reproduction. Raise genuine ambiguity instead of inventing a specification. A failure the report says did not recur on every replay points at uncontrolled state, such as a race or non-repeatable setup, and setting `nondeterminism` to `Forbid` makes that an error while you chase it.

**Counterexample generalization and causal isolation:** form a hypothesis from the shrunk witness, then vary values, topology, sharing, ordering, intervening mutations, and prior reads individually and together to separate triggers from incidental structure. Encode the resulting failure family in generators.

**Variant analysis:** test the same mechanism in adjacent operations, alternate construction paths, related cached state, and structurally similar code, in parallel if your harness supports sub-agents.

**Root-cause clustering:** group findings by mechanism and repair scope, since different traces may share a cause and similar symptoms may not. `reportMultipleFailures = Just True` reports every distinct failure, and `stopOnFirstFailure = Just False` keeps generating after the first. Exceptions of one type thrown inside the code under test report as one failure, so catch each operation's specific exception and report it with `failure` to separate them. Let clustering decide distinct bugs and change boundaries.

Reduce each failure to a readable unit test with literal values in the project's own framework, and confirm it fails on unmodified code for the claimed reason. For a race, force the bad interleaving deterministically, for example with a hook that pauses one operation between its read and write. A timeout or disagreement with an opaque model is not the deliverable.

## 6. Extract reviewable fixes

**Security-sensitive findings:** stop and bring potential security implications to me before doing anything else with them. Keep related reproductions, fixes, and discovery artifacts local, and leave disclosure to me.

Start each bug's change from the main development line with only its regression test and smallest complete fix, keeping models, broad suites, and scaffolding on the discovery branch. Make any dependency between fixes explicit.

Verify the regression test fails before the fix and passes after. In an investigative working copy, run the discovery properties and failure-family generators against the fix on fresh cases as well as stored failures, check related operations and entry points, and run the repository's required checks.

Describe the change by its trigger, incorrect behavior, expected result, and fix, referencing the investigation for background. State validation accurately and separate the demonstrated violation from unproven impact, so a reviewer who has never seen this skill can follow it.

After preparing each change, merge its fix into the discovery branch and keep searching past it. Feed findings and suspicious patterns back into target selection, and report findings and consequential questions as they arise.
