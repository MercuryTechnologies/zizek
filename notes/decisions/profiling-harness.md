# Profiling harness — decision record & guide

## Context

Stateful testing landed, and we want to see where `zizek` spends its time — specifically, to separate **Haskell binding overhead we can reduce** (CBOR encode/decode, FFI marshalling, journaling, generator interpretation) from **engine search cost that is a fact of the universe** (`libhegel` owns sampling, choice-sequence bookkeeping, rule selection, and shrinking behind the FFI boundary).

This is *intra-zizek attribution*; the cross-library comparison against QuickCheck/hedgehog is the separate eval harness in `notes/roadmap/02-eval-harness.md`. Two ideas are shared with that note: the phase-attribution trick (run with and without the `Shrink` phase) and the "expose a shrink-step counter from `Hegel.Runner`" follow-up.

The shape mirrors `../sentry-haskell`'s harness: a dedicated profile executable with named scenarios, a `cabal.project.profiling` (`-fprof-late`), capture scripts, hyperfine for wall-clock, just recipes, and viz tools in the nix devShell.

## Pieces

- `tests/profile/Main.hs` — the `profile-hegel` executable: CLI + scenarios.
- `tests/profile/Warehouse.hs` — copy of the demo warehouse machine with a `Bug = Fixed | Buggy` toggle. Deliberately duplicated from `tests/demo/stateful-rich/Main.hs`: the demo's source *text* is its content (the rich renderer splices those exact declarations), so profiling-motivated edits must not perturb it.
- `cabal.project.profiling` — `optimization: 1`, `profiling: True`, `profiling-detail: late-toplevel` (cost centres inserted after optimization, so the `.prof` reflects optimized code). `-O1` deliberately: zizek is a library that runs inside its consumers' test suites, and cabal builds those at `-O1` by default — that's the code users actually execute. The optimization level is a harness dimension: `just prof_opt=0 profile-space <scenario>` captures the un-optimized dev loop instead, into its own builddir and `profiles/O0/` (so O0 and O1 captures coexist).
- `scripts/profile-space.sh` / `scripts/profile-time.sh` / `scripts/profile-time-compare.sh` — capture, timing, and A/B drivers. `profile-time` runs the plain default build (`-O1`, non-profiled — again, what consumers get; an earlier separate `cabal.project.release` at `-O2` was dropped for measuring a configuration no user runs); both timing scripts derive their scenario list from `profile-hegel --list`, so new scenarios are covered automatically. The compare script takes free-form label/binary pairs, so it also serves for two-commit A/Bs.
- just recipes: `profile-run` (smoke), `profile-space` (capture, per-level into `profiles/O<n>/`), `profile-time` (hyperfine), `profile-time-compare` (`-O1` vs `-O0`, per-scenario ratios into `profiles/compare/`).

The profiling builddir nests inside the default one (`dist-newstyle/prof`), so the two configurations never thrash each other's caches while `cabal clean` and the existing `.gitignore` entry cover both. Captured artifacts land in `profiles/` (gitignored). Gitignore gotcha, hit once already: a pattern like `*.prof*` also swallows `cabal.project.profiling` — keep the artifact patterns narrow (`*.prof`, `*.prof.*`).

## Scenarios

Every scenario is `check` with a **fixed seed** (default `2026`, `--seed` to override) and a fixed `testCases` count, so consecutive runs do identical work — hyperfine iterations are comparable and profiles are reproducible. The binary is pinned to one capability (`-with-rtsopts=-N1`, overriding the conformance stanza's `-N`): the workloads are single-threaded, and parallel GC would only add nondeterministic noise. Reports are summarized in one line and never rendered (rendering would pollute a failing scenario's profile) — except the `render-*` scenarios, where a forced render loop *is* the workload. Database persistence is off (the `defaultSettings` default).

| name | default cases | body | isolates |
|---|---|---|---|
| `baseline` | 10000 | one full-range int draw | per-case floor: `driveLoop` + `hegel_next_test_case` + `markComplete` + outcome classification |
| `draws` | 1000 | 100 × `forAllSilent` small int | per-draw round-trip: schema CBOR encode → `hegel_generate` → decode, allocas + `packCStringLen` copy; `forAllSilent` keeps journaling out |
| `payloads` | 500 | one list-of-text + one map draw | per-byte CBOR cost + `Hegel.Collection` span machinery, vs `draws`' per-call cost |
| `steps` | 2000 | passing one-rule counter machine | per-step overhead: `stateMachineNextRule` FFI + step journaling + invariant dispatch + `withFailureNote`/`nested` |
| `mixed` | 1000 | warehouse machine, bug fixed (passing) | realistic mixed workload: multi-draw rules, `assume` discards, annotations, `(===)` invariants |
| `shrink` | 100 | warehouse machine, buggy (failing) | find + shrink + replay; pair with `--no-shrink` for phase attribution |
| `heap-stress` | 300 | 24-SKU warehouse, per-step audit-log thunk chains + fat annotations | allocator pressure & residency shape under big states; per-case sawtooth |
| `gen-churn` | 2000 | fresh dependent generator per draw | pre-encoding's no-win case: per-draw generator construction + schema encode |
| `gen-hoard` | 20 | 10k generators alive as a CAF, 1k-wide draw window per case | cached-encoding retention; residency plateau |
| `render-plain` | 200 | one fixed find run (100 cases, full shrink), then N forced plain renders | per-failure report-render latency |
| `render-rich` | 100 | same find run, rich renderer | source discovery + splicing + Timeline doc cost |

For the `render-*` scenarios the case count is the number of render iterations, not `testCases`.

Two facts discovered while wiring this up, load-bearing for the design:

- A **zero-draw body runs exactly one valid case**: the engine deduplicates identical choice sequences, so the per-case floor needs a minimal draw that varies. `baseline` draws one full-range int (a bounded range would also exhaust its value space below 10k cases).
- The `shrink` scenario's `valid` tally dwarfs `testCases` (~12k replays for 100 configured cases with the default seed): the shrink search re-executes the whole machine per attempt. That tally is itself a useful magnitude to watch.

Derived metrics:

- `draws` − `baseline` (per case) → cost per draw.
- `payloads` vs `draws` → payload-size cost vs call-count cost.
- `steps` → cost per step, after subtracting its one draw.
- `shrink` vs `shrink --no-shrink` (hyperfine pair) → wall time attributable to the Shrink phase.

## Interpreting the outputs

### Engine vs bindings split in the `.prof`

GHC attributes foreign-call time to the SCC of the Haskell wrapper making the call. With `late-toplevel` detail every top-level binding gets its own SCC, so **inherited time under the FFI-wrapper cost centres is approximately engine time** — not reducible from Haskell:

- `Hegel.Internal.FFI.generate` (the `hegel_generate` round-trip)
- `Hegel.Runner.driveLoop` own time (the blocking `hegel_next_test_case`)
- `Hegel.Internal.TestCase.markComplete`
- `Hegel.Internal.DataSource.stateMachineNextRule`

Refinement: `FFI.generate`'s SCC also contains the `useAsCStringLen`/`alloca`/`packCStringLen` marshalling. Its `%alloc` column separates that out — the engine never allocates on the Haskell heap, so allocation attributed there is marshalling. If a sharper split is ever needed, wrap just the ccall in a manual `{-# SCC #-}`.

**Bindings overhead (reducible)** is everything else: `Hegel.Internal.CBOR` / `wireform-cbor` SCCs (schema encode, value decode), `Hegel.Gen.Internal` interpretation, `Hegel.Stateful.run` own time and journal appends, `Runner.runTestCase` classification. High `%alloc` anywhere is Haskell-side by definition.

If time disappears into `wireform-cbor` at too-coarse granularity (hackage/source-repo deps get the default `exported-functions` detail), add to `cabal.project.profiling`:

```
package wireform-cbor
  profiling-detail: late-toplevel
```

### Other captures

- `profiles/O<n>/<scenario>.rts` (`+RTS -s`) — mutator vs GC split; high GC% means allocation pressure is worth chasing before anything else.
- `profiles/O<n>/<scenario>.eventlog(.html)` — heap shape over time, by cost centre (`-hc`); eventlog2html renders it.
- `profiles/O<n>/<scenario>.prof(.html)` — the cost-centre tree; profiteur renders it, `profiterole`/`ghc-prof-flamegraph` are also in the devShell.
- `profiles/wallclock.{md,json}` — hyperfine means for all scenarios; `profiles/compare/<scenario>.md` — the `-O1` vs `-O0` (or any A/B) tables.
- Wall-clock per case = hyperfine mean / `testCases`; profiled builds perturb timing, so take *ratios* from the `.prof` and *absolute numbers* from `profile-time`.

## Findings (2026-07-02, -O0 vs -O1 vs -O2)

Measured with all four optimization passes applied (pre-encoding, slot pre-allocation, specialization/inlining, silent journaling): per-scenario hyperfine A/B of the default `-O1` build against `-O2` and `-O0` builds (`profiles/o1-vs-o2/`). **`-O2` is indistinguishable from `-O1` on every scenario** (ratios 1.00–1.02× with overlapping σ), while **`-O0` costs 1.8–2.2× on everything stateful**:

| scenario | -O0 | -O1 | -O2 | -O0 penalty |
|---|---|---|---|---|
| baseline | 111.5 ms | 106.3 ms | 108.0 ms | 1.05× |
| draws | 142.9 ms | 104.6 ms | 104.1 ms | 1.37× |
| payloads | 56.6 ms | 48.4 ms | 47.8 ms | 1.17× |
| steps | 381.1 ms | 205.2 ms | 200.4 ms | 1.86× |
| mixed | 2.141 s | 1.164 s | 1.165 s | 1.84× |
| heap-stress | 734.7 ms | 358.9 ms | 352.1 ms | 2.05× |
| gen-churn | 416.9 ms | 254.2 ms | 248.6 ms | 1.64× |
| shrink | 926.3 ms | 425.9 ms | 416.6 ms | 2.18× |

The `-O0` column is the same story from the other side: rewrite rules don't fire at `-O0`, so the `USPEC` specializations and INLINE work go dark and the dictionary-passing cost returns — largest exactly where the specialization pass claimed its wins (stateful, shrink), near-zero on the engine-bound per-case floor (baseline). It also bounds what an un-optimized consumer test-suite build (`cabal test -O0`) pays: ~2× on stateful-heavy properties.

Two reasons, verified rather than assumed:

- The SPECIALIZE/INLINABLE work already fires at `-O1`: the `-O1` `Stateful.hi` carries `run_$srun` and the `"USPEC run @_ @IO"` rewrite rule, so IO call sites are dictionary-free without `-O2`. (`-fspecialise` is an `-O1` flag; the pragmas, not the optimization level, were what mattered.)
- What remains after the opt passes is FFI/engine-bound, which `-O2`'s additions (SpecConstr, LiberateCase) cannot touch.

This retroactively validates profiling and timing at `-O1`: nothing was lost dropping the `-O2` release configuration.

## Findings (2026-07-02, memory stress pass)

Three stress scenarios added (`tests/profile/Stress.hs`): `heap-stress` (24 SKUs, per-step audit-log thunk chains, fat annotations), `gen-churn` (fresh dependent generator per draw — pre-encoding's no-win case), `gen-hoard` (10k generators alive as a CAF — pre-encoding's retention case). The harness suppresses all health checks (deliberately extreme workloads) and prints abort/gave-up reasons. Engine constraint discovered: ~10k draws in one case overruns the per-case choice budget — `gen-hoard` draws a 1k-wide window per case instead.

- **No leak shapes anywhere.** Heap censuses are flat for the whole run: `mixed` 5k cases / 12.8s sits at 238 KB ± 1 KB (one +35 KB blip at the end = report reconstruction); `draws`, `gen-churn`, `heap-stress` equally flat. Max residency across all realistic scenarios: 356–530 KB.
- **Per-case sawtooth is real but bounded**: a 5 ms census on `heap-stress` shows live heap cycling 241→269 KB per case — the audit/journal accumulation peaks at ~+28 KB before the next case resets it. The engine's ~50-step cap bounds per-case state; user state cannot leak across cases because `initial` re-runs.
- **Pre-encoding retention (gen-hoard)**: residency ramps to a plateau as hoard windows force cached encodings (census ~4.5 MB live at 500 cases; RTS max 12.5 MB with profiling headers), i.e. roughly ~0.5 KB per generator held alive — most of which is the generator closure + schema `Value` that existed before pre-encoding. The CAF is even collected once the run ends (census drops back to 232 KB). Bounded, proportional, converges: fine.
- **Pre-encoding churn (gen-churn)**: constructing a fresh generator per draw costs ~2.6× ticks and ~4.3× alloc vs the cached path (`encode` 32% + builder 14% + CBOR map-building 16% of ticks). Not a leak — pure garbage — but worth a user-facing doc note someday: hoist generators out of loops/binds when bounds allow.
- `heap-stress` cumulative alloc ≈ 1.8 MB/case — barely above plain `mixed` (1.7 MB/case) despite 24 SKUs and the audit log; per-step library machinery, not user state, dominates allocation.

## Deferred

- **Native sampling inside libhegel**: GHC profiling cannot see past the FFI boundary. If engine time dominates and we want to dig in: add `samply` to the devShell, tweak `nix/libhegel/default.nix` to keep debug symbols (`dontStrip`, Cargo `debug = true`), and sample the release binary (`samply record <bin> <scenario>` or Instruments on macOS).
- **tasty-bench micro-benchmarks** (e.g. CBOR encode/decode inner loops): add only once a specific function is being optimized; `notes/roadmap/02-eval-harness.md` already plans a `benchmark eval` stanza.
- **Shrink-step counter in `Hegel.Runner`** — shared follow-up with the eval note; would let `shrink` report attempts directly instead of inferring from the `valid` tally.

## Findings (2026-07-01, first capture)

Captured at `baseline 100000` / `draws 20000` / `payloads 10000` / `steps 20000` / `mixed 5000` / `shrink` (defaults). Release-build wall costs (captured on the since-removed `-O2` release configuration — not directly comparable with today's `-O1` `profile-time` numbers): ~10.7µs/case floor, ~1.1µs/draw, ~5µs/stateful step, mixed ~1.4ms/case; shrink phase ≈ 96% of a failing run.

Since the hot FFI imports are `safe` ccalls, their time does **not** tick in the `.prof` — comparing `.prof` ticks against `.rts` MUT elapsed gives the split directly. `mixed`: ~3.0s of Haskell ticks vs 9.8s MUT elapsed → roughly 30% Haskell / 70% engine (profiling inflates the Haskell share). GC is a non-issue everywhere (97%+ productivity) despite ~2MB/case allocation on mixed.

Reducible Haskell costs, ranked (details in the profiles):

1. **Schema re-encoded on every draw** — ✅ **fixed 2026-07-02**: `BasicGenerator` now carries a lazy `encoded` field (built once via the `basicGenerator` smart constructor; laziness under `StrictData` gives free memoization), `runBasic` sends it through the new `DataSource.generateEncoded`, and `frequency` floats its index-schema encode out of the per-draw closure. Result: `CBOR.Encode.encode` disappeared from the profiles; draws −43% Haskell ticks / −49% alloc, mixed −19% ticks / −16% alloc; wall-clock draws −12%, shrink −6.6%, steps −5.8%. Interactive `OneOf` still encodes its (tiny) index schema per draw — caching it means threading a field through the `OneOf` constructor; do it if a workload ever shows it.
2. **Marshalling in `FFI.generate`** — ✅ **fixed 2026-07-02**: `TestCase` now carries a `Slot` (pinned 2-word block, allocated once per case by `mkTestCase`, now in `IO`) that `FFI.generate`'s replies return through, replacing two `alloca`s per draw; `unsafeUseAsCStringLen` (ByteString buffers are pinned) replaced the `useAsCStringLen` send-side copy. Result: `allocaBytesAligned` disappeared from the profiles; on top of fix #1, draws a further −19% ticks / −33% alloc, mixed −6.5% alloc; wall-clock mixed −3.7%, shrink −4%. The result copy (`packCStringLen`) stays — the engine's reply buffer is borrowed and invalidated by the next call. Remaining per-draw cost: the safe-ccall transition itself (`$wgenerate`), CBOR decode, and the `catch` layers.
3. **Unspecialized `PropertyT` monad ops** — ✅ **fixed 2026-07-02**: INLINE/INLINABLE pragmas across the `MonadIO`-polymorphic surface (`Hegel.Property.Internal`, `Hegel.Assertion`) plus `INLINABLE`+`SPECIALIZE … IO` on `Hegel.Stateful.run`, so call sites at `IO` (the universal case) compile dictionary-free. Combined with fix #5: steps −53% ticks / −60% alloc, mixed −51% ticks / −57% alloc vs post-#2; wall-clock steps −19%, mixed −13%, shrink −18%. `draws` unchanged, as expected (already `IO`-concrete).
4. **Per-step journaling on passing runs** — ✅ **fixed 2026-07-02**: `Env.journal` is now a sum (`Silent | Recording (Note -> IO ())`). Under `Silent` (every search-phase case, including all shrink replays) `journalNote` never constructs the `Note` — its Text/SrcLoc arguments stay unforced thunks — and `Stateful.run` skips the per-step `withFailureNote` catch bracket entirely (a failure note to a silent journal is pointless; the failure propagates regardless). Only the reconstruction replay (`observeProperty`) runs `Recording`, keeping notes strict there so recorded journals can't retain machine states. Result: mixed −31% ticks / −31% alloc, steps −22% alloc; wall mixed −4.1%, shrink −3%. (Considered instead: lazy `Note` fields — one-line diff, but keeps per-note allocation, can't skip the bracket, and creates a retention hazard on the recording path.)

Also measured (2026-07-02): **report rendering**, via the `render-plain`/`render-rich` scenarios (fixed find run, then N forced renders). Per render: **~10 µs plain, ~0.65 ms rich** (discovery + splicing + Timeline doc, `loadDeclarations` inside the loop; `findDeclarations` is most of it). Rendering happens once per failure and is ~500× cheaper than the find+shrink that precedes it — nothing to optimize.
5. **Backtrace collection on control signals** — ✅ **fixed 2026-07-02**: `backtraceDesired _ = False` on `AssumeRejected`, `TestStopped`, `HegelError`, and `AssertionFailure` (which carries its own `callStack`). Subtlety: that alone didn't cover **rethrows** — the signals travel wrapped in `SomeAsyncException` (via `asyncExceptionToException`), and a `throwIO (e :: SomeException)` rethrow consults the *wrapper's* `backtraceDesired` (`True`), so `onFailure`/`tryProperty` were re-collecting ~1.8M backtraces per mixed-scenario run. Both rethrow sites now wrap in base's `NoBacktrace` (which delegates `toException`, preserving async classification). `collectBacktraces` no longer appears in any profile.
6. **`safe` ccalls on the per-draw path** — each safe call costs ~0.1–0.5µs over `unsafe` (capability release/reacquire), a real fraction of the 1.1µs/draw floor. **Verified 2026-07-02 against `references/hegel-rust/hegel-c`** (post-#4, `$wgenerate` + `$wstateMachineNextRule` = ~43% of mixed Haskell ticks):
   - *Eligible for `unsafe`*: all per-test-case primitives (`hegel_generate`, spans, collections, pools, state machine, `hegel_primitive_boolean`, `hegel_target`, `hegel_mark_complete`). They execute **inline on the calling thread** (`tc.ds` → `NativeDataSource.with_ntc` under an uncontended `Mutex` — the worker is parked while the caller drives the case), never call back into Haskell, never touch disk: `mark_complete` is in-memory (`ntc.conclude`; DB persistence happens in the worker's run loop), and unicode tables are `include_str!`-embedded, parsed once into a `OnceLock` (first text draw pays a one-time bounded CPU parse).
   - *Must stay `safe`*: `hegel_next_test_case` (blocks on the worker channel; shrink bookkeeping and database IO happen behind it), `hegel_run_start`/`hegel_run_free`/result inspection (worker lifecycle; cold anyway).
   - *The asterisk*: `Backend::Urandom` — selected explicitly or **automatically under Antithesis** — opens and reads `/dev/urandom` on every draw, inside `hegel_generate`. Under `unsafe` that blocks the capability (and delays GC sync) for a syscall pair per draw. Options: accept it (Antithesis fuzzing runs are not perf-sensitive), or dual imports (`safe` + `unsafe` variants of `hegel_generate`, chosen at run setup from the resolved backend).

Not reducible from Haskell: the per-draw engine round-trip itself (~0.6–0.8µs of the 1.1µs) and the per-case floor. Structurally reducing those means protocol-level batching (fewer round-trips per case) — an engine-side project.

## Verification

```
just profile-run baseline 100     # smoke on the dev build
just profile-space draws        # first run builds all deps for profiling (slow once)
head -40 profiles/O1/draws.prof
just profile-time                 # hyperfine table incl. the shrink attribution pair
just profile-time-compare         # per-scenario -O1 vs -O0 ratios
```

Sanity checks: `baseline` should show nearly all time under `driveLoop`/FFI wrappers; `draws` shifts inherited time into `FFI.generate` with allocation in CBOR/ByteString; back-to-back `profile-time` runs should have overlapping confidence intervals (fixed seed).
