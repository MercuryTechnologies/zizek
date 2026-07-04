# The three arcs — project structure & boundary principles

This note is the map. It fixes what `zizek` *is*, where new work belongs, and why —
so that later notes (and later selves) don't relitigate the boundary every time a
feature looks tempting to bolt onto core.

Read this before `01-arc2-package-layout.md` (next-up work) or
`02-stateful-trace-rendering.md` (deferred reporting work). The first-principles
reasoning that justifies the split lives in `notes/design/temporal-properties.md`.

## The arcs

Work sorts into three arcs by **who owns the semantics** and **whether it can be
made sound without an engine change** — not by how much code it is.

1. **Arc 1 — faithful Haskell exposure of Hegel as a property-testing library.**
   Property testing and state-machine (stateful) testing, exactly as `libhegel`
   models them, plus **rich failure reporting of those primitives** (the
   Hedgehog-heritage part). The engine owns sampling, choice-sequence
   bookkeeping, and integrated shrinking; `zizek` describes what to generate and
   interprets the replies. **This is `zizek`.**

2. **Arc 2 — sound higher-level testing patterns, no engine change.** Enrichments
   that add *test-writing surface* Hegel doesn't model directly but that are
   *finitely refutable* (safety): bounded-deadline obligations, standing
   promises, `violated`, trace-aware invariants. Sound over Arc 1 with zero
   engine change. **Lives in its own in-repo package** so it can churn while
   `zizek` stays a faithful exposure (see boundary principles below).

3. **Arc 3 — engine-gated capabilities.** Anything that can't be made sound (or
   possible) without `libhegel`/`hegel-rust` cooperation, driven as *requirements
   onto the engine* rather than faked in a Haskell layer: unbounded-liveness
   search, the four-valued verdict, `pool_remove` lifecycle phenomena,
   shrink-journal mining. Flagship: comprehensive temporal / model-checking.

## Two boundary principles

**(a) The library line is semantics vs. presentation — not richness.** What would
stop `zizek` from being "a faithful property-testing exposure" is adding *test
semantics the engine doesn't model*: a new way to express a property, a new
pass/fail meaning, a new verdict. Rich **reporting** — even causal blame, the
citation ledger, the prose verdict — adds none of that: it explains a
counterexample the engine already produced. So all of reporting is **Arc 1 /
core**, however elaborate. (Dependency direction agrees: reporting must be
reachable from the default render path in `Hspec`/`Tasty`, and an upper package
can't be depended on by `zizek`.)

**(b) Arc 2's test is "sound without an engine change" — not "no engine change."**
Give-up/liveness needs *no* engine change to produce numbers, but it can't be
*sound* without engine search (sampling only ever says "not yet," never
"never"). So it is Arc 3, not Arc 2. The line between Arc 2 and Arc 3 is
soundness, not effort.

The safety/liveness distinction is what makes (b) precise, and it cuts the
tempting "temporal logic" feature almost perfectly in half: `Within N` (bounded)
and `Standing`/`violated` are finitely refutable → Arc 2; unbounded `Eventually`
is the only true-liveness constructor → Arc 3. Full argument in
`notes/design/temporal-properties.md`.

## Current status

- **Arc 1 — shipped / on this branch.** Property + state-machine testing; the
  full reporting stack (`Report.{Trace,Blame,Verdict,Ledger,Trajectory,Glyph,
  Phrase,Style}`, `Internal.Event`, `Pool.transfer`/`named`); output-encoding
  ascii-safety (`Report.Encoding`, factored out as the first commit of this
  branch so it's independent of the ledger's glyph vocabulary). Decision records:
  `notes/decisions/stateful-reporting.md`, `stateful-trace-rendering.md`,
  `profiling-harness.md`. Deferred *reporting* faces (braid, taxonomy headlines,
  trace artifact) are in `02-stateful-trace-rendering.md`.
- **Arc 2 — planned, unwritten.** No Arc 2 code exists yet. The next-up work is
  the package layout + the safety obligation surface: `01-arc2-package-layout.md`.
- **Arc 3 — not planned on.** Gated on engine asks tracked in
  `02-stateful-trace-rendering.md` (tier 3) and `temporal-properties.md`.

## How the current tree maps to the arcs

| thing | arc | why |
|---|---|---|
| FFI, `Gen`, `Property`, `Stateful`, `Pool`, `Collection`, `Runner`, `Settings` | 1 | faithful exposure of engine primitives |
| `Report.*` (journal, splice, trace, blame, verdict, ledger, glyph) + `Internal.Event` | 1 | presentation of engine-produced counterexamples |
| `Report.Encoding` (ascii-safety) | 1 | output hygiene for any rich report |
| obligation / temporal *testing surface* (unwritten) | 2 | new sound test semantics → separate package |
| unbounded liveness, 4-valued verdict, `pool_remove`, shrink-journal | 3 | not sound / not possible without the engine |

**Identity commitment:** keep Arc 2 out of `zizek`. Putting the testing surface in
core would close the door on "faithful exposure." There are no consumers yet, so
the structure is free to set now — that is the point of this note.
