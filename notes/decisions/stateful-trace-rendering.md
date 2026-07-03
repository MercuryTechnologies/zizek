# Stateful trace rendering — decision record

Everything here is **implemented** (the five-milestone slice of 2026-07-02).
The design exploration, survey material, and all *deferred* work live in
`notes/roadmap/01-stateful-trace-rendering.md`; this records what shipped and
the decisions not to re-litigate. Companion to
`notes/decisions/stateful-reporting.md` (the journal + splice layer this
builds on).

## What a pool-bearing stateful failure renders as (the composed report)

`renderReportRich`/`renderReportRichAnsi` now produce, for a stateful
counterexample with pool context: the `failed after N tests` headline, a
phenomenon chip when a pattern matched, the **verdict paragraph** (a prose
proof: violation, "since" clauses, quoted outcome), the **citation ledger**
(the failing value's lifeline as a failure-first slice with a mid-line rail
drawing the blame edges), the failing step's **freeze-frame splice** (the
existing Timeline splice, demoted from whole-report to panel), and the
reproduction **footer** when a database key exists.

## Architecture: one trace, many projections

- **Event stream** (`Hegel.Internal.Event`): pool activity (`Born` with
  optional lineage / `Reused` / `Consumed` / `Named`) recorded on `TestCase`
  beside the note journal, sharing one monotonic `Clock` — recording only in
  the final reconstruction replay, zero-cost otherwise (mirrors `Journal`'s
  `Silent`/`Recording`). A pool draw's event immediately precedes its
  `Drawn` note (adjacency), which is how the trace correlates them.
  `TestCase` = engine `Handle` (ptr pair) + per-case run context (`Slot`,
  `Event.Log`).
- **Trace IR** (`Hegel.Report.Trace`): `Trace.build :: [Note] -> [Event] ->
  Trace` zips the streams into steps (split on the structural `StepHeader`
  note kind; prelude step 0 keeps it total), per-value `Lifeline`s
  (birth/touches/consumption, labels, lineage), and the located `Failure`.
  Versioned (`version = 1`): the seed of the future `.hegel-trace` schema.
- **Blame** (`Hegel.Report.Blame`): `analyze` produces a rose tree of
  `Observation`s — the violating one at the root, citations beneath
  (most-recent-first, one per step), each carrying a `Fact` (`BornAt` /
  `TouchedAt` / `ConsumedAt` / `TransferredAt` / `HauntedAt` — a
  lineage-continued consumption classifies as a transfer, so a handoff never
  words or draws as a death; `◌` is reserved for consumption without
  continuation). `citations`/`citationClosure`
  are the projections. Every explanatory layer walks this one edge-set, so
  prose and geometry cannot disagree (pinned by the agreement test).
- **Tables applied last**: layout emits abstract `Cell`s; glyphs come from a
  `GlyphTable` (`Glyph.unicode`/`ascii`); *words* come from a `PhraseTable`
  (`Phrase.english`) — every sentence the renderers can emit composes table
  fields plus quoted user data, never inflected. `Glyph.displayName`
  resolves value names through lineage roots (`h₁` across pools). Both
  tables plus the layout knobs (direction, budgets) ride
  `Hegel.Report.Style` — the one record every composed-report section
  consumes (`defaultStyle`; the `*With` renderers take it explicitly).

## Decisions (with the one-line rationale)

- **Death = consumed draw.** The engine has no `pool_remove`; a consuming
  draw is the death event. `Pool.transfer src dst` models close-like
  handoffs: same two engine calls, with the identity link *declared* in the
  `Born` event's lineage — never inferred (a heuristic could silently assert
  a false identity; rejected). `Pool.named` labels a pool's values for the
  report; `Pool.new` keeps auto-letters (`v, w, x…`, doubling past five).
- **Failure-first is the ledger's default direction** (chronological stays
  an option, and is the only possible order for anything streamed). Accepted
  costs recorded in the roadmap note; the verdict paragraph is
  violation-first too, so prose and geometry read in the same order.
- **Mid-line rail** (between call column and annotations): justifications
  sit at their arrowheads; text stays strictly right of geometry. The call
  column clips (`⋯`) as the accepted cost. Rail budget 3; overflow degrades
  to a numeric `← cites …` list.
- **Elision is explicit, always**: `⋯ n steps` rows (with "none touch h₁"
  when true), `~` history terminator, `▸ k lifelines elided (…)` footer.
- **The degradation ladder** (`richDoc`): no pool events → today's spliced
  Timeline *byte-for-byte*; events but nothing to cite → Timeline + footer;
  a blame tree → the composed report; the verdict line itself degrades away
  when citations are empty. Non-stateful reports unchanged.
- **Never crash on a non-UTF-8 handle — by selection, not forcing.**
  `Glyph.preference` picks ascii when stdout's encoding is not UTF-capable
  (`HEGEL_GLYPHS=ascii|unicode` overrides); the integrations then apply
  `Glyph.sevenBitClean`, which **transliterates** every known glyph (derived
  from the cell tables + the not-yet-tabled splice chrome) and
  `\xNNNN`-escapes only genuinely unknown user text. A forced
  `hSetEncoding` prototype was rejected as too blunt.
- **`ReplayDiverged` is a verdict, not an error**: the engine reported a
  failure whose blob passed (or discarded) on zizek's reconstruction replay.
  Reachable when the engine itself couldn't observe the flake (e.g. no
  shrink replays); otherwise the engine's own flaky-test check fires first.
- **The footer is honest**: `stored: <key> — replays automatically next run`
  only when persistence was on (`Report.databaseKey`); no shrink counts
  (engine-internal, no API — omitted, not faked); no CLI is advertised
  because none exists.
- **`Stateful.respond`** declares a rule's result (`Response` note kind);
  the ledger's `call → response` column and the verdict's outcome clause
  quote it. Last one in a step wins.

## Contracts and invariants

- **Adjacency**: a pool draw's event clock immediately precedes its `Drawn`
  note's clock (pinned in `tests/unit/PoolEvents.hs`); `Trace` correlation
  relies on it.
- **One edge-set, worded twice**: every step number in the verdict plan is
  in `Blame.citationClosure` (pinned).
- **Ascii loses no semantics**: the ascii table is injective within each
  cell family (gutter/rail never share a column; pinned).
- **Rung 1 is byte-for-byte**: pool-free stateful reports are untouched by
  the slice (pinned, plus all pre-slice pins unchanged).
- **Totality**: `Trace.build` is total on malformed streams (never-born
  vars synthesized; lineage cycles guarded in `root`/`chain`).
- **One citation per step**: `Blame` dedupes same-step facts (strongest
  wins) so the rail never draws an orphan column.

## Verification

- `just test unit` — `PoolEvents`, `TraceIR`, `LedgerRendering` (byte-exact
  ledger pins in both directions and both tables, verdict pins, ladder
  rungs, glyph-preference knob, transliteration), `DatabaseReplay`
  (`ReplayDiverged`), plus all pre-slice pins unchanged.
- `just gallery` — four scenarios; 3–4 render the
  composed report through the wired path (scenario 3 also in ascii).
