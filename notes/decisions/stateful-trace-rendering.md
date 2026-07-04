# Stateful trace rendering — decision record

Everything here is **implemented** (the five-milestone slice of 2026-07-02).
The design exploration, survey material, and all *deferred* work live in
`notes/roadmap/02-stateful-trace-rendering.md`; this records what shipped and
the decisions not to re-litigate. Companion to
`notes/decisions/stateful-reporting.md` (the journal + splice layer this
builds on).

## What a pool-bearing stateful failure renders as (the composed report)

`renderReportRich`/`renderReportRichAnsi` produce, for a stateful
counterexample with pool context, one of two shapes depending on whether the
failing value's cited history holds a **death or handoff**
(`Blame.hasLifecycleEvent` — a consume/transfer, or a synthetic posthumous
touch):

- **with** one — the **composed report**: the `failed after N tests` headline,
  a phenomenon label when a pattern matched, the **verdict headline** (one line,
  what broke), the **citation ledger** (the failing value's lifeline as a
  failure-first slice with a mid-line link drawing the blame edges), the
  failing step's **splice** (the existing Timeline splice, demoted
  to a panel), and the reproduction **footer** when a database key exists;
- **without** — the value's story is flat (born, then touched), so the ledger's
  geometry would draw nothing worth drawing: the report **degrades** to the
  step timeline plus a compact **trajectory lead** (`↳ v₁: open @1 · use @2`)
  under the failing step, and the footer.

## Architecture: one trace, many projections

- **Event stream** (`Hegel.Internal.Event`): pool activity (`Born` with
  optional lineage / `Reused` / `Consumed` / `Named`) recorded on `TestCase`
  beside the note journal, sharing one monotonic `Tick` — recording only in
  the final reconstruction replay, zero-cost otherwise (mirrors `Journal`'s
  `Silent`/`Recording`). A pool draw's event immediately precedes its
  `Drawn` note (adjacency), which is how the trace correlates them.
  `TestCase` = engine `Handle` (ptr pair) + per-case run context (`Slot`,
  `Tick.Recording` and event buffer).
- **Trace IR** (`Hegel.Report.Trace`): `Trace.build :: [Note] -> [Event] ->
  Trace` zips the streams into steps (split on the structural `StepHeader`
  note kind; prelude step 0 keeps it total), per-value `Lifeline`s
  (birth/touches/consumption, labels, lineage), and the located `Failure`.
  Versioned (`version = 1`): the seed of the future `.hegel-trace` schema.
- **Blame** (`Hegel.Report.Trace.Blame`): `analyze` produces a rose tree of
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
  tables plus the layout knobs (budgets) ride
  `Hegel.Report.Style` — the one record every composed-report section
  consumes (`defaultStyle`; the `*With` renderers take it explicitly).

## Decisions (with the one-line rationale)

- **Death = consumed draw.** The engine has no `pool_remove`; a consuming
  draw is the death event. `Pool.transfer src dst` models close-like
  handoffs: same two engine calls, with the identity link *declared* in the
  `Born` event's lineage — never inferred (a heuristic could silently assert
  a false identity; rejected). `Pool.named` labels a pool's values for the
  report; `Pool.new` keeps auto-letters (`v, w, x…`, doubling past five).
- **Failure-first is the ledger's only direction** — the failure sits at eye
  level, history reading back beneath it. (An earlier `Chronological` knob was
  removed: one order, no configuration.)
- **The verdict is a one-line headline, shaped by the failing fact.** A
  posthumous touch (`HauntedAt`) is *itself* the violation, so the headline
  contrasts the deed with the observed outcome (`accessed v₁ after its death —
  but …`). Any other failing fact is a *benign* access (the value is incidental
  to the failure), so a `— but` would dangle over a bare return value — it
  leads with the failure reason instead, as a `location: message` line (`Step 5
  (verify): expected Nothing.`). No per-step justifications either way: the
  ledger already renders
  every cited fact at its arrowhead, so a bulleted restatement would only
  duplicate the margin. Dropping the bullets deleted the `VerdictBullet` glyph,
  and later the orphaned `Verdict.plan`/`Clause` IR the bullets had used —
  `verdictDoc` now derives its one line straight from the `Blame`.
- **Mid-line link** (between call column and annotations): justifications
  sit at their arrowheads; text stays strictly right of geometry. The call
  column clips (`⋯`) as the accepted cost. Link budget 3; overflow degrades
  to a numeric `← cites …` list.
- **Link connectors gated to cross-thread citations** (`linkMode`, default
  `Auto`). The mid-line connectors only carry what a numeric list can't when a
  citation crosses a *concurrent timeline* (thread) — see
  `notes/decisions/report-visual-grammar.md` (lanes mean concurrency; a value
  crossing is not a lane crossing). Today `Blame.citationsFor` cites only the
  blamed value's own lineage chain, and the engine is sequential, so no trace
  has a cross-thread citation: `Auto` always renders the numeric `← cites …`
  list, and the connectors would merely duplicate the per-row `observed`
  annotations while costing call-column width. Sequential (single-thread)
  reports render the numeric list; connectors return once concurrent schedule
  exploration produces cross-thread citations. `Links`/`Numeric` force either
  mode (the link-geometry pins run under `Links`).
- **Elision is explicit, always**: `⋯ n steps` rows (with "none touch h₁"
  when true), `~` history terminator, `▸ k lifelines elided (…)` footer.
- **Form selection** (`richDoc`): no pool events → today's spliced
  Timeline *byte-for-byte*; events but nothing to cite → Timeline + footer;
  a blame tree **with a lifecycle event** → the composed report; a blame tree
  **without** one → Timeline + **trajectory lead** + footer. Non-stateful
  reports unchanged.
- **The ledger is gated on a lifecycle event, not a phenomenon.** Its
  distinctive geometry (death rows, cross-pool handoff links) earns its place
  only when the cited history has a consume or transfer. Phenomenon-gating was
  rejected: no engine run produces one — a `HauntedAt` (posthumous touch) is
  structurally impossible through engine pool draws (`valuesConsumed` removes
  the value; a transfer consumes the *source* var while reads touch the *live*
  destination), so `UseAfterConsume` arises only from synthetic fixtures.
  Gating on it would have deleted the ledger from every real scenario — the
  flagship file-handle bug included (a transfer + live touch, not a haunt).
  The **trajectory lead** is the ledger's honest degradation: the failing
  value's history as a compact line in **rule names** (each `@N`
  cross-references the timeline; the rule name points back to source),
  chronological, sans the failing step — no edges, no verdict, a lead not a
  diagram (new `TraceLead` glyph, `↳` / `\->`).
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
  the ledger's `call → response` column and the verdict's contrastive outcome
  clause (the haunted case) quote it. Last one in a step wins.

## Contracts and invariants

- **Adjacency**: a pool draw's event clock immediately precedes its `Drawn`
  note's clock (pinned in `tests/unit/PoolEvents.hs`); `Trace` correlation
  relies on it.
- **One edge-set, many projections**: the verdict headline, the ledger, and
  the trajectory lead all read the same `Blame`/`Trace`; no layer re-derives
  the citation set.
- **Ascii loses no semantics**: the ascii table is injective within each
  cell family (gutter/link never share a column; pinned).
- **The spliced timeline is byte-for-byte**: pool-free stateful reports are untouched by
  the slice (pinned, plus all pre-slice pins unchanged).
- **Totality**: `Trace.build` is total on malformed streams (never-born
  vars synthesized; lineage cycles guarded in `root`/`chain`).
- **One citation per step**: `Blame` dedupes same-step facts (strongest
  wins) so the link never draws an orphan column.

## Verification

- `just test unit` — `PoolEvents`, `TraceIR`, `LedgerRendering` (byte-exact
  failure-first ledger pins in both glyph tables under `linkMode = Links`,
  plus the `Auto` default rendering numeric citations on single-thread traces;
  verdict-headline pins
  (contrastive + reason-led), trajectory-lead pins, the flat-degrades-to-
  trajectory form, glyph-preference knob, transliteration incl. the
  trajectory glyph), `DatabaseReplay` (`ReplayDiverged`), plus all pre-slice
  pins unchanged.
- `just gallery` — four scenarios; scenario 3 (a transfer) renders the composed
  report through the wired path (also in ascii), scenario 4 (a flat story)
  degrades to the timeline with a trajectory lead.
