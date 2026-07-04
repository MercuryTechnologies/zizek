# Obligation rendering — design

> **Status: design exploration, not committed work.** The obligation API itself
> is Arc 2 (`notes/roadmap/01-arc2-package-layout.md`,
> `notes/design/temporal-properties.md`) and isn't built yet; the concurrency
> braid it composes with is future (gated on the concurrency IR). This note is
> the outcome of the "regroup on obligation-API visualization" that
> `notes/roadmap/02-stateful-trace-rendering.md` flagged for after the
> lanes-mean-concurrency landings. It fixes the *visual grammar* for
> obligations; the concrete glyph tables and layout code are for when the API
> lands.

Builds on the committed grammar in `notes/decisions/report-visual-grammar.md`:
**lanes are reserved for concurrent threads; an obligation is a right-region
span, a detail-region device, never a lane.** Read that first.

## Core principles

### Obligations are spans, rendered as facts — not lanes, not geometry

An obligation is a promise made at one step (**incur**) and resolved at another
(**discharged** / **violated** / left **open** at trace end). It is rendered as
prose facts on the existing ledger, in the annotation region — never a second
gutter column. The approach we landed on ("Promissory Note"):

- The **promise is a fact on its incur row** — the ledger already narrates cited
  rows ("h₁ was created", "h₁ was transferred"); an incur is one more:
  `promised: further reads of h₁ fail`. On a row that both incurs a promise and
  has a lifecycle fact, the two share the annotation slot, joined with `·`
  (`promised: … · transferred`). The mockups below omit the shipped per-row
  lifecycle narrations (`c₁ was created`, `was accessed`) for focus; real output
  keeps them.
- The **breach cites the incur** via the existing citation channel, with the
  phrase `cites` → `violates`: `← violates 4`.
- **Expected/got is a plain diff** — the ordinary `-`/`+` block any assertion
  failure produces. Provenance ("this came from the promise at 4") lives in the
  `← violates 4` citation, *not* in a labelled diff legend. (We rejected a
  contract-diff legend `(- promised @4) (+ got)` as restating what the citation
  already says.)

### One home per fact (de-duplication)

Each fact appears in exactly one place. This was the load-bearing lesson of the
design sessions — earlier drafts stated "promised @4, broken @5" in a verdict
couplet *and* a register row *and* the ledger, three times over.

- **Headline** — the one-line verdict (what broke), greppable. Does not restate
  mechanics.
- **Ledger incur row** — the promise text (once).
- **Ledger breach row** — the `← violates N` citation + the plain diff (once).
- **Register** — *only the other* obligations (open / kept); never the starring
  violated one, which the ledger owns.
- **Splice** — source + origin only; no re-printed diff, no promise prose.

### The four-valued verdict rides the resolution

- `✗` violated (definitely false) · `?` open at trace end (probably false, a
  give-up) · `▪` discharged (kept). The fourth value, definitely-true, is a
  passing run and never renders a report. A run halts at the first violation, so
  a counterexample has one `✗`; `?`/`▪` describe the *other* obligations' final
  states.
- Bounded obligations (`within N`) fail definitely at the deadline; unbounded
  (`eventually`) degrade to the probably-false give-up.
- A give-up (or a reached deadline) that *is the starring verdict* draws a
  **synthetic terminal row** carrying a new **`?` gutter cell** (`NodeOpen`) —
  plain ASCII, already blessed as the give-up terminator, free within the gutter
  cell family — and cites its incur numerically, like any breach:

  ```
  ?  20  (trace end)      ← still owes 6     probably false, open 14 steps
  ```

  The bounded case is the same synthetic row with `✗` and a definite headline
  (reaching the deadline *is* the witness): `✗ 20 (deadline) ← violates 6`. A
  *non-starring* open obligation gets no synthetic row — it lives in the register
  only, so it keeps exactly one home.

### Glyph semantics (recap from the shipped ledger)

`●` born · `○` reused/touched · `◌` consumed **with no continuation (a death)**
· `✗` violation. The transfer rule matters: a consuming draw whose **lineage
continues** classifies as a *transfer* and draws `○` ("a handoff never words or
draws as a death"); `◌` is reserved for a consume that ends the value. So
`close h₁` in the file-handle machine is `○` (a `Pool.transfer` to the closed
pool — the lineage continues), not `◌`. Today `◌` cannot reach a rendered ledger
at all: citing a lineage-ending consume needs a later touch of the same value,
which is the engine-impossible `pool_remove` case. Cross-value blame (below) is
what first makes `◌` renderable — as a death on a *cited* strand rather than the
subject's own.

## Form S1 — sequential

The default. Promise on the incur row, breach cites it, plain diff, register as a
single `also open: … · N kept` line (or an `other promises` block at ≥2), splice
source-only, elided lifelines to the footer.

```
━━━ ConnPool ━━━
  ✗ prop_conn_lifecycle — a closed connection still served a read
    failed after 1,204 tests

    ✗ 14  read c₁ → Right (Row 7 "x")     ← violates 9
    │     - Left ConnClosed
    │     + Right (Row { id = 7 , val = "x" })
    ┆     ⋯ 3 steps, none touch c₁
    ○  9  close c₁ → ok           promised: further reads of c₁ return ConnClosed
    ┆     ⋯ 2 steps, none touch c₁
    ○  6  write c₁ (Row 7 "x") → ok
    ○  3  reset c₁ → ok
    ●  1  open c₁ → ok
    ~
    also open: flush of c₂ eventually completes (made @8) · 2 kept
    ▸ 2 lifelines elided (c₂, lk · 4 steps)

    ┏━━ tests/machines/ConnPool.hs ━━━
    287 ┃   Stateful.Rule "read" \m -> do
    288 ┃     c <- forAll (Pool.valuesReusable m.closed)
        ┃     │ c₁
    289 ┃     r <- liftIO (query m c)
    290 ┃     Stateful.respondShow r
    291 ┃     ensure c (readsReturn (Left ConnClosed))
        ┃     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ┃     │ c₁ · at ConnPool.hs:291
    292 ┃     pure m

    stored: 4e91…a7 — replays automatically next run
```

## Form C2 — concurrent (the braid)

Lanes are **threads** (straight vertical rails — no right-angle weaving; both
threads own their column for the whole trace, so the switch is just *which
column the next node lands in*). A step's glyph sits in its thread's column,
coloured by the value it concerns. Preemptions are quiet `» P→P` link rows
between adjacent shown steps, or folded into an elision qualifier when they
happen inside a collapsed run. A cross-**thread** citation (breach on one thread,
promise on another) draws the connector; the header carries the schedule
summary + context-switch counter.

```
━━━ ConnPool · 3 threads ━━━
  ✗ prop_conn_lifecycle — a closed connection still served a read
    failed after 1,204 tests · reproduces with 3 context switches

    P1 P2 P3
    ✗  │  │   6  read  c₁ → Right "x"      ●──╮
    │  │  │        - Left ConnClosed          │
    │  │  │        + Right "x"                │
    │  │  │   » P2 → P1                       │
    │  ○  │   5  close c₁ → ok      ◀─────────╯  promised: reads of c₁ return ConnClosed
    ┆  ┆  ┆      ⋯ 2 steps (P3) · P1 → P3 → P2
    ○  │  │   2  write c₁ "x" → ok
    ●  │  │   1  open  c₁ → ok
    ~  ~  ~
    also open: lk released within 6 (made @3, P3) · 1 kept
    ▸ 1 lifeline elided (lk · 2 steps)

    ┏━━ tests/machines/ConnPool.hs ━━━
    288 ┃     c <- forAll (Pool.valuesReusable m.closed)   ‹P1›
        ┃     │ c₁
    289 ┃     r <- liftIO (query m c)
    290 ┃     Stateful.respondShow r
    291 ┃     ensure c (readsReturn (Left ConnClosed))
        ┃     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ┃     │ c₁ · at ConnPool.hs:291
    292 ┃     pure m

    stored: 4e91…a7 — replays automatically next run
```

### Context-switch counter

`reproduces with N context switches` = the number of scheduler preemptions in
the *shrunk* schedule. Concurrency's analog of the minimal counterexample size:
schedule-exploration engines shrink the schedule (coalescing switches), so N is
the minimum interleaving that still triggers the bug — a shallowness gauge (low
N = a race that bites under almost-any timing) and a determinism claim (the exact
schedule is in the choice sequence and replays). Surfaced only for the
fully-shrunk counterexample, and only when the engine reports it (else omit,
don't fake — same rule as the shipped "no shrink counts" decision).

## Multiple pooled values — strands on one spine

The lifeline abstraction conveys value *lifetimes*, plural. Because columns are
reserved for threads, multiple values share the single spine and are told apart
by **colour + name**; each row's gutter glyph is that row's value's lifecycle
event. You read a lifeline by following a colour (its name is the mono/ascii
fallback), not by scanning a column.

```
  ✗ prop_shared_file — closing one handle corrupted a sibling's read
    failed after 892 tests

    ✗ 5  read  h₂ → Right "\NUL"      ← violates 4
    │    - Right "a"
    │    + Right "\NUL"
    ○ 4  close h₁ → ok            promised: other open handles to f stay readable
    ○ 3  write h₁ "a" → ok
    ● 2  open  h₂ → ok
    ● 1  open  h₁ → ok
    ~
    ▸ 1 lifeline elided (h₃ · 2 steps)
```

Here h₁ (rows 1, 3, 4) and h₂ (rows 2, 5) are two strands woven on one spine,
distinct by hue. The failing step (`read h₂`) cites `close h₁` — a **cross-value
citation**, rendered numerically (`← violates 4`), **not** a connector
(connectors stay reserved for cross-*thread*). `close h₁` draws `○` because it is
a `Pool.transfer` to the closed pool — its lineage continues; a plain-consume
close with no continuation would be h₁'s `◌` (and, per the glyph recap, only
becomes renderable *because* this cross-value citation reaches it).

Two dependencies this surfaces:

- **Needs the cross-value blame extension.** Today `Blame.citationsFor` walks
  only the subject's own lineage, so a real run can't yet cite `read h₂ ← close
  h₁`; it would show only h₂'s strand and elide h₁. Multi-value rendering is the
  strongest motivation for building cross-value (data-flow) blame. Until then,
  multi-value failures degrade to the single blamed strand + a numeric footer.
- **Identity is carried by colour, not position.** This is the deliberate
  consequence of giving columns to threads, and it resolves the two-axis
  tension: **threads own the column (position); values own the colour.** They
  never compete for the palette.

### Multiple values × multiple threads (the scaling wall)

Composed, both axes vary at once — each value weaves across the thread lanes:

```
    P1 P2
    ✗  │   5  read  h₂ → Right "\NUL"   ●──╮
    │  │        - Right "a"                │
    │  │        + Right "\NUL"             │
    │  ○   4  close h₁ → ok      ◀─────────╯  promised: other handles to f stay readable
    ○  │   3  write h₁ "a" → ok
    │  ●   2  open  h₂ → ok
    ●  │   1  open  h₁ → ok
    ~  ~
```

A failure that is *both* cross-thread (connector) and cross-value (colour
change). This is the honest ceiling: two independent categorical axes in play,
legible only because they use different channels (position vs. hue), leaning hard
on colour and degrading in mono to name-tracking. Shrinking is load-bearing — it
keeps real counterexamples at ~2 values / ~2 threads rather than a rainbow.
Treat this as the explicit scaling wall; past it, demote (elide values,
trunk-slice) as the ledger already does.

## Rules & precedence

- **The register lists only non-starring obligations.** The violated (starring)
  one lives in the ledger, so the register never carries a `✗` — only `?` (open)
  and `▪` (kept). Thresholds: no open others → no register line at all (a bare
  `N kept` isn't worth one); exactly one open other → the
  `also open: X (made @n) · N kept` one-liner; ≥2 open others → the
  `other promises` block (glyph column `?`/`▪`, `made @n · resolved @m`). Printed
  only when it earns its place, so the common single-obligation case adds no
  second place to look.

  ```
  other promises
    ? j₂ eventually delivered     made @6 · open at end
    ? lk released within 6        made @3 · open at end
    ▪ 2 kept
  ```
- **Diff vs. connector precedence.** A wide multi-line (structural) diff and the
  cross-thread connector both want the right margin. When the diff is wide, the
  connector degrades to the numeric `← violates N` — the same numeric form the
  shipped `linkMode = Auto`/`linkBudget` fallbacks already produce; the wide-diff
  trigger itself is new. You do not get both a fat diff and the connector on the
  same row.
- **Switches folded into elisions.** A preemption between two *shown* adjacent
  steps gets a `» P→P` row; switches inside a collapsed run are absorbed into the
  elision qualifier (`⋯ 2 steps (P3) · P1 → P3 → P2`). So the count of visible
  `»` rows is ≤ the header's switch count — the header states the true total.
- **One failing step; possibly many broken promises.** A run halts at the first
  violation, so a counterexample has exactly one `✗` *row* — never two failing
  steps (a later deadline never fires; the run already stopped). But one step can
  break several promises at once (two invariants failing on the same post-step
  check; two bounded deadlines coinciding), rendered as one `✗` row citing
  multiple incurs: `← violates 4, 2`. The shrinker reduces *size* (steps, values,
  threads, switches), never violation *count* — the count is capped at one step
  by stop-at-first-failure, with a fan-out of promises broken *at* that step.

## Dependencies & deferred

- **Cross-value (data-flow) blame** — required for multi-value citations
  (`read h₂ ← close h₁`); a `Blame` extension, unbuilt. Must not re-activate the
  cross-thread connectors (a value crossing is not a thread crossing — see the
  `crossThreadCitations` gate in `Hegel.Report.Trace.Ledger.layoutRows`).
- **Engine schedule reporting** — the context-switch counter and the load-bearing
  vs. incidental switch distinction want the schedule/shrink-journal exposure
  that is tier-3 gated. Degrade honestly without it (mark all switches, claim no
  count).
- **Coverage-tier glyph vetting** — `?` (gutter give-up cell) is plain ASCII;
  `▪` (discharge mark, ASCII `=`) is WGL4 but should be vetted before the tables
  land. The `»` switch row and the splice's `‹P1›` thread tag need ascii
  fallbacks too (candidates: `>>` and `<P1>`). Retire `□` (Mockup D's incur-armed
  marker): the Promissory Note approach makes the incur pure prose, so it is dead
  stock — drop it from the roadmap's coverage-vetting list when this lands. No
  circled-digit id tokens (`①②`) — rejected; obligations are keyed by step number
  (`@5`), value name, and the citation, not a synthetic id scheme.
- **`pool_remove`** (tier-3 engine) — a true `◌` death seated *below* a failure
  needs a touch-after-consume, which is engine-impossible today; not required for
  obligation rendering, noted for completeness.

## Open questions

- Register heading wording (`other promises` for the block vs `also open:` for
  the one-liner) — the thresholds are settled above; the exact words are not.
- Failure-first vs. chronological ordering for the braid (the sequential ledger
  is failure-first; the interleaving story may read better chronologically —
  needs eyeballing once real concurrent traces exist).
- Exact `»` switch-row glyph and the connector routing through intervening rows.
- How the obligation region coexists with `± ` delta rows and numeric `← cites`
  when all three co-occur (right-region column budget / ordering). Related: a
  wide structural-diff detail row inflates the shipped `callW`, pushing the whole
  annotation column right — decide whether detail rows are excluded from the
  call-width computation.
- Whether an elision row draws `┆` in every lane or keeps `│` in lanes whose
  steps were not elided (jj's tenet puts elision "on the edge").

## Rejected (so they aren't re-litigated)

- **obligation-lanes** (a second gutter column) — obligations are not lanes;
  lanes are threads. Replaced by spans-as-facts.
- **Per-row obligation rail** (`┌╴ … │ … ✗╴`) — too noisy; the per-row `│`
  connector weaving around node glyphs was the density culprit.
- **Inline obligation detail rows** (`▸ ① incur: …` under each step) — too noisy.
- **Right-margin scope brace** (`⎧⎨⎩`) — bad glyph coverage; failure-first makes
  it read upward.
- **Right-angle switch connectors** in the braid (`├──┘`) — three line-systems
  fighting; replaced by straight rails + column position + quiet `»` rows.
- **Circled-digit id tokens** (`①②`) — coverage risk, twee, and a second
  numbering scheme competing with step numbers.
- **Contract-diff legend** (`(- promised @4) (+ got)`) — restates what
  `← violates N` already carries; use a plain diff.
