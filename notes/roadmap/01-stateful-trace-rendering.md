# Stateful trace rendering — design explorations & deferred work

Design-session record (2026-07-02). **The first slice shipped** — event
stream, trace/blame IR, citation ledger, verdict paragraph, composed report,
`Pool.transfer`/`named` — and is recorded in
`notes/decisions/stateful-trace-rendering.md` (decisions there are not to be
re-litigated; the "landed" paragraphs in the decided-plan section below are
the fuller build log). This note remains the home of the design space, the
survey material, and everything **deferred**: R1 lanes, R2 obligation lanes
(zizek-side; see N3), F2–F5, O1–O5, the N-series, the recorded code debts
(A2/A4/A5), and the engine asks (`pool_remove`).

Running example used throughout: a file-handle machine where `open` puts a
handle in a `Hegel.Pool`, `write`/`read`/`transfer` draw from it, `close`
removes it. The bug: `read` on a closed handle returns stale data.

## Hard requirements

- **No ANSI escapes when output is not a tty.** Color/SGR codes are what
  actually rot CI logs (raw-log downloads render as `[0;91m` soup — the
  glyphs were never the problem). Strip them under `NO_COLOR` /
  `TERM=dumb` / non-tty.
- **Never crash on a non-UTF-8 output handle — via ASCII detection, not
  encoding forcing** *(revised 2026-07-02 during M1)*. GHC derives handle
  encoding from the locale; under `LANG=C` (common in minimal containers)
  writing `●` doesn't mojibake — it **throws** (`hPutChar: invalid
  character`). The original requirement here forced `hSetEncoding h utf8`;
  a prototype landed and was backed out — mutating the host process's
  handle encoding from a library is too blunt. Decision: when the output
  handle's encoding is not UTF-capable, the integrations auto-select the
  **ascii glyph table** (M3) and the ascii mode escapes non-ASCII user
  text (annotations, shown values, rule names), giving a 7-bit-clean
  guarantee; `HEGEL_GLYPHS` overrides in both directions. Accepted
  interim: until the glyph table lands, reports still crash under
  `LANG=C` (the pre-existing behavior).
- **Unicode glyphs are the default everywhere, including CI.** Modern CI
  log viewers are HTML + monospace and render box drawing fine; jj/git set
  the precedent (piping drops color, keeps glyphs). The ascii glyph table
  is an *escape hatch* (`--format=ascii`) for the true worst cases
  (windows-1252 pipelines, exotic log processors, `LANG=C` consumers) —
  nearly free via the table architecture, but not what CI gets by default.
  Losing *semantics* (rather than aesthetics) in the ascii table is a bug.
- **Declare `charset=utf-8`** on any artifact hegel ever serves or uploads
  itself: browsers rendering bare `text/plain` without a charset may fall
  back to the windows-1252 legacy default and shred multibyte glyphs.

## Adopted tenets (from jujutsu / Sapling's renderdag)

jj's log graph is the `renderdag` crate from Sapling (built for Mercurial
smartlog, explicitly as a reaction to `git log --graph` spaghetti). Its
tenets, adopted here wholesale:

1. **Columns are allocated, never woven.** Each lifeline owns a column;
   freed columns are *reused* by new values, not closed up by shifting
   neighbours. No diagonal lane swaps, ever (`╱╲╳` misalign in many fonts
   and can't express over/under).
2. **Node rows vs link rows.** Rows where a step happens never move edges
   horizontally; topology changes (birth, death, any compaction) get their
   own link rows.
3. **Layout emits abstract cell kinds; glyphs and colors are tables applied
   last.** One engine → (curved | square | ascii) × (color | mono) for free.
   The ascii table is the CI-log / font-paranoia answer, not a degraded mode.
4. **Elision is explicit and lives on the edge.** Dashed edge = "steps
   between these two were filtered out" (with a count); `~` = history
   continues past the view. Never silently truncate.
5. **Glyphs carry state, columns carry identity.** Node glyph says what
   happened; column (+ color) says to whom.
6. **Text lives strictly right of the graph region.** Causal role
   annotations ("wrote the bytes read at 14") are words in the text column,
   not geometry.

One structural advantage over jj: **we render post-hoc, jj streams.** The
whole trace is known before layout, so column allocation gets lookahead —
the blamed value always gets the trunk (leftmost) column, and "dead but
touched again later" lanes can be kept while "dead for good" columns are
reclaimed for reuse.

## Glyph vocabulary

```
●  value born (pool insert)        ┊  dead but lingering (posthumous lane)
○  value touched (pool draw)       ┆  time elided (dashed edge + count)
│  alive, untouched this step      ~  history continues past the view
◌  value destroyed (pool remove)   ✗  failing touch
```

Detail-line sigils (the dim second line of a ledger block, see R1 below):
`±` model delta (changed fields only), `▸` promise incurred/retired, `✗`
failure detail. `±` chosen over `Δ` (Greek delta has ambiguous East-Asian
width and can break column alignment in CJK-configured terminals); the
ascii glyph table maps it to `=` — *not* `~`, which already means "history
continues".

**Coverage tiers.** Raw-text-in-browser is the fragile rendering case:
browsers don't enforce a cell grid, so a glyph missing from the default
monospace font gets a fallback glyph with a *different advance width* and
the columns wobble — cosmetic degradation, not data loss, and avoidable by
staying in well-covered ranges:

- *bulletproof*: box drawing U+2500–257F, `● ○`, `⋮` — use freely.
- *solid*: `✗ ◌ ▸ ± → ⋯` — near-universal in monospace fonts.
- *risky — keep out of real output*: `→` (use `→` instead — decided),
  `⇠ ◂` (used provisionally in the R3 sketches; pick safer stock, e.g.
  `◀` or plain `<`, when the tables are specced), `∿`, circled digits
  `①②` (double-width under CJK fonts; prose-only), subscript names `h₁`
  (borderline: keep for now, `h1` in the ascii table regardless).

Crossings (a connector spanning an uninvolved lane): **occlusion** — the
uninvolved lane's `│` simply interrupts the connector (`○─│─┤`); Gestalt
reads continuation. Rejected alternatives: mixed-weight glyphs (`╂`/`┿`,
precise but font-fragile and needs a legend) and dashed-under (`╌╌│╌╌`,
loud). Occlusion is also what a painter's-algorithm renderer emits naturally.
A step touching two values: node `○` in the primary lane, junction `┤` on
the co-touched lane, light `─` between.

Provenance comes from pool events (insert/draw/remove are engine-visible),
so the braid degrades gracefully to today's flat list exactly when a machine
uses no pools. Death events (`◌`/`┊`) require pool-removal to land in the
journal — load-bearing prerequisite.

## Candidate designs

### Design 1 — smartlog slice (single column, failure-first)

The failing value's lifeline as a jj log: failure at eye level where `@`
sits, relevance decaying downward into elided history, `~` at the bottom.
Role annotations in the text column.

```
  ✗  19 · read h₁               touched h₁ after close
  │       - Left HandleClosed
  │       + Right "a"
  ┆    ⋯ 6 steps, none touch h₁
  ○  12 · close h₁              should have invalidated h₁
  ┆    ⋯ 8 steps
  ○   3 · write h₁ "a"          wrote the bytes read at 19
  ┆    ⋯ 1 step
  ●   1 · open → h₁             h₁ born
  ~
  ▸ elided lifelines: h₂ (7) · h₃ (5) · h₄ (3)
```

Constant width, O(|ancestry|) height, zero reader tracking load. Discards
cross-value context (interleaving shape; other values' stories).

### Design 2 — braid (multi-lane, chronological)

Full lifelines under the tenets above. Stress-tested at 14 steps / 5 values
/ 2 transfers: renders correctly, but eager compaction made columns drift
(one value occupied 3 columns over its life) and cost one link row per
reclaimed lane. **Decision: no-shift allocation** — reuse freed columns for
new values, never shift; width is bounded by peak concurrent liveness either
way, columns stay stable for life, and most link rows disappear.

```
    ●              1 · open → h₁            h₁ born
    │ ●            2 · open → h₂            h₂ born
    ○─┤            3 · transfer h₁→h₂ 40    drained h₁
    ○ │            4 · write h₁ "a"         wrote the bytes read at 8
    ◌ │            5 · close h₁             dead — touched later, lane kept
    ┊ ○            6 · write h₂ "x"
    ┆ ┆               ⋯ 2 steps elided
    ✗ │            8 · read h₁              posthumous touch
                        - Left HandleClosed
                        + Right "a"
```

The visual thesis: after `◌` the lane goes dotted, so the failure glyph
visibly lands **on a dead lifeline** — the bug is legible before reading a
word.

### Scaling & demotion policy

Design 2's wall is reader working memory, not character width (2L−1 cells):
comfortable to **~4 concurrent lanes, ~25 steps, connectors spanning ≤2
lanes**; degrades steeply past that. Design 1 scales indefinitely. Shrinking
is the equalizer — minimal counterexamples almost always land at 1–3 values
and a handful of steps, i.e. braid territory — so Design 2 is the default
*final* report and Design 1 the regime for everything unpolished
(incomplete shrinks, verbose mid-run dumps, stale stored replays).

All variants are one layout engine + a *revset* (jj's deepest lesson: one
renderer, different filters):

```
peak live lanes ≤ 4  ∧  steps ≤ ~25    →  full braid            (all())
else                                    →  1-hop neighborhood    (ancestry(✗) ∪ values sharing a step with it)
neighborhood still over budget          →  trunk slice           (ancestry(✗))
```

Thresholds are config; demotion is always printed
(`▸ 3 lifelines elided — --trace=full to expand`), never silent.

### Color policy

16-color ANSI only — the palette is theme-defined, so light/dark safety is
delegated to the terminal theme (why git and jj look fine everywhere).
Truecolor requires background detection + dual palettes; not worth it.
Color is a **redundant channel**: everything must survive `NO_COLOR`/pipes.

```
reserved:  red        → failure only (✗, diff)
           default fg → step text, structure
           dim (SGR 2)→ elided rows, dead segments (harmless if unsupported)
lanes:     cyan, magenta, yellow, blue, green   (in that order; red never)
never:     white/black/gray lanes (theme-fragile)
```

Highest-value use: color the value's *name in the text region* with its lane
color (`write h₂` with magenta `h₂` beside a magenta lane) — binds graph to
text with zero geometry and disambiguates reused columns. Lane order is
colorblind-aware (deutan confusion pair deferred to lane 5). Five
distinguishable hues ≈ the 4-lane readability wall: the budgets agree.

## Survey: prior art for temporality & causal chains in terminals

### Elle / Jepsen — causal chains as prose proofs ★ the lede

[Elle](https://github.com/jepsen-io/elle) (Jepsen's transactional-anomaly
checker) explains each anomaly as a minimal witness plus a **Let/Then proof
with "because" clauses**:

> Let: T1 = {...}, T2 = {...}. Then: T1 < T2, because T2 observed T1's
> append of 1 to key :y. However, T2 < T1, because T1 appended 2 after T2
> appended 1 to :x: **a contradiction!**

Three properties worth stealing: (1) every ordering edge is *justified by an
observation*, not asserted; (2) the witness is minimal — "easy to understand
and verify"; (3) the conclusion is stated as a contradiction between what
was required and what was observed. Also relevant from the wider Jepsen
toolbox: histories are process-indexed op logs (`:process` = lane), and
Knossos renders "no valid linearization" failures — prior art for parallel
testing later.

### quickcheck-state-machine — model state interleaved; parallel two-column

[qsm](https://github.com/stevana/quickcheck-state-machine) sequential
counterexamples sandwich the **model state between commands** with diff
notation on the changed bindings (`Model [_×_ (Reference Opaque) -0 +5]`),
plus symbolic vars (`Var 0`) naming step results. Parallel counterexamples
render as **two-column box-drawn timelines** (one box per command spanning
invoke→response, columns = threads) — the shape to reach for if hegel grows
parallel/linearizability testing.

### Hypothesis — the counterexample as runnable code

[Hypothesis stateful](https://hypothesis.readthedocs.io/en/latest/stateful.html)
prints falsifying examples as **copy-pasteable Python**, with `var1, var2, …`
naming rule results so the data-dependency chain is explicit in the variable
graph:

```python
state = DatabaseComparison()
var1 = state.add_key(k=b'')
state.save(k=var1, v=state.add_value(v=var1))
```

A reproduction *artifact* distinct from the visual report. Their tracker
shows the failure mode too: printed vars that don't reproduce
([#2139](https://github.com/HypothesisWorks/hypothesis/issues/2139)) — if we
emit repro code it must round-trip through the real replay machinery, not be
prose that resembles code.

### TLC (TLA+) — delta-compressed traces

TLC's `-difftrace` prints **only the variables that changed** per state; the
toolbox highlights changed values in red; `ALIAS` lets users project the
state shown. Convention to steal: state-per-step displays must be deltas by
default, with user-supplied projection as the escape hatch for big states.
(Quint inherits the same shape: counterexample = precise state sequence,
machine-readable trace export alongside the human one.)

### otel-tui / flowline — span waterfalls in terminals

Terminal trace viewers ([otel-tui](https://github.com/ymtdzzz/otel-tui),
flowline) render span trees with proportional timeline bars. Only relevant
if steps ever carry *duration* semantics (IO-heavy machines, timeout bugs).
Noted and deferred — our steps are logical time.

## New options synthesized from the survey

### Option A — the verdict paragraph (Elle-style) ★ adopt

Lead the report with a 3–5 line prose *proof* of the failure, every claim
justified by an observation, ending in the contradiction:

```
  ✗ prop_handles: read-after-close returns stale data.

    Let: h₁ = the handle opened at step 1.
    Step 4 wrote "a" through h₁ (response: ok).
    Step 5 closed h₁ (response: ok) — after which every read of h₁
      must return Left HandleClosed.
    However, step 8's read of h₁ returned Right "a" — the bytes from
      step 4: a contradiction.
```

This is the braid's annotation column elevated to a headline — same causal
edges (pool provenance + the failing assertion), rendered as words. Elle
pairs prose with a graph; we pair the verdict with the braid. The "because"
clauses come from the same role-annotation source the graph uses, so it's a
projection, not new analysis. Open question: how much of this can be derived
mechanically (edge types: born/wrote-value-later-read/destroyed/posthumous
are all pool-event patterns) vs needing user-supplied rule descriptions.
**Answered by the second survey below**: the structure is fully mechanical
given a blame-tree IR (the ERL recipe); user descriptions only improve atom
wording.

### Option B — repro snippet footer (Hypothesis-style) ★ adopt, small

End every failure with the deterministic replay incantation (we already
have reproduction blobs + the example database; this is surfacing, not new
machinery). Runnable-Haskell rendering of the trace is *not* worth it —
hegel replays through choice sequences, and Hypothesis's own issues show
code-that-doesn't-reproduce is worse than nothing.

### Option C — delta rows (TLC/qsm-style) — opt-in annotation

Model-state deltas interleaved under steps, changed-fields-only, with a
user projection knob (TLC's `ALIAS` analog) for large states. Composes with
either design as an extra text-region line per step; off by default (the
braid carries temporality; deltas answer "what did this step do to the
model", which today's `annotate` already half-covers).

### Option D — parallel two-column timelines (qsm-style) — future goal

When/if parallel stateful testing lands: qsm's box-per-command two-column
layout for the interleaving, Elle/Knossos-style contradiction prose for
"no linearization exists". The braid's lanes-are-values idea does **not**
transfer directly (parallel lanes are threads); keep the two lane semantics
distinct.

## Late round: what kind of object is the report?

The survey reframed the question from "how is the trace drawn" to "what is
the report": Elle's is a *proof*, Hypothesis's a *program*, Jepsen's history
a *dataset*, Quint's a *session*. Options in that frame:

- **F1 — obligation ledger.** Steps *observe* and *incur obligations*; the
  failure is the first observation violating an outstanding obligation
  (`⑤ close h₁ — incurs: reads of h₁ ⇒ Left HandleClosed` … `⑧ read h₁ —
  ✗ violates ⑤`). Puts the *promise-creating* step on the timeline, not just
  the breaking one. Mechanical subset from pool events; full version needs
  rules to declare postconditions (API question).
- **F2 — counterexample as a program listing.** The shrunk trace rendered as
  the do-block you'd have written (`h₁ ← open; write h₁ "a"; close h₁;
  r ← read h₁; r === Left HandleClosed -- ✗ got Right "a"`). Binding
  structure *is* data dependency — likely beats the braid for 1-value cases,
  reads worse for interleavings. Cheapest prototype in this note (journal
  pretty-print, no layout engine). Must be labeled illustration-not-repro
  (hypothesis#2139 lesson).
- **F3 — named phenomena.** Jepsen's taxonomy move: detect pool-event shapes
  (touch-after-remove = use-after-close, remove-remove = double-free,
  insert-never-removed + count invariant = leak, …) and headline the
  diagnosis (`✗ failed — pattern: use-after-close`). Greppable, composes
  above any layout, generic verdict as fallback.
- **F4 — trace as artifact.** Jepsen's deepest lesson: history is data,
  renderers are pure functions over it. Emit `.hegel-trace` (CBOR) on
  failure; report = pure function of it; `hegel-trace show --layout=…
  --revset=…` re-renders offline. Makes layout/direction debates
  non-ship-blocking and turns captured real traces into a renderer
  regression corpus. (`.hegel-trace` names the *format*; storage location
  decided at F4 time — preference on record (2026-07-02): a subdirectory
  of the database dir, e.g. `.hegel/traces/…`, namespaced defensively
  because `.hegel/` is engine-owned layout; weigh a frontend-agnostic
  `traces/` against a `zizek/` namespace.)
- **F5 — time-travel session.** Choice-sequence replay makes a stored
  counterexample steppable (`:step` / `:back` / `:state` / `:why`).
  Farthest out; design F4's format so this stays possible.

F1–F3 are alternative front matter over the same braid engine; F4 is
enabling architecture; F5 rides on F4. Sleeper pick: F2.

## Final round: the ledger grammar (R1–R4) ★ the presentation targets

A first pass at rendering the survey ideas directly (state-transition
chain, citation prose, obligation Gantt, recorded-session transcript)
produced visually rough results because each invented its own chrome. jj's
coherence comes from **one visual grammar that every fact rents space
inside**: a two-line block per node, a narrow glyph gutter, fixed columns.
Synthesis: one **ledger format** — `[lane gutter] [step №] [call →
response]` on line one, a dim indented detail line beneath (droppable:
strip every dim line and the report is still complete — that's the density
knob), text strictly right, elision in the gutter — and the survey ideas
become columns and lane-semantics *inside* it.

**There is one renderer; the design space is its settings**: lane semantics
(values / obligations / none), detail-line content (`±` deltas / citations
/ off), revset, direction, glyph & color tables. Two ideas stay outside the
grammar: the choice-sequence waterfall (a `--trace=choices`
generator/shrinker diagnostic — a different axis entirely) and the
recorded-session transcript (collapses into the repro footer until F4/F5
exist).

### R1 — step ledger (flagship body)

Braid lanes + qsm/Jepsen `call → response` + TLC changed-fields-only
deltas on the detail line:

```
●     1  open → h₁
│          ± +h₁ Open ∅
│ ●   2  open → h₂
○─┤   3  transfer h₁→h₂ 40 → ok
│ │        ± h₁ 50→10 · h₂ 0→40
○ │   4  write h₁ "a" → ok
│ │        ± h₁.buf +"a"
◌ │   5  close h₁ → ok
┊ │        ± −h₁
┆ ┆      ⋯ 2 steps (h₂ only)
✗ │   8  read h₁ → Right "a"
           ✗ expected Left HandleClosed — h₁ closed at 5
           - Left HandleClosed
           + Right "a"
~
```

Color per column, jj-style: lane glyphs in lane colors, step numbers dim,
responses by outcome, `±` lines dim, `✗` block red.

### R2 — promise lanes (`--lanes=obligations`, a view toggle)

Obligations *are* lifelines: same gutter, glyphs, and allocation rules —
`●` incur, `◌` retire, `✗` violate, `│` in flight — each promise named on
its birth row's detail line. The failing glyph lands on the *promise's*
lane: "the read broke what close promised" becomes geometry. Realizes F1's
mechanical subset; a view toggle, not a second renderer.

```
●     1  open → h₁
│          ▸ promises: h₁ serves reads
│     4  write h₁ "a" → ok
◌ ●   5  close h₁ → ok
  │        ▸ retires ①'s promise · promises: reads of h₁ ⇒ Left HandleClosed
  ┆      ⋯ 2 steps
  ✗   8  read h₁ → Right "a"
           ✗ breaks ⑤'s promise
```

### R3 — citation ledger + rail

Design 1's slice + Elle's justified edges: the failing step's citations
are **drawn** as rail edges landing (`◂`) on the cited rows, each
justification at its arrowhead — words and geometry are one edge-set and
cannot disagree. Stress-tested on an *indirect* failure (gc closed the
session because an earlier step idled it; 61→34 steps, 6 live values):

```
✗   34  read s₃ → Right "v9"          ●─┬─┬─╮
│         - Left SessionClosed          │ │ │
│         + Right "v9"                  │ │ │
┆       ⋯ 4 steps, none touch s₃        ┆ ┆ ┆
○   29  gc → closed [s₁ s₃ s₅]        ◂─╯ │ │   reads of s₃ must fail  ⇠ 21
│                                         │ │
┆       ⋯ 7 steps                         ┆ ┆
○   21  idle s₃ → ok                  ◂───╯ │   made s₃ collectable
┆       ⋯ 8 steps                           ┆
○   12  write s₃ "v9" → ok            ◂─────╯   returned exactly these bytes
┆       ⋯ 8 steps
●    3  open → s₃
~
▸ 5 lifelines elided (s₁ s₂ s₄ s₅ s₆ · 19 steps) — --trace=full
```

Rules pinned by the stress test:

- **Only the failing step gets drawn edges**; ancestors keep dim numeric
  `⇠ n` citations. One node's ancestry is a fan; everyone's is spaghetti.
- **Rail budget 3 columns**; overflow falls back to the numeric form with
  its justification under the failure block.
- **Chains draw one hop**: deeper links are numeric breadcrumbs (`⇠ 21` on
  gc's row), though transitively-cited steps do get pulled into view.
- **Revset = the failure's citation closure**, generalizing "ancestry" — a
  step appears if the edge-set reaches it, even off the trunk lifeline
  (gc isn't a touch of s₃'s pool entry; it's present via the edge).
- In R1 (multi-lane) form the rail hangs off the failing row on the right;
  lanes-left / rail-right stay disjoint regions, so crossing machinery
  never activates. Rail edges render in the lane color of the value they
  concern.
- **Rail position (decided 2026-07-02): mid-line** — between the
  call→response column and the annotations, never hard-right of the
  annotation text. Tenet 6 decides it (hard-right sandwiches text between
  two graph regions); mid-line also keeps each justification at its
  arrowhead — the words-and-geometry fusion is the point — and keeps the
  annotation column left-aligned and scannable. Cost accepted: the
  response column gets a width budget; long responses clip with an
  in-value `⋯`, the full value appearing in the failure block / splice.

### R4 — the composed report

| layer | renders when | degrades to |
|---|---|---|
| headline + pattern chip | always | chip dropped if no taxonomy match (F3) |
| verdict paragraph (A) | causal edges exist | skipped — headline suffices |
| ledger body (R1/R3) | always | braid → neighborhood → slice per budgets; `±` lines off past ~12 steps; rail numeric past 3 citations |
| freeze-frame splice | failing step's source discoverable | structured journal lines (existing per-note fallback) |
| footer | always | repro line only |

Compact example (shrunk, one value — the everyday case):

```
━━━ FileStore ━━━
  ✗ prop_handles failed at tests/FileStore.hs:41:5
    after 312 tests and 7 shrinks · pattern: use-after-close

    h₁ was opened (1), written (4), and closed (5); the read at 8
    returned 4's bytes instead of failing.

    ●    1  open → h₁
    ○    4  write h₁ "a" → ok
    ◌    5  close h₁ → ok
    ┆       ⋯ 2 steps (h₂ only)
    ✗    8  read h₁ → Right "a"

        ┏━━ tests/FileStore.hs ━━━
     40 ┃   r ← readHandle h k
     41 ┃   r === modelRead s h k
        ┃   ^^^^^^^^^^^^^^^^^^^^
        ┃   │ - Left HandleClosed
        ┃   │ + Right "a"

    reproduce: hegel-trace repl .hegel/traces/prop_handles-a3f2
```

Properties to pin when built:

- **Every layer is a projection of the same trace data** — verdict
  sentences, rail arrows, and citation numbers are one edge-set; `±` lines
  and the model in the splice are one state sequence. Nothing is authored
  twice, so nothing can disagree.
- **Report height grows sublinearly with trace complexity** — on the
  stressed 34-step case, 21 steps never render and the report is the same
  height as the simple case; complexity goes into rail columns and the
  verdict paragraph, not rows. The report also *shrinks with the
  counterexample*: small traces auto-drop lanes and detail lines.

## Further afield (oddball angles, unranked except O2)

- **O1 — shrink archaeology.** Every surviving step was *defended*: N
  shrink attempts failed against it. Print the defense count and boundary
  observations (`⟨held ×9⟩ · couldn't shorten "v9" below 2 chars`) — "why
  is this step here" answered empirically. Cost: retain the shrink journal.
- **O2 — differential pair. ★ cheap early win.** The last *passing* shrink
  attempt is the failure's nearest passing neighbor; render the failure as
  a diff against it (`- 21 idle s₃  ← without this, the run passes`). A
  counterfactual computed by machinery that already ran; needs only the
  final passing attempt retained.
- **O3 — the census.** Mine the many failing cases seen *before* shrinking
  for shared features ("all 41 failing cases contain write·idle·gc·read on
  one session; no passing case does") — characterizes the failure *class*
  and certifies the shrunk witness as representative. Real analysis work;
  far-future.
- **O4 — improbability column.** Score draws by generator likelihood and
  surface outliers (`∿ ≈1/10⁵ draw`) — separates "rare-input bug" from
  "everywhere bug", predicts repro flakiness, hints at distribution tuning.
- **O5 — timeline badge.** The whole trace as one glyph-word in framework
  summary lines (`✗ ●○⋯○◌⋯✗ use-after-close @34/61`) — triage altitude:
  know which failure is trivial and which is a monster before opening
  anything.

## Explored and set aside (one line each, so they aren't re-derived)

- Filmstrip spine / transcript log / rewind view — subsumed by Designs 1–2
  (rewind's failure-first insight survives as Design 1's default order).
- Patch series & swimlane (model vs sut columns) — survives as Option C +
  the divergence marker idea below.
- Score (rules × steps grid) — unique for swarm-subset visibility; park
  until swarm debugging demands it.
- Gauge (state trajectory vs invariant ceiling) — spectacular, narrow;
  needs numeric projections; revisit as a special-cased panel.
- Narrative prose for the *whole trace* — doesn't scale; the verdict
  paragraph keeps the good part.
- Divergence marker (`⚡ diverged at step k` ≠ observed at step n) — needs
  per-step system-state observation we don't have; double-stroke `═` rule
  reserved for it if it ever lands.
- Mixed-weight crossing glyphs, diagonal lane swaps, truecolor palettes —
  rejected above with reasons.
- Rough survey renderings (state-transition chain, citation prose block,
  obligation Gantt, recorded-session transcript) — absorbed, not discarded:
  they became R1's `±` lines, R3, R2, and the repro footer respectively.
- Choice-sequence waterfall (otel-style; bar = choices consumed) — real but
  a different axis (generator/shrinker debugging, not failure explanation);
  park as a possible `--trace=choices` diagnostic.

## Second survey (2026-07-02): Antithesis, Wickström, DST tooling, PBT explanation

Four-track web research pass run after the first draft of this note
(Antithesis; Quickstrom/Bombadil/picostrom; model-checker & DST trace UX;
Hypothesis/Tyche & counterexample-explanation research). Findings keyed to
the options they upgrade; wholly new ideas get N-numbers.

### Option A is now mechanical — the ERL recipe (Wickström 2025)

Wickström's ["Computer Says No"](https://wickstrom.tech/2025-11-01-error-reporting-linear-temporal-logic.html)
ports **Error Reporting Logic** (Jaspan & Aldrich,
[ASE'08](https://www.cs.cmu.edu/~cchristo/docs/jaspan-ASE08.pdf)) to LTL
([picostrom-rs](https://codeberg.org/owi/picostrom-rs)). The recipe:

- Normalize to NNF so every atom knows its polarity; render atoms
  **deontically** ("reads of h₁ must return Left HandleClosed") at the
  obligation site and **indicatively** with actuals ("read h₁ returned
  Right \"a\"") at the violation, joined by "but"; implication antecedents
  join with "since" — because-clauses fall out of rule shape
  (precondition ⟹ effect) structurally, not from analysis.
- Evaluation produces a **`Problem` tree**: mirrors the assertion/spec
  structure but keeps only falsity-contributing branches, every temporal
  node pinned to a numbered state (→ N1).
- **Report minimality rule** (independent of engine shrinking): two
  assertions failing at the *same* step compose into one paragraph;
  failing at *different* steps, only the first is reported.
- **Four-valued verdicts** (RV-LTL lineage): *definitely false*
  (counterexample in hand) vs *probably false* (obligation outstanding at
  trace end; gave up) — different bugs, different next actions (→ N3).
- His hand-drawn diagrams are R2's obligation lanes exactly: `○` operator
  entered, `□` obligation armed, striped band across the active span, red
  `✗` at violation — deontic sentence at the lane's left end,
  indicative-with-values at the right; give-up cases terminate in
  "probably false after N states" instead of `✗`. R2's grammar is
  independently validated; adopt the give-up terminator.

Cautionary tale attached: Quickstrom never connected the violated formula
to the violating transition ("you get 'false' … perhaps along with a
trace … It's not great"), and Wickström blames its bespoke spec languages
for killing adoption
([There and Back Again](https://wickstrom.tech/2026-01-28-there-and-back-again-from-quickstrom-to-bombadil.html)).
The failure report is the product surface. Also from that lineage:
Quickstrom's display unit is the **transition** (action + before/after
diff), matching R1's `call → response` + `±` lines; Bombadil's manual
names its property shapes (invariant / guarantee / sliding window / state
machine) → N7.

### R3's citations can be earned empirically

- **Antithesis causality analysis**
  ([blog](https://antithesis.com/blog/2026/causality_analysis/),
  [docs](https://antithesis.com/docs/debugging/causality_analysis/)) cites
  causes by counterfactual measurement: rewind, replay branches, plot
  P(bug) over time — "sharp vertical jumps in the graph signal causally
  significant moments"; the log view auto-filters to the selected window.
  Terminal miniature: per-prefix continuation resampling → a P(fail)
  sparkline column beside the ledger, rail edges drawn only at the jumps
  (→ N2).
- **Hypothesis `Phase.explain`** ships the shrink-provenance version
  today: passing choice sequences already seen during shrinking are free
  experiments; bounded resampling then concludes "freely variable" (`"or
  any other generated value"`) vs load-bearing. Its part B computes
  always-failing-never-passing code lines, filtered to *first divergence
  points*, capped, denylisted
  ([scrutineer](https://github.com/HypothesisWorks/hypothesis/blob/master/hypothesis/src/hypothesis/internal/scrutineer.py)).
  Anti-slippage discipline: an **interesting origin** (exception type +
  location) constrains the shrinker so explanation never silently reasons
  about a different bug — extend hegel's origin-deduplication into any
  O1/O2 work.
- **Pernosco** ([dataflow](https://pernos.co/about/dataflow/)) is the
  design document for *which* citations to show: provenance chains are
  mostly copies; the renderer's job is **copy-chain elision** — surface
  the steps that *transformed* the value the failing assertion read, skip
  the ones that merely transported it. Hegel has semantic steps, so the
  heuristic can be crisper than theirs.

### F4 is unanimous; Option B refinements

Every surveyed tool converged on trace-as-artifact + pure renderers: Spin
trails + flag-algebra renderers (`-p/-g/-l/-M` over one trail, 1990s);
P's three-artifact split (human `.txt` / replayable `.schedule` /
typed-JSON the Peasy visualizer purely consumes); Coyote's `.trace` +
`replay --break`; Bombadil's `test`-writes/`inspect`-reads split;
Hypothesis observability JSONL with opt-in choice sequence + spans.
Hard-won details:

- **Version the artifact from day one** (shuttle documents its schedule
  strings breaking across versions).
- **The repro key must survive the crash** (TigerBeetle prints the seed
  from the parent process: "It can explode completely, but the parent
  process will still print the seed").
- **Paste-able commands in the footer** (shuttle puts the schedule inside
  the panic message; Bombadil prints its `inspect` + `--reproduce`
  invocations after every run); the replay session opens **at the failing
  step** (Coyote `--break`), not at step 0.
- **Divergence is a first-class verdict**: Bombadil's `--reproduce` fails
  loudly when replay diverges rather than reporting nonsense.
- **Honesty counters** (Antithesis findings UX): "failed 51/852 cases;
  shrunk trace replays 1000/1000" — one line separates flake-chasing from
  bug-chasing and states the determinism promise explicitly.

### O1–O3 / F3 got concrete recipes

- O1/O2: fast-check's verbose mode dumps the whole shrink tree (`√`/`×`
  per attempt) — the data exists but is undigested; the innovation is
  *selection*. Zeller's cause-effect chains
  ([FSE'02](https://www.st.cs.uni-saarland.de/papers/fse2002/p201-zeller.pdf))
  are O2's ancestor: explanation = minimized difference against a passing
  run, every reported cause verified by actually running the flipped
  variant. Peasy's compare/contrast (diff two traces, divergence markers)
  is O2's renderer precedent.
- O3: Hypothesis's observation record (status, structured arguments,
  features, coverage, timing, how_generated — one JSONL row per case) is
  the census row schema; scrutineer's AFNP algebra
  (∩failing − ∪passing, keep first divergence points) works over journal
  events without code coverage: "every failing case ran close before
  read; no passing case did".
- F3: **Semantic Crash Bucketing**
  ([ASE'18](https://dl.acm.org/doi/10.1145/3238147.3238200)) buckets by
  *which intervention fixes it* — "deleting the close makes it pass" is a
  bucket key derivable from shrink archaeology, richer than exception
  location. [MoreBugs](https://smallbone.se/papers/more-bugs.pdf) (AST'16)
  feeds the taxonomy back into generation (suppress known patterns to
  hunt new ones). SmartCheck shows users accept machine generalizations
  when each is empirically verified.

### New ideas (N-series)

- **N1 — Problem-tree IR.** One blame structure between trace and
  renderers: verdict sentences, rail edges, and citation numbers all walk
  it. Gives the R4 pin "every layer is a projection of the same trace
  data" a concrete shape — the artifact is `(trace, blame tree)`.
- **N2 — empirical citation census.** Per-prefix P(fail) sparkline beside
  the ledger; edges drawn at the jumps. Between O2 and O3 in cost; can
  stream partial estimates while sampling. Rendering decided
  (2026-07-02, work itself still deferred): the block-column sparkline
  (`▁▄█` with `▲ 0.31→0.98` jump labels — mockup F). Lighter variants
  explored and set aside: measurement-as-citation-label (delta printed
  dim on the rail annotation), staircase lane (box-drawing line chart),
  dot strip chart, intensity ramp on step numbers (color-only, so it
  would need a text channel underneath regardless). Block elements
  U+2581/2584/2588 need coverage-tier vetting before the glyph tables
  land.
- **N3 — four-valued verdict header.** definitely/probably false;
  obligation lanes end in a give-up terminator instead of `✗`.
  Division of labor pinned (2026-07-02): N3/R2 are **zizek-side** — the
  obligation API lives on `Machine` (rules declare promises;
  `Stateful.run` tracks incur/discharge), not in the engine. Bounded
  obligations (`within N steps`) are fully falsifiable with zero engine
  changes (an undischarged bound is a definite failure at that step —
  QuickLTL's numeric-annotation trick); unbounded `eventually` degrades
  to honest give-up reporting, and only engine-side formula-driven trace
  extension (QuickLTL required-next) would make its *search* strong —
  the one piece that must not be planned on. Consequently the IR
  reserves **no** verdict type: `Blame` is definitionally
  definitely-false today, and the verdict record (with the open
  obligation as payload — a nullary `ProbablyFalse` would guess the
  shape wrong) is introduced together with R2. Decided at checkpoint 2
  follow-up; the earlier reservation was cut.
- **N4 — offline trace verbs: motif queries & trace diff.** Peasy's motif
  language (`{a}>{b}>>{c}`: immediate vs eventual succession) and its
  compare/contrast diffing as `hegel-trace` verbs; motifs are R2's
  obligations reinvented as searches — one mechanism could serve both.
- **N5 — every prefix is an address** (stateright's URL-per-path):
  `hegel-trace explore s5` lists the rules enabled at step 5. F5-lite —
  counterfactual browsing without building a debugger.
- **N6 — observability-dialect export.** Emitting Hypothesis's JSONL
  observation schema gets a free visual frontend (Tyche) and the O3
  census dataset; it is the only extant cross-framework standard.
- **N7 — property-shape taxonomy in headlines.** Bombadil's pattern names
  (invariant / guarantee / sliding window / state machine) prime the
  reader for pinpoint-`✗` vs give-up-zone; complements F3's pool-event
  taxonomy.

Design warning (Tyche's user study): experts wanted *fewer, denser* views
— two chart types sufficed; more read as distraction. Confirms the
sublinear-height pin; resist dashboard-itis.

## Decided plan (2026-07-02): R3 + verdict, IR-first

Chosen over: the pre-survey plan (R3 + F2 side by side, verdict as a
later spike — the spike is answered), artifact-first (F4 before any new
renderer — slowest to visible payoff), and a cheap-insight stack (O2 +
counters, no layout work — defers the flagship). Order of work:

1. **Pool-event recording** (prerequisite) — design decided 2026-07-02,
   **landed** (`Hegel.Internal.Event`; vocabulary reworked at
   checkpoint 1): a *separate structural event stream* on `TestCase`,
   not new `NoteKind`s. `Event {clock :: Clock, var :: Var, kind ::
   EventKind}` with `EventKind = Born | Reused | Consumed` (flat, no
   draw±consume nesting) and `Var {pool, id}` as a value's first-class
   identity (ids are only unique per pool; birth names `h₁, h₂, …` are
   renderer-assigned over `Var`s). `Event.Log = Silent | Recording …`
   mirrors the journal's `Journal` type; `mkTestCase` takes the `Log`
   directly (`Event.Silent` live/shrink, `Event.newLog` in the
   reconstruction replay), zero-cost when silent. `TestCase` itself was
   restructured at the same checkpoint: the engine pointer pair is now a
   nested `Handle {ctx, ptr}`, making `TestCase = Handle + per-case run
   context (Slot, Event.Log)` — the outer name stays `TestCase` because
   everything draw-facing (`Gen`'s `Draw` closures, `Pool`, `Env`)
   speaks it, and the env is 1:1 with the case; a full `DrawEnv` split
   of `Gen` was considered and recorded as the move if per-case context
   keeps accreting. `journalNote` stamps the same clock onto each
   `Note.clock`; the render boundary zips the two streams by stamp, so
   step association comes from ordering and the depth question
   dissolves — structural events never enter `groupByDepth`.
   Rationale: `Note` stays user-level vocabulary; the journal capability
   stays "not draw behaviour" (per the stateful-reporting decision
   record); appends keep the journal's exception property (complete the
   moment emitted); the merged stream *is* the seed of the F4 trace
   record; and `TestCase` already carries per-case mutable context
   (`slot`), so this is its natural home. Rejected: `PropertyT` wrappers
   (two-surface API; direct `IO` callers would silently produce braids
   with missing births), threading the journal + ambient depth into `IO`
   (duplicates `Env.noteDepth` into the delicate async-exception
   discipline), and engine-side reconstruction (`hegel.h` exposes only
   `new_pool`/`pool_add`/`pool_generate` — no event introspection).
   **Death = consumed draw** (revised during M1): the engine has no
   `pool_remove` either, so an explicit `Pool.remove` is impossible
   without an engine API addition — deferred. A consuming draw
   (`valuesConsumed`) is the only death event, and the event vocabulary
   is accordingly flat — `Born | Reused | Consumed` (no
   `Drawn ± consume` nesting, which also avoids a constructor collision
   with `NoteKind.Drawn`). `◌` means "consumed here"; the flagship
   use-after-close pattern is modeled with close-as-consuming-draw.
   `Pool`'s public API is unchanged (emission lives in
   `DataSource.poolAdd`/`poolGenerate`). Open sub-question: link
   `forAll`'s `Drawn` note to its pool-draw event by clock adjacency
   (start here; pinned by a unit test) or by carrying the vid on the note
   (promote only if the renderer's correlation logic gets fragile).
2. **Thin IR** (N1) — **landed** (`Hegel.Report.Trace`,
   `Hegel.Report.Blame`): `Trace.build :: [Note] -> [Event] -> Trace`
   zips the streams on the shared clock; `Trace {version, steps,
   lifelines, failure}` with `Step {index, rule, window, notes,
   response, touches, failed}`, `Touch {var, kind, note}` (draw
   notes correlated by clock adjacency), and `Lifeline {var, ordinal,
   bornAt, consumedAt, touchedAt, posthumous}`; the trace-located
   failure record is `Trace.Failure` (né `FailureInfo`). Step boundaries key on the
   structural `StepHeader !Int !Text` note kind — promoted at
   checkpoint 2, since `Trace.build`'s parser was exactly the trigger
   condition the stateful-reporting decision record set for it
   (`Note.text` keeps the rendered string, so existing renderers are
   untouched); everything before the first header is a
   prelude step (index 0, label `<initial>`), so `build` is total on
   non-stateful journals. `Blame.analyze :: Trace -> Maybe Blame`
   produces the blame tree — reshaped from the sketched
   `Violation | Since` sum to a rose tree
   `Observation {step, fact, since}` (root = the violating observation,
   children = the citations most-recent-first, deeper nesting reserved
   for indirect chains; renamed from `Problem` at checkpoint 2 — the
   children are evidence, not problems); `Fact = BornAt | TouchedAt |
   ConsumedAt | HauntedAt` (the mechanical subset; `Fact` chosen over
   `Edge`/`EdgeKind` as what the tree node observes — `Citation
   {from, to, fact}` remains the edge; `HauntedAt` = touched after
   death), `subject :: Var` (non-Maybe; `analyze` is `Nothing` when
   there is nothing to cite), `diagnosis :: Maybe Phenomenon`
   (`UseAfterConsume` — F3-lite, Jepsen's taxonomy vocabulary),
   No verdict type: a `Blame` is definitionally a definite failure, and
   verdict strength enters the IR with R2's obligation API (see N3).
   Projections: `Blame.citations` (flattened edges),
   `Blame.citationClosure :: Blame -> IntSet` (the revset). Full CBOR
   `.hegel-trace` serialization (F4) still lags; `Trace.version` (= 1)
   pins the schema. `Stateful.respond`/`respondShow` landed with a new
   `Response` note kind (existing renderers treat it as an annotation;
   `Step.response` lifts a step's last one). Pinned by
   `tests/unit/TraceIR.hs` — including that a synthetic stream *can*
   express a posthumous touch even though engine pool draws cannot,
   which keeps the flagship blame path testable ahead of any engine
   `pool_remove`.
3. **R3 citation ledger** — **landed gallery-first** (`Hegel.Report.Glyph`
   + `Hegel.Report.Ledger`; default renderer untouched): abstract `Cell`
   enum → `GlyphTable` (unicode + ascii built together; response arrow
   and numeric-cite sigils also table-routed so ascii mode leaks no
   Unicode; per-region injectivity pinned — gutter and rail families
   never share a column, so `╯`/`┊` may both map to a quote-like glyph
   without semantic loss). `Ledger.layoutRows` emits a `Row` model
   (gutter cell, step no, clipped call column, rail cells, annot);
   `ledgerDoc` aligns and annotates (lane/rail colours, dim step
   numbers/elisions, diff-coloured details). Pinned rules hold: only the
   failing row draws rail edges (mid-line, justifications at arrowheads);
   overflow → `← cites …` numeric fallback; explicit `⋯ n steps` elision
   (with "none touch vₙ" when true), `~` terminator, `▸ lifelines
   elided` footer; call column clips with in-value `⋯`. Both directions
   built; details stay under the ✗ row in each. Gallery scenarios 8–10
   were the checkpoint-3 artifact; after the checkpoint the gallery was
   trimmed to four scenarios (two spliced, two ledger), with the flagship
   file-handle machine redesigned so its *minimal* counterexample needs
   an unrelated open between write and close — the full ledger vocabulary
   (three-edge rail, elision row, lineage-linked lifeline, footer) forced
   by the bug's shape rather than decoration. Byte-exact pins in
   `tests/unit/LedgerRendering.hs`.
   Ship unicode and ascii tables together, specced against the coverage
   tiers. **Direction decided at checkpoint 3: failure-first default,
   chronological retained as the option.** Accepted costs, consciously:
   reads against time's arrow (descending step numbers are the cue); the
   ✗ row sits far from the M5 freeze-frame splice — its adjacency goes
   to the verdict paragraph instead, which is the better neighbor since
   M4's grammar is also violation-first; anything *streamed* (verbose
   mode, mid-run dumps) must stay chronological, so both directions are
   permanent; R1's braid must follow the same default when it lands (jj
   proves lifelines work top-down). R1 is the same
   engine with lanes switched on; build it second. Also decided at this
   checkpoint, on real gallery output: the **two-pool reconnection
   question**. With death = consumed draw, an exercisable
   use-after-close machine models close as consume-from-open +
   add-to-closed — two engine `Var`s for one logical handle, so the
   blame chain reads `read → BornAt(close step)` and
   `PosthumousTouch`/`UseAfterConsume` never fire on real traces.
   **Decided at checkpoint 3: (c) `Pool.transfer`, alone — no heuristic
   fallback** (an inferred link can silently assert a false identity,
   disqualifying for a report whose thesis is every-edge-justified; the
   exactly-one-consume-one-bear inference is recorded here as rejected).
   Landed with the checkpoint: `Event.Born` carries `Maybe Var` lineage
   (schema touch done pre-F4, deliberately); `Pool.transfer src dst` =
   consuming draw + `poolAddFrom` with the declared link — zero engine
   changes; `Trace.Lifeline` gains `lineage`, with `Trace.root`/`chain`
   resolving the logical value; `Blame.citationsFor` cites across the
   chain (root birth, all touches, each consumption); the ledger names,
   gutters, elisions, and footer all resolve through the root. Gallery
   scenario 8 now renders the flagship one-lane story
   (`✗ read ← ◌ close ← ○ write ← ● open`, all `h₁`). Also landed:
   **`Pool.named`** (a `Named` event kind carries the label;
   `Lifeline.label`; `Glyph.valueName` consults it — `h₁` instead of
   `v₁`; `Pool.new` stays for auto-named quick use), and the **ascii
   picks** `◌→%` (was digit-confusable `0`) and `┊→.` (density gradient
   `| : .`).
4. **Verdict paragraph** — **landed** (`Hegel.Report.Verdict`), with a
   prose-bounding architecture decided at its checkpoint: the blame tree
   projects to a wordless `[Clause]` plan (`Violated`/`Since`/`Returned`/
   `FailedWith` — data, serializable, one edge-set with the rail, pinned
   by the agreement test), and words come from a `PhraseTable` applied
   last — the prose twin of the glyph tables. Every sentence the verdict
   can emit is a composition of the English table's ~10 fields plus
   *quoted* user data (names, responses, messages — never inflected), so
   the linguistic surface is enumerable and another locale is another
   table (bounding discipline first; dev-tool diagnostics are
   conventionally English-only). The mockups' semantic deontic clauses
   ("every read of a closed handle must…") are *not* mechanically
   derivable — they need rule descriptions (deferred API); the mechanical
   paragraph states the violation, the since-chain, and the quoted
   outcome. `Nothing` when there are no citations (headline suffices).
5. **Footer**: paste-able replay command opening at the failing step +
   honesty counters; divergence as a distinct verdict when replay breaks.

Code review record (2026-07-02, pre-M5). Fixed: lineage-cycle totality
guards on `Trace.root`/`chain`; one-citation-per-step dedupe in
`Blame.citationsFor` (two same-step touches used to orphan a rail
column); pool letters double past five pools (`vv, ww, …`) instead of
silently colliding; **all renderer words unified into one
`Hegel.Report.Phrase.PhraseTable`** (the ledger's arrowhead/elision/
footer/cites text had been a second untabled English vocabulary; both
renderers now agree by construction) and `displayName` moved to
`Hegel.Report.Glyph` (killing the Verdict→Ledger dependency). Recorded
debt, with triggers: **A2** — `Row.call`/`annot` carry pre-rendered
glyph+phrase text, so the layout pins are table-coupled; restructure to
abstract row content when A4/M5/R1 reshape `Row` anyway (doing it now =
three pin churns for one). **A4** — `layoutRows` is a where-monolith;
decompose when M5's composition reveals the seams. **A5** — converge
`Ledger.Options`/verdict params into one style record when M5 wires the
composed report (partially done: `Options.phrases`). Smaller items
(fixture dedup across test suites, `Segment.header` tuple→record,
`lifelinesOf` fold clarity, name-in-lane-colour rule) batch
opportunistically.

Deferred from this slice (recipes recorded above so nothing re-derives):
R1 lanes, R2 obligation lanes, F2 program listing, O1–O3
archaeology / differential pair / census, N2 sparkline, N4–N6 offline
verbs, N6 observability export.

## Mockups (2026-07-02): the decided slice, drawn

Provisional glyph picks (per the coverage-tier instruction to choose
safer stock when the tables are specced): `◀` replaces `◂` for rail
arrowheads, `← n` replaces `⇠ n` for numeric citations. New stock needing
coverage-tier vetting before the tables land: `□` (obligation armed, D),
`▁▄█` block elements (sparkline, F). `?` (give-up terminator) is plain
ascii.

### Mockup A — composed report, failure-first (the default target)

Full R4 stack on the running example: header + counters, verdict
paragraph, R3 ledger with rail, freeze-frame splice, footer.

```
━━━ FileStore ━━━
  ✗ prop_handles — definitely false · pattern: use-after-close
    at tests/FileStore.hs:41:5
    after 312 tests and 7 shrinks · failed 51/852 · shrunk trace replays 1000/1000

    Every read of a closed handle must return Left HandleClosed,
    but step 8's read of h₁ returned Right "a" — the bytes written
    at step 4 — since h₁ was closed at step 5: a contradiction.

    ✗    8  read h₁ → Right "a"        ●─┬─╮
    │         - Left HandleClosed        │ │
    │         + Right "a"                │ │
    ┆       ⋯ 2 steps (h₂ only)          ┆ ┆
    ◌    5  close h₁ → ok             ◀──╯ │   reads of h₁ must now fail
    ○    4  write h₁ "a" → ok         ◀────╯   wrote the bytes read at 8
    ┆       ⋯ 2 steps
    ●    1  open → h₁
    ~
    ▸ 1 lifeline elided (h₂ · 2 steps) — --trace=full

       ┏━━ tests/FileStore.hs ━━━
    40 ┃   r ← readHandle h k
    41 ┃   r === modelRead s h k
       ┃   ^^^^^^^^^^^^^^^^^^^^
       ┃   │ - Left HandleClosed
       ┃   │ + Right "a"

    reproduce: hegel-trace repl .hegel/traces/prop_handles-a3f2 --at 8
```

Note how the verdict paragraph and the rail annotations are the same
edge-set worded twice (deontic sentence on the close row ↔ "since h₁ was
closed at step 5"); N1 guarantees they cannot disagree.

### Mockup B — same trace, chronological

For the direction decision (plan step 3). The rail origin sits at the
bottom; corners flip (`╮` at the cited rows, `┴` on the origin row).

```
    ●    1  open → h₁
    ┆       ⋯ 2 steps
    ○    4  write h₁ "a" → ok         ◀────╮   wrote the bytes read at 8
    ◌    5  close h₁ → ok             ◀──╮ │   reads of h₁ must now fail
    ┆       ⋯ 2 steps (h₂ only)          ┆ ┆
    ✗    8  read h₁ → Right "a"        ●─┴─╯
              - Left HandleClosed
              + Right "a"
```

Chronological reads as a story (born → written → closed → ✗) and matches
the existing plain renderer's order; failure-first puts the diff at eye
level and matches jj's `@`-at-top instinct. Live traces decide.

### Mockup C — indirect failure, chained citations (the stress case)

The gc/session scenario from R3, now with the verdict paragraph chaining
"since" clauses one hop deep, and the numeric breadcrumb (`← 21`) for the
second hop on gc's row.

```
━━━ SessionStore ━━━
  ✗ prop_sessions — definitely false · pattern: use-after-close
    after 4981 tests and 23 shrinks · shrunk trace replays 1000/1000

    Every read of a collected session must return Left SessionClosed,
    but step 34's read of s₃ returned Right "v9" — the bytes written
    at step 12 — since gc closed s₃ at step 29, which was possible
    since step 21 idled it: a contradiction.

    ✗   34  read s₃ → Right "v9"       ●─┬─┬─╮
    │         - Left SessionClosed       │ │ │
    │         + Right "v9"               │ │ │
    ┆       ⋯ 4 steps, none touch s₃     ┆ ┆ ┆
    ○   29  gc → closed [s₁ s₃ s₅]    ◀──╯ │ │   reads of s₃ must now fail  ← 21
    ┆       ⋯ 7 steps                      ┆ ┆
    ○   21  idle s₃ → ok              ◀────╯ │   made s₃ collectable
    ┆       ⋯ 8 steps                        ┆
    ○   12  write s₃ "v9" → ok        ◀──────╯   returned exactly these bytes
    ┆       ⋯ 8 steps
    ●    3  open → s₃
    ~
    ▸ 5 lifelines elided (s₁ s₂ s₄ s₅ s₆ · 19 steps) — --trace=full

    reproduce: hegel-trace repl .hegel/traces/prop_sessions-9c41 --at 34
```

### Mockup D — probably-false / give-up (N3 + R2 teaser, deferred slice)

What the four-valued header buys once obligations exist. The lane ends in
`?`, not `✗`; the headline says gave-up, not counterexample.

```
━━━ JobQueue ━━━
  ? prop_delivery — probably false (obligation open at trace end)
    after 500 tests · step cap 20 reached

    Every enqueued job must eventually be delivered, but j₂
    (enqueued at step 6) was still undelivered when the trace
    ended — probably false after 14 further steps.

    ●    6  enqueue j₂ → ok            □──╮   j₂ must eventually be delivered
    ┆       ⋯ 13 steps (j₂ never drawn)   ┆
    ?   20  (trace end)                ◀──╯   still open after 14 steps
```

### Mockup E — ascii table of Mockup A (semantics-preserving)

Strawman ascii stock: `✗→x` `●→*` `○→o` `◌→0` (vet: digit-confusable)
`│→|` `┆┊→:` `─→-` `╮→.` `╯→'` `┬┴→+` `◀→<` `▸→>` `⋯→...` `━→=` `·→.`.

```
=== FileStore ===
  x prop_handles -- definitely false . pattern: use-after-close
    after 312 tests and 7 shrinks . failed 51/852 . shrunk trace replays 1000/1000

    Every read of a closed handle must return Left HandleClosed,
    but step 8's read of h1 returned Right "a" -- the bytes written
    at step 4 -- since h1 was closed at step 5: a contradiction.

    x    8  read h1 -> Right "a"       *-+-.
    |         - Left HandleClosed        | |
    |         + Right "a"                | |
    :       ... 2 steps (h2 only)        : :
    0    5  close h1 -> ok            <--' |   reads of h1 must now fail
    o    4  write h1 "a" -> ok        <----'   wrote the bytes read at 8
    :       ... 2 steps
    *    1  open -> h1
    ~
    > 1 lifeline elided (h2 . 2 steps) -- --trace=full

    reproduce: hegel-trace repl .hegel/traces/prop_handles-a3f2 --at 8
```

### Mockup F — N2 sparkline inset (deferred slice)

The empirical citation census as a right-margin column on C's ledger:
P(fail | prefix ending at this row), `▲` at the jumps that earn rail
edges. Failure-first means probability decreases reading downward.

```
    ✗   34  read s₃ → Right "v9"       ●─┬─┬─╮    █
    ┆       ⋯ 4 steps                    ┆ ┆ ┆    █
    ○   29  gc → closed [s₁ s₃ s₅]    ◀──╯ │ │    █ ▲ 0.31→0.98
    ┆       ⋯ 7 steps                      ┆ ┆    ▄
    ○   21  idle s₃ → ok              ◀────╯ │    ▄ ▲ 0.06→0.31
    ┆       ⋯ 8 steps                        ┆    ▁
    ○   12  write s₃ "v9" → ok        ◀──────╯    ▁
```

Sources: [Elle](https://github.com/jepsen-io/elle) ·
[G1c](https://jepsen.io/consistency/phenomena/g1c) ·
[quickcheck-state-machine](https://github.com/stevana/quickcheck-state-machine) ·
[Hypothesis stateful](https://hypothesis.readthedocs.io/en/latest/stateful.html) ·
[hypothesis#2139](https://github.com/HypothesisWorks/hypothesis/issues/2139) ·
[TLC options](https://docs.tlapl.us/using:tlc:start) ·
[learntla CLI](https://learntla.com/topics/cli.html) ·
[Quint REPL](https://quint-lang.org/docs/repl) ·
[otel-tui](https://github.com/ymtdzzz/otel-tui)

Second-survey sources:
[Computer Says No](https://wickstrom.tech/2025-11-01-error-reporting-linear-temporal-logic.html) ·
[ERL, Jaspan & Aldrich ASE'08](https://www.cs.cmu.edu/~cchristo/docs/jaspan-ASE08.pdf) ·
[picostrom-rs](https://codeberg.org/owi/picostrom-rs) ·
[Quickstrom PLDI'22](https://arxiv.org/abs/2203.11532) ·
[There and Back Again (Bombadil)](https://wickstrom.tech/2026-01-28-there-and-back-again-from-quickstrom-to-bombadil.html) ·
[Bombadil manual](https://antithesishq.github.io/bombadil/) ·
[Antithesis causality analysis](https://antithesis.com/blog/2026/causality_analysis/) ·
[Antithesis multiverse debugging](https://antithesis.com/blog/multiverse_debugging/) ·
[Antithesis findings](https://antithesis.com/docs/product/reports/findings/) ·
[Hypothesis Phase.explain / scrutineer](https://github.com/HypothesisWorks/hypothesis/blob/master/hypothesis/src/hypothesis/internal/scrutineer.py) ·
[Hypothesis observability](https://hypothesis.readthedocs.io/en/latest/reference/integrations.html) ·
[Tyche UIST'24](https://harrisongoldste.in/papers/uist24-tyche.pdf) ·
[MacIver & Donaldson ECOOP'20](https://drops.dagstuhl.de/entities/document/10.4230/LIPIcs.ECOOP.2020.13) ·
[Multi-bug discovery](https://hypothesis.works/articles/multi-bug-discovery/) ·
[Zeller FSE'02](https://www.st.cs.uni-saarland.de/papers/fse2002/p201-zeller.pdf) ·
[SmartCheck](https://leepike.github.io/pub_pages/smartcheck.html) ·
[MoreBugs AST'16](https://smallbone.se/papers/more-bugs.pdf) ·
[Semantic Crash Bucketing ASE'18](https://dl.acm.org/doi/10.1145/3238147.3238200) ·
[Pernosco dataflow](https://pernos.co/about/dataflow/) ·
[stateright Explorer](https://www.stateright.rs/seeking-consensus.html) ·
[P / Peasy trace visualizer](https://p-org.github.io/peasy-ide-vscode/) ·
[Coyote replay](https://microsoft.github.io/coyote/get-started/using-coyote/) ·
[TigerBeetle VOPR](https://github.com/tigerbeetle/tigerbeetle/blob/main/docs/internals/vopr.md) ·
[shuttle](https://docs.rs/shuttle/latest/shuttle/) ·
[loom](https://docs.rs/loom/latest/loom/) ·
[Spin trails](https://spinroot.com/spin/Man/Spin.html) ·
[counterexample-explanation SLR](https://arxiv.org/abs/2201.03061)
