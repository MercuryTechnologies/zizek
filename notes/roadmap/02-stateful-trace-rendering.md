# Stateful trace rendering — deferred work

> **Re-scoped against the arc model (`00-arcs.md`).** The *reporting* faces here
> (braid/ledger engine, taxonomy headlines, trace artifact) are **Arc 1 / core**
> — presentation of engine-produced counterexamples — and the engine-gated items
> (tier 3) are **Arc 3**. The **obligation system / four-valued verdict /
> obligation-lanes** faces are the **Arc 2** testing surface and now live under
> `01-arc2-package-layout.md` + `notes/design/temporal-properties.md`: the
> obligation *API* is a separate package built over `Stateful.run`, and unbounded
> liveness / `ProbablyFalse` is deferred to Arc 3. Read the obligation-face
> subsections below as historical design input to those notes, not as the plan of
> record.

Design-session record (2026-07-02), pruned to deferred work only. **The first
slice shipped** — event stream, trace/blame IR, citation ledger, verdict
headline, composed report, `Pool.transfer`/`named` — and its rationale lives in
`notes/decisions/stateful-trace-rendering.md` (companion:
`notes/decisions/stateful-reporting.md`); those decisions are not to be
re-litigated. This note holds the design space and everything **deferred**.

**Priority is by cost, in three tiers** (the next work should come from the
top):

1. **Near-term — render over the trace we already have.** Everything that is a
   pure function of the in-memory `(trace, blame)` we already build: more
   renderers, more headline classifiers, more detail-line content, and the
   obligation system's zizek-side core. No new persistence, no interactivity,
   no engine changes.
2. **Recording & interactivity — the `.hegel-trace` artifact and what rides on
   it.** Emitting a versioned trace file, rehydrating it offline, and the
   step-through / prefix-browse / query verbs built on replay. Useful, but a
   large new surface — a format to version, a rehydration path, a session
   model — so it ranks beneath tier 1.
3. **Engine extensions.** Two asks the engine doesn't answer today, ranked
   last and not to be planned on: `pool_remove` (observing an operation on an
   already-consumed value) gates the lifecycle phenomena (use-after-close,
   double-free) and the strong-liveness search; **shrink-journal retention**
   (exposing the passing shrink attempts the engine currently runs silently)
   gates all of shrink-journal mining.

Running example used throughout: a file-handle machine where `open` puts a
handle in a `Hegel.Pool`, `write`/`read`/`transfer` draw from it, `close`
removes it. The bug: `read` on a closed handle returns stale data.

The shipped-work exploration, the landed-build log, and the mockups of shipped
output were pruned — their ground truth is now the byte-exact test pins.

## Map

### Tier 1 — near-term (over the current trace + blame)

| work item | one line | needs | status |
|---|---|---|---|
| **obligation system** | rules declare promises; an undischarged bound = definite failure; lanes + four-valued verdict are its faces | — (zizek-side; strong search → tier 3) | ready · **next** |
| **ledger engine (braid)** | multi-lane extension of the shipped single-trunk ledger; activates the cross-lane link connectors; hosts every view toggle | — | ready · **anchor** |
| **taxonomy headlines** | classify property shape → a headline word (lifecycle shapes → tier 3) | — (shape/leak); lifecycle `gated: engine` | mixed · sugar |
| **trace badge** | whole trace as one glyph-word for summary lines | wants the taxonomy word | ready · sugar |
| **observability export** | emit Hypothesis JSONL → free Tyche frontend + census data (one-way, no rehydration) | — | ready · defer |
| **P(fail) census + sparkline** | per-prefix P(fail) resampling, drawn as a ledger column | resampling infra; ledger engine | partial · defer |

**Sequencing (refreshed 2026-07-03).** The original plan led with two
cheap-and-valuable wins — program-listing and nearest-pass-diff — to make
single-value failures legible first. Both are gone: program-listing was cut
(the shipped single-trunk ledger already reads cleanly at 1–3 values), and
nearest-pass-diff proved engine-gated (tier 3, shrink-journal retention). No
cheap-and-high-value item remains, so lead with the load-bearing pure-Haskell
pair instead:

1. **obligation-api (bounded)** — highest near-term value, no engine change,
   renderer-independent; its verdict/lanes faces are cheap once it exists.
2. **the braid** — the anchor renderer; hosts the P(fail) column and
   obligation-lanes, and *activates the cross-lane link connectors* the shipped
   `linkMode = Auto` currently parks (single-lane failures render numeric
   citations — see `notes/decisions/stateful-trace-rendering.md`).

Optional cheap sugar alongside: shape-headlines → trace-badge (pure, low-cost,
low-stakes; shape-headlines produces the word the badge consumes). Defer
P(fail) (needs the braid + resampling infra), observability export (buys
external tooling more than in-terminal explanation), and everything
engine-gated (tier 3).

### Tier 2 — recording & interactivity (the trace artifact and what rides on it)

| work item | one line | needs | status |
|---|---|---|---|
| **trace artifact** | emit `.hegel-trace` (CBOR) on failure; report = pure fn of it; re-render offline | — | ready |
| ↳ replay verbs | steppable session + `explore s5` prefix browsing | trace-artifact | `gated: trace-artifact` |
| **trace query engine** | motif queries + trace diff over stored traces | trace-artifact | `gated: trace-artifact` |

### Tier 3 — engine extensions

| work item | one line | needs | status |
|---|---|---|---|
| **lifecycle phenomena** | use-after-close / double-free headline words (a face of taxonomy headlines) | `gated: engine (pool_remove)` | gated |
| **strong-liveness search** | make unbounded `eventually` obligations *searched*, not just reported (a face of the obligation system) | `gated: engine (pool_remove)` | gated |
| **shrink-journal mining** | shrink-defense, failure-census, improbability (nearest-pass-diff dropped) | `gated: engine (shrink-journal retention)` | gated |

## Foundations

### Hard requirements

- **No ANSI escapes when output is not a tty.** Color/SGR codes are what
  actually rot CI logs (raw-log downloads render as `[0;91m` soup — the
  glyphs were never the problem). Strip them under `NO_COLOR` /
  `TERM=dumb` / non-tty.
- **Never crash on a non-UTF-8 output handle — via ASCII detection, not
  encoding forcing** *(revised 2026-07-02)*. GHC derives handle
  encoding from the locale; under `LANG=C` (common in minimal containers)
  writing `●` doesn't mojibake — it **throws** (`hPutChar: invalid
  character`). The original requirement here forced `hSetEncoding h utf8`;
  a prototype landed and was backed out — mutating the host process's
  handle encoding from a library is too blunt. Decision: when the output
  handle's encoding is not UTF-capable, the integrations auto-select the
  **ascii glyph table** and apply `Glyph.sevenBitClean` — transliterating
  every known glyph and escaping only unknown user text — giving a
  7-bit-clean guarantee; `HEGEL_GLYPHS` overrides in both directions.
  **Shipped** (see the decision record); the interim crash-under-`LANG=C`
  window is closed.
- **Unicode glyphs are the default everywhere, including CI.** Modern CI
  log viewers are HTML + monospace and render box drawing fine; jj/git set
  the precedent (piping drops color, keeps glyphs). The ascii glyph table
  is an *escape hatch* (`HEGEL_GLYPHS=ascii`) for the true worst cases
  (windows-1252 pipelines, exotic log processors, `LANG=C` consumers) —
  nearly free via the table architecture, but not what CI gets by default.
  Losing *semantics* (rather than aesthetics) in the ascii table is a bug.
- **Declare `charset=utf-8`** on any artifact hegel ever serves or uploads
  itself: browsers rendering bare `text/plain` without a charset may fall
  back to the windows-1252 legacy default and shred multibyte glyphs.

### Adopted tenets (from jujutsu / Sapling's renderdag)

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

A framing that recurs below: the survey reframed the question from "how is
the trace drawn" to "what *is* the report" — Elle's is a *proof*,
Jepsen's history a *dataset*, Quint's a *session*.
Each answer became one of the work items here (the proof → the obligation
system, the dataset → the trace artifact).

## Tier 1 — near-term (over the current trace + blame)

### Ledger engine (braid)

A first pass at rendering the survey ideas directly (state-transition chain,
citation prose, obligation Gantt, recorded-session transcript) produced
visually rough results because each invented its own chrome. jj's coherence
comes from **one visual grammar that every fact rents space inside**: a
two-line block per node, a narrow glyph gutter, fixed columns. Synthesis: one
**ledger format** — `[lane gutter] [step №] [call → response]` on line one, a
dim indented detail line beneath (droppable: strip every dim line and the
report is still complete — that's the density knob), text strictly right,
elision in the gutter — and the survey ideas become columns and lane-semantics
*inside* it.

**There is one renderer; the design space is its settings**: lane semantics
(values / obligations / none), detail-line content (`±` deltas / citations /
off), revset, direction, glyph & color tables. This engine hosts the
obligation-lanes toggle and the P(fail) sparkline column. Two ideas stay
outside the grammar: the choice-sequence waterfall (a `--trace=choices`
generator/shrinker diagnostic — a different axis entirely) and the
recorded-session transcript (collapses into the repro footer until the replay
verbs exist).

**Shipped down-payment (`linkMode`, 2026-07-03).** The single-trunk ledger's
mid-line **link connectors** (`●─┬─┬─╮` / `◀─╯`) are now gated to *cross-lane*
citations (`linkMode = Auto`): a single-lane failure — every stateful failure
today — renders the numeric `← cites …` list instead, since the connectors
only carry what a number can't when they point across lanes. The connector
machinery is intact but dormant, so **the braid is what activates it**
(rationale in `notes/decisions/stateful-trace-rendering.md`).

**braid-ledger — the flagship body.** Full lifelines under the tenets above,
with qsm/Jepsen `call → response` + TLC changed-fields-only deltas on the
detail line:

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
responses by outcome, `±` lines dim, `✗` block red. The visual thesis: after
`◌` the lane goes dotted, so the failure glyph visibly lands **on a dead
lifeline** — the bug is legible before reading a word.

Stress-tested at 14 steps / 5 values / 2 transfers: renders correctly, but
eager compaction made columns drift (one value occupied 3 columns over its
life) and cost one link row per reclaimed lane. **Decision: no-shift
allocation** — reuse freed columns for new values, never shift; width is
bounded by peak concurrent liveness either way, columns stay stable for life,
and most link rows disappear.

#### Scaling & demotion policy

The braid's wall is reader working memory, not character width (2L−1 cells):
comfortable to **~4 concurrent lanes, ~25 steps, connectors spanning ≤2
lanes**; degrades steeply past that. The trunk-slice fallback scales
indefinitely. Shrinking is the equalizer — minimal counterexamples almost
always land at 1–3 values and a handful of steps, i.e. braid territory — so
the full braid is the default *final* report and the slice the regime for
everything unpolished (incomplete shrinks, verbose mid-run dumps, stale stored
replays). All variants are one layout engine + a *revset* (jj's deepest
lesson: one renderer, different filters):

```
peak live lanes ≤ 4  ∧  steps ≤ ~25    →  full braid            (all())
else                                    →  1-hop neighborhood    (ancestry(✗) ∪ values sharing a step with it)
neighborhood still over budget          →  trunk slice           (ancestry(✗))
```

Thresholds are config; demotion is always printed
(`▸ 3 lifelines elided — --trace=full to expand`), never silent.

#### Color policy

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

#### Detail-line settings

- **delta rows** *(opt-in annotation)*. Model-state deltas interleaved under
  steps, changed-fields-only, with a user projection knob (TLC's `ALIAS`
  analog) for large states. An extra text-region line per step; off by
  default (the braid carries temporality; deltas answer "what did this step
  do to the model", which today's `annotate` already half-covers).
- **two-column timelines** *(future goal)*. When/if parallel stateful testing
  lands: qsm's box-per-command two-column layout for the interleaving,
  Elle/Knossos-style contradiction prose for "no linearization exists". The
  braid's lanes-are-values idea does **not** transfer directly (parallel
  lanes are threads); keep the two lane semantics distinct.

### Obligation system

The promise/obligation mechanism: rules declare postconditions, `Stateful.run`
tracks them, and the failure is the first observation that violates an
outstanding obligation. Everything below is a face of one tracked structure,
so `obligation-api` is load-bearing — the rendering face and the verdict face
are cheap once it exists.

- **obligation-api** *(the core; zizek-side)*. The obligation API lives on
  `Machine` — rules declare promises; `Stateful.run` tracks incur/discharge —
  **not** in the engine. Bounded obligations (`within N steps`) are fully
  falsifiable with zero engine changes (an undischarged bound is a definite
  failure at that step — QuickLTL's numeric-annotation trick); unbounded
  `eventually` degrades to honest give-up reporting, and only engine-side
  formula-driven trace extension (QuickLTL required-next) would make its
  *search* strong — the one piece that must not be planned on (`gated: engine`;
  see tier 3). Consequently the IR reserves **no** verdict type:
  `Blame` is definitionally definitely-false today, and the verdict record
  (with the open obligation as payload — a nullary `ProbablyFalse` would
  guess the shape wrong) is introduced together with the lanes.
- **four-valued verdict header** *(verdict face)*. definitely / probably
  false; obligation lanes end in a give-up terminator instead of `✗`. See
  Mockup D for what the header buys once obligations exist.
- **obligation-lanes** *(rendering face; a view toggle, not a second
  renderer — hosted by the ledger engine)*. Obligations *are* lifelines:
  same gutter, glyphs, and allocation rules — `●` incur, `◌` retire, `✗`
  violate, `│` in flight — each promise named on its birth row's detail
  line. The failing glyph lands on the *promise's* lane: "the read broke
  what close promised" becomes geometry.

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
- **obligation-ledger** *(the same idea as the report's spine)*. Steps
  *observe* and *incur obligations*; the failure is the first observation
  violating an outstanding obligation (`⑤ close h₁ — incurs: reads of h₁ ⇒
  Left HandleClosed` … `⑧ read h₁ — ✗ violates ⑤`). Puts the
  *promise-creating* step on the timeline, not just the breaking one.

*Related:* the trace query engine's motif matching is "obligations reinvented
as searches" — one mechanism could serve both.

### Taxonomy headlines

A diagnosis word in the headline — greppable, composes above any layout,
generic verdict as fallback. Two classifiers feed one slot; it also supplies
the word the trace badge embeds.

- **named-phenomena** *(core; lifecycle shapes `gated: engine` — see tier 3)*.
  Jepsen's taxonomy move: detect pool-event shapes (touch-after-remove =
  use-after-close, remove-remove = double-free, insert-never-removed + count
  invariant = leak, …) and headline the diagnosis (`✗ failed — pattern:
  use-after-close`). The use-after-close / double-free shapes need
  `pool_remove` (post-consumption observation) to arise; leak is observable
  today. **Semantic Crash Bucketing** ([ASE'18]) buckets by *which
  intervention fixes it* — "deleting the close makes it pass" is a bucket key
  derivable from shrink archaeology, richer than exception location.
  [MoreBugs] (AST'16) feeds the taxonomy back into generation (suppress known
  patterns to hunt new ones). SmartCheck shows users accept machine
  generalizations when each is empirically verified.
- **shape-headlines** *(the cheaper complementary classifier)*. Bombadil's
  pattern names (invariant / guarantee / sliding window / state machine)
  prime the reader for pinpoint-`✗` vs give-up-zone.

### P(fail) census & sparkline

`partial` — needs resampling infrastructure; renders as a ledger column.
**pfail-sparkline** — per-prefix P(fail) resampling drawn as a sparkline
column beside the ledger, rail edges drawn only at the jumps. Antithesis's
causality analysis is the precedent: it cites causes by counterfactual
measurement (rewind, replay branches, plot P(bug) over time — "sharp vertical
jumps in the graph signal causally significant moments"). Between
shrink-defense and failure-census in cost; can stream partial estimates
while sampling.

Rendering decided (2026-07-02, work itself still deferred): the block-column
sparkline (`▁▄█` with `▲ 0.31→0.98` jump labels — Mockup F). Lighter variants
explored and set aside: measurement-as-citation-label (delta printed dim on
the rail annotation), staircase lane (box-drawing line chart), dot strip
chart, intensity ramp on step numbers (color-only, so it would need a text
channel underneath regardless). Block elements U+2581/2584/2588 need
coverage-tier vetting before the glyph tables land.

### Trace badge

`ready` — standalone (wants the taxonomy word). **trace-badge** — the whole
trace as one glyph-word in framework summary lines (`✗ ●○⋯○◌⋯✗
use-after-close @34/61`) — triage altitude: know which failure is trivial and
which is a monster before opening anything.

### Observability export

**observability-export** — emitting Hypothesis's JSONL observation schema
(status, structured arguments, features, coverage, timing, how_generated — one
JSONL row per case) gets a free visual frontend (Tyche) and the failure-census
dataset; it is the only extant cross-framework standard, a parallel
serialization off the in-memory trace (it consumes neither the `.hegel-trace`
format nor its renderers). One-way emit — no rehydration path — which is why it
sits in tier 1 rather than with the trace artifact.

Design warning (Tyche's user study): experts wanted *fewer, denser* views —
two chart types sufficed; more read as distraction. Confirms the
sublinear-height pin; resist dashboard-itis.

## Tier 2 — recording & interactivity (the trace artifact and what rides on it)

Jepsen's deepest lesson: history is data, renderers are pure functions over
it. The artifact is `(trace, blame tree)` — the trace plus the shipped
`Blame`. This tier is genuinely useful, but every item here is a large new
surface (a versioned on-disk format, a rehydration path, a replay session
model), so it ranks beneath the tier-1 work that renders over the in-memory
trace we already have.

### Trace artifact

- **trace-artifact** *(the core + format)*. Emit `.hegel-trace` (CBOR) on
  failure; report = pure function of it; `hegel-trace show --layout=…
  --revset=…` re-renders offline. Makes layout/direction debates
  non-ship-blocking and turns captured real traces into a renderer
  regression corpus. (`.hegel-trace` names the *format*; storage location
  decided at build time — preference on record (2026-07-02): a subdirectory
  of the database dir, e.g. `.hegel/traces/…`, namespaced defensively
  because `.hegel/` is engine-owned layout; weigh a frontend-agnostic
  `traces/` against a `zizek/` namespace.)

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

- **replay verbs** *(ride on the artifact + choice-sequence replay)*:
  - **time-travel** — choice-sequence replay makes a stored counterexample
    steppable (`:step` / `:back` / `:state` / `:why`). Farthest out; design
    the artifact format so this stays possible.
  - **prefix-explore** — every prefix is an address (stateright's
    URL-per-path): `hegel-trace explore s5` lists the rules enabled at step
    5. A lighter cut of the same replay-a-prefix machinery — counterfactual
    browsing without building a debugger.

### Trace query engine

`gated: trace-artifact`. **trace-verbs** — offline motif queries & trace diff:
Peasy's motif language (`{a}>{b}>>{c}`: immediate vs eventual succession) and
its compare/contrast diffing as `hegel-trace` verbs. Motifs are the obligation
system's obligations reinvented as searches — one mechanism could serve both.
Peasy's compare/contrast (diff two traces, divergence markers) is the trace-diff
verb's renderer precedent.

## Tier 3 — engine extensions

Two engine asks gate everything here; neither may be planned on. `pool_remove`
unlocks a *face* of two otherwise-shippable tier-1 features; shrink-journal
retention gates a whole family outright.

- **`pool_remove`** — observing an operation on an already-consumed value.
  - **lifecycle phenomena** (a face of *Taxonomy headlines*):
    touch-after-remove = use-after-close, remove-remove = double-free. The
    shapes need post-consumption observation to arise; **leak**
    (insert-never-removed + count invariant) is observable today and stays in
    tier 1.
  - **strong-liveness search** (a face of the *Obligation system*): bounded
    obligations are fully falsifiable zizek-side today; only making unbounded
    `eventually` *searched* (QuickLTL required-next trace extension) needs the
    engine. Until then unbounded obligations degrade to honest give-up
    reporting, which is tier 1.
- **shrink-journal retention** — exposing the passing shrink attempts and
  pre-shrink failing corpus the engine currently runs silently (it discards
  everything but the final counterexample blob). Gates all of shrink-journal
  mining outright; nothing in that family ships without it. nearest-pass-diff
  was dropped from the plan; the rest is detailed just below.

### Shrink-journal mining

Explain the failure from the search process the engine already ran (retained
shrink attempts, the pre-shrink failing corpus). Gated on an engine change:
the C engine runs shrink probes silently and exposes only the final
counterexample blob, so none of this data is observable Haskell-side today.

- **shrink-defense**. Every surviving step was *defended*: N shrink attempts
  failed against it. Print the defense count and boundary observations
  (`⟨held ×9⟩ · couldn't shorten "v9" below 2 chars`) — "why is this step
  here" answered empirically. Cost: retain the shrink journal. fast-check's
  verbose mode already dumps the whole shrink tree (`√`/`×` per attempt) —
  the data exists but is undigested; the innovation is *selection*.
  Hypothesis's `Phase.explain` ships the shrink-provenance version today:
  passing choice sequences already seen during shrinking are free
  experiments; bounded resampling then concludes "freely variable" (`"or any
  other generated value"`) vs load-bearing. Anti-slippage discipline: an
  **interesting origin** (exception type + location) constrains the shrinker
  so explanation never silently reasons about a different bug — extend
  hegel's origin-deduplication into any of this work.
- **failure-census** *(real analysis work; far-future)*. Mine the many
  failing cases seen *before* shrinking for shared features ("all 41 failing
  cases contain write·idle·gc·read on one session; no passing case does") —
  characterizes the failure *class* and certifies the shrunk witness as
  representative. scrutineer's AFNP algebra (∩failing − ∪passing, keep first
  divergence points) works over journal events without code coverage:
  "every failing case ran close before read; no passing case did".
- **improbability-column**. Score draws by generator likelihood and surface
  outliers (`∿ ≈1/10⁵ draw`) — separates "rare-input bug" from "everywhere
  bug", predicts repro flakiness, hints at distribution tuning.

*Which* citations to surface is a selection problem: Pernosco's dataflow
design document treats provenance chains as mostly copies — the renderer's
job is **copy-chain elision**, surfacing the steps that *transformed* the
value the failing assertion read and skipping the ones that merely
transported it. Hegel has semantic steps, so the heuristic can be crisper
than theirs.

## Explored and set aside (one line each, so they aren't re-derived)

- Filmstrip spine / transcript log / rewind view — subsumed by the braid
  (rewind's failure-first insight survives as the default order).
- Patch series & swimlane (model vs sut columns) — survives as the ledger's
  delta rows + the divergence marker idea below.
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
  they became the braid's `±` lines, the shipped citation ledger,
  obligation-lanes, and the repro footer respectively.
- Choice-sequence waterfall (otel-style; bar = choices consumed) — real but
  a different axis (generator/shrinker debugging, not failure explanation);
  park as a possible `--trace=choices` diagnostic.

## Mockups (2026-07-02): deferred slices, drawn

Provisional glyph picks (per the coverage-tier instruction to choose
safer stock when the tables are specced): `◀` replaces `◂` for rail
arrowheads, `← n` replaces `⇠ n` for numeric citations. New stock needing
coverage-tier vetting before the tables land: `□` (obligation armed, D),
`▁▄█` block elements (sparkline, F). `?` (give-up terminator) is plain
ascii.

### Mockup D — probably-false / give-up (obligation-system teaser, deferred slice)

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

### Mockup F — P(fail) sparkline inset (deferred slice)

The empirical citation census as a right-margin column on a chained-citation ledger:
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

## Sources

[Elle](https://github.com/jepsen-io/elle) ·
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
