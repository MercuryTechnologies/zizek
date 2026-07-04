# Report visual grammar — lanes mean concurrency

A cross-cutting decision about the failure-report renderer's visual grammar. It
constrains work beyond any one slice — the stateful-trace ledger (shipped), the
obligation system (Arc 2), and concurrent stateful testing (future) — so it
lives here rather than inside a single feature's decision record.

## The principle

**Parallel vertical lanes are reserved to mean genuine concurrency.** A *lane*
is one concurrent thread's drawn column. Lanes exist only in the (future)
braid renderer — the renderer for concurrent stateful testing, which the engine
will eventually support as deterministic, replayable schedule exploration
(loom/shuttle/dejafu-style, *not* observational linearizability). Nothing in a
*sequential* test ever fans out into lanes.

The reasoning: parallel lanes are the strongest "these things run independently,
in parallel" signal the layout has. That signal should go to the one axis it
actually depicts — concurrent execution — not to any axis that merely has
several things alive at once. Multiplicity is not parallelism. Spending lanes on
a non-concurrent axis (values, obligations) is a semiotic inversion: it gives
the parallel primitive to something that isn't parallel, and leaves genuine
concurrency — which has no good non-geometric representation — without it.

## Vocabulary (normative)

- **thread** — an IR-level concurrent timeline (a unit of independent execution).
- **lane** — a thread's drawn column. Future braid only.
- **column** — the layout engine's axis-agnostic allocation unit. A lane is one
  use of a column; "column" says nothing about what it means.
- **value → strand** — a value/lifeline is a *strand*: it rides the single
  spine, identity carried by colour + name, lifecycle by node glyphs
  (`●` born, `○` reused, `◌` consumed). A strand is never a lane.
- **obligation → span** — an obligation is a *span*: an incur→resolve interval
  drawn in the right (annotation) region. Never a lane.
- **"timeline"** stays reserved for the spliced step view (the "Timeline"
  splice). Do not use it for the concurrency axis — that's "thread".

## Consequence 1 — values are strands, not lanes

Several values alive at once in a sequential test are strands on one spine, not
parallel lanes. The shipped single-trunk ledger already *is* this
representation; it needs no new geometry, only names that say so. The colour
annotations carry this: `StrandAnn` (a value's identity index) and
`paletteColor` (the shared five-hue identity palette) — see
`Hegel.Report.Ann`. Wiring per-strand identity colour (today hardcoded to index
0) is a separate follow-on, not a grammar change.

## Consequence 2 — obligations are spans, not lanes

The obligation / temporal-property system renders as **right-region spans**, not
as a second gutter column. Architecturally an obligation-span is a
**detail-region setting** (a peer of the `±` delta rows), *not* a lane
semantics — the braid's lane-semantics choice is threads-only.

- The four-valued verdict rides the **span terminator**: `✗` violated (definite),
  `?` open at trace end (probably-false), `▪` discharged.
- **Bounded** safety obligations (`within N`) are a span with a deadline;
  **unbounded** liveness is a span running to trace-end and ending in `?`.
- The incur→resolve edge is where the connector/edge vocabulary genuinely pays
  off (an obligation *is* a cross-step interval).

Reference sketch — **a sketch, not a settled spec** (the concrete rendering is
designed when the obligation API lands; see the "regroup" note in
`notes/roadmap/02-stateful-trace-rendering.md`):

```
●   1  open  h₁ → ok        □╶① h₁ serves reads
│   4  write h₁ "a" → ok    │
◌   5  close h₁ → ok        ▪╶① retired    □╶② reads of h₁ ⇒ Left HandleClosed
┆      ⋯ 2 steps                                        ┆
✗   8  read  h₁ → Right "a"                             ✗╶② broke ② (open since 5)
~
```

This is the treatment `02-…`'s "Mockup D" already draws for liveness; the
earlier `obligation-lanes` proposal (a second gutter column) is rejected.

## Consequence 3 — `linkMode`'s cross-lane gate keys on threads

The ledger's mid-line connectors (`linkMode = Auto`) are gated to citations that
cross **concurrent timelines (threads)**, not values. No trace has more than one
thread yet, so `Auto` always renders the numeric `← cites …` list today. When
`Blame` is later enriched to cite *across values* in a sequential test
(data-flow / causal citations), that must **not** re-activate the connectors — a
value crossing is not a thread crossing; it stays on the single spine (numeric
cites + strand-identity colour). The connectors return only when concurrent
schedule exploration produces genuine cross-thread citations.

## What waits for the concurrency IR

The axis-agnostic layout engine (a `Grid`-style column allocator), the `Braid`
projection, a `thread` field on the trace's `Step` (under a schema version
bump), and cross-thread `Blame` facts all wait until the shape of a
schedule-exploration counterexample is known. Extracting them speculatively now
would fossilize a one-column API against requirements we can't yet see.
