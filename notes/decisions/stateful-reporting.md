# Stateful reporting — decision record

Consolidates the former `notes/01-stateful-test-reporting.md` (journal
design, interleaved plain rendering) and `notes/03-stateful-rich-rendering.md`
(rich source-spliced rendering). Everything here is **implemented**. The
next-generation trace-rendering exploration lives in
`notes/roadmap/01-stateful-trace-rendering.md`.

## Journal design & rationale

**Depth-stamped flat journal, not a tree.** The journal is an append-only
streaming sink (`journal :: Note -> IO ()` into an IORef `Seq`) that must
record correctly across exception boundaries — the failure *is* an exception
arriving mid-step. Depth stamping has no "close group" operation, so there is
nothing to unbalance: every note is complete and correctly placed the moment
it is emitted, and an exception simply stops further emissions. A tree-shaped
journal would need open/emit/close (or a zipper cursor) threaded through
`Env`, with balanced-bracket guarantees maintained through the same
async-exception discipline `catchControl`/`onFailure` already make delicate.
And it buys nothing: the tree is fully recoverable from the depth stamps by a
small pure fold at render time (`Hegel.Report.Journal.groupByDepth`). The
flat journal is the wire format; the tree belongs at the render boundary.

**`Env.noteDepth` + `local`, not explicit parameters or journal-wrapping.**
Ambient, dynamically-scoped reporting context is exactly what ReaderT `local`
is for; explicit depth parameters would ripple through every note-emitting
signature, and wrapping the journal callback in `nested` hides the depth in a
closure for no gain. Like the `journal` capability itself, this varies by
context but is explicitly *not* draw behaviour.

**Suppression contract (implicit tri-module coupling).** `Stateful.run`
journals a `Failure` note whose text/loc/diff mirror what `failureDetails`
extracts; `reconstructProperty` still populates `Counterexample.{message,loc,
diff}` from the same exception (non-stateful reports and framework
integrations need them); the renderer suppresses the top-level copy when a
`Failure` note is present. Three modules must agree — documented in haddocks
and pinned by the "must be suppressed … so they do not appear twice" test.
The predicate that embodies it is `hasInBandFailure`.

## Interleaved plain rendering

Stateful (`Hegel.Stateful`) counterexample reports nest each step's draws
under its `Step N` header and render the failure in-band at the step that
produced it:

```
failed after 5038 tests
  Initial invariant check.
  Step 1: push
    Draw 1: 0
  Step 2: push
    Draw 1: 1
  Step 3: check_palindrome
    ✗ === failed, values are not equal
        (- lhs) (+ rhs)
        - Stack [ 1 , 0 ]
        + Stack [ 0 , 1 ]
      at Spec.hs:23
```

Mechanism: `Note` carries `depth :: !Int` (0 = top level) and `NoteKind`
carries `Failure (Maybe Diff)`; `journalNote` is the sole `Note` construction
site and stamps the ambient `Env.noteDepth`; rule and invariant bodies run
under `nested`. `Stateful.run`'s `withFailureNote` catches a real
(synchronous, non-control) failure via `onFailure`, journals it, and
re-throws so the runner still classifies the case `Interesting` and drives
shrinking. `onFailure` is composed *inside* the `catchControl` bracket — do
not replace with a bare `catch @SomeException`, which would swallow
discard/stop signals.

## Rich rendering: the layouts

Two pure projections of the same flat, depth-stamped journal (the wire format
is unchanged; trees are recovered at the render boundary by
`Hegel.Report.Journal.groupByDepth`):

- **`Timeline`** (default, wired): the structured timeline spine, unchanged
  from the plain renderer; the step carrying the in-band `Failure` splices
  __all__ of its notes into their source declarations. Full context at the
  failure site, no repetition elsewhere.
- **`Aggregate`** (under evaluation, demo-only): compact timeline, then each
  fired declaration's source once, with every step's values stacked under the
  line that drew them, labeled `step N:`.

**Why Timeline won** (two evaluation rounds, `demo-stateful-rich` scenarios;
decisive: the warehouse machine, scenario 7): user `annotate` calls carry the
run's narrative in causal order, which Aggregate re-sorts by code position;
and Aggregate is comprehensive-not-compact — it scales with fired-rules ×
body-size (~84 lines vs Timeline's ~33 on the warehouse). Aggregate's
surviving value is the cross-step reading (value evolution per draw site).
**Reopening trigger**: repeatedly wanting cross-step value evolution when
debugging real failures. If Aggregate is instead deleted, it reconstructs in
~40 lines over the shared core (`toGroups`/`spliceNote` + one label parameter
+ a regrouping).

Note for the trace-rendering work (`notes/roadmap/01-stateful-trace-rendering.md`):
its proposed report composition keeps Timeline's failing-step splice but
demotes it from the whole report to a freeze-frame panel beneath a
braid/slice trace view — the Timeline-vs-Aggregate decision above is about
the splice's *layout*, and is unaffected by that recomposition.

## Contracts and invariants

- **Per-note fallback**: every note that cannot splice (no loc, unreadable
  source) renders as its structured journal line; spliced and structured
  notes mix freely. Degenerate case (pinned): when *nothing* splices, rich
  output equals the plain layout byte-for-byte.
- **Suppression contract, extended**: `statefulDoc` mirrors `failureDoc`'s
  branches — an in-band `Failure` suppresses the top-level
  headline/diff/location block; a `Failure`-less step journal (exception
  mid-loop, e.g. `MalformedTest`) keeps them.
- **`isStepJournal`** keys the rich stateful path:
  `hasInBandFailure || any (depth > 0)` — the disjunction covers both a
  `Failure`-less nested journal and a depth-0 failure from `machine.initial`.
- **`"Step N: "` textual contract**: `Aggregate.stepLabel` parses
  `Stateful.run`'s header text. Deliberately left textual: its structural
  replacement (`StepHeader !Int !Text` note kind) lands iff Aggregate
  survives evaluation, and dies unbuilt otherwise (decided at checkpoint —
  a speculative constructor buys an add→remove round trip; either change is
  compiler-guided with zero rendering-pin churn since display text is
  regenerated identically).
- **`Failure (Maybe Diff)`**: the diff rides in the note kind; there is no
  partial `Note.diff` field.
- **Sibling-scoped draw numbering**: `Draw N:` counts within a step's
  siblings (the same `forAll` keeps its index across firings); flat
  non-stateful journals keep global 1..n.
- **Splicing requires cwd = package root** (`srcLocFile` is
  package-relative) — true for `just test` and `cabal run` from the root.

## Rendering details worth remembering

- Same-file declarations union into one `┏━━ file ━━━` listing with `⋮` at
  the line-number gap (`mergeFileDeclarations`; also applied to the
  non-stateful rich path). Elision marks only *interior* gaps.
- When the failure splices, the failing step's spine header is re-anchored
  with a red `✗` suffix; the splice block ends with a grep-able
  `at file:line` gutter line.
- Diffs render with a colour-keyed legend `(- lhs) (+ rhs)` as their first
  line (`Ann.diffDocs` — hedgehog's header convention); `(===)`'s message is
  `"=== failed, values are not equal"`, symmetric with `(/==)`.

## Verification

- `just test unit` — splice-marker, fallback-mix, nothing-splices, and
  end-to-end (`tests/unit/Stateful.hs`) tests pin the wired behaviour; the
  plain layout's pins are unchanged.
- `cabal run demo-stateful-rich` — seven scenarios × (plain, Timeline,
  Aggregate); the permanent eyeball harness. Scenario 7's runtime is engine
  search (stochastic, 10k–40k cases), not rendering.
