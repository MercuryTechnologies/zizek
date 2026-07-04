# Temporal properties in zizek — first principles

The reasoning behind the arc split (`notes/roadmap/00-arcs.md`): whether
temporal-property testing belongs in `zizek` at all, which fragment, in which
primitive, and where the semantics live. This note is the "why"; `00-arcs.md` is
the resulting structure and `01-arc2-package-layout.md` is the plan.

This exists because the direction was almost taken on *expedience* — "highest-
value near-term work, entirely zizek-side, no engine change" — which is a
cost argument, not a first-principles one. Written down so it isn't re-derived.

## The layers

1. **Property testing** — `∀ input. P(input)`. Counterexample = one input. The
   engine samples and shrinks; `zizek` describes and interprets. This is core.
2. **Stateful / model-based testing** — `∀ trace of operations. model ≈ system`.
   Counterexample = a finite trace. Already reached (libhegel's state-machine
   mode). Note an invariant is already a **safety** property: "in every reachable
   state, φ."
3. **Temporal-property testing** — `∀ trace. Φ(trace)` for a temporal formula.
   The proposed new layer. The question is whether (2)→(3) is a natural
   continuation or a categorically different tool.

## The load-bearing distinction: safety vs. liveness

- **Safety** ("nothing bad happens") is *finitely refutable*: a finite trace
  witnesses the violation — exactly what testing produces and shrinks. Native.
- **Liveness** ("something good eventually happens") is *not* finitely refutable:
  no finite trace disproves `◇P`; you can only ever observe "not yet."

Applied to the three obligation kinds the naive plan proposed:

| kind | class | finitely refutable? | new verdict? |
|---|---|---|---|
| `Standing` + `violated` | safety | yes | no |
| `Within N` | bounded liveness ≡ safety | yes — the trace reaching step N without P *is* the witness | no |
| `Eventually` (unbounded) | true liveness | no | yes (`ProbablyFalse`) |

**The entire "we're bolting a temporal-logic framework onto a property tester"
concern reduces to one constructor: unbounded `Eventually`.** Everything else is
finitely refutable — arguably not "temporal logic" at all, just *assertions with
memory over a trace*, a clean generalization of the invariants stateful testing
already has.

## Where each fragment wants to live (semantic ownership)

- **Safety is zizek-native and sound.** The engine already searches and shrinks
  finite traces; a safety violation is an assertion firing at a point in a trace
  it already explores. And it needs *nothing new from the engine* — it can be
  built entirely on top of `Stateful.run` (see `instrument`-over-`run` in
  `01-arc2-package-layout.md`). `zizek` describes; the engine searches. No
  ownership inversion.
- **Liveness's honest form is engine search.** To *refute* `◇P` you must search
  for a trace that extends without ever satisfying P (QuickLTL-style
  formula-driven trace extension). `zizek` can't search — it can only *observe*
  that P never happened across sampled traces and *guess*, statistically, that it
  never will. That is runtime verification / monitoring (RV-LTL's four-valued
  lattice is a monitor's verdict set), not testing.

**Conclusion:** the safety/liveness line is also the Arc 2 / Arc 3 line. Every
soundness gap a zizek-side liveness heuristic would have — lateness bias, weak
trigger thresholds, adversarial-scheduling foot-guns — is downstream of doing
liveness without engine search. So unbounded `Eventually`, give-up, and a
`ProbablyFalse` verdict are **deferred to Arc 3** (engine search), *not* shipped
as a heuristic. If liveness *feedback* is wanted before then, it must be surfaced
as an explicit non-verdict observation ("open at trace end across N cases"), with
no pass/fail effect — never a verdict it can't justify by sampling.

## Is `incur`/`discharge` even the right primitive (for safety)?

An unforced, rendering-driven choice, worth deciding on merits:

- **(A) Obligations** — `incur`/`discharge` lifecycle + tokens riding model
  state. Ergonomic for response patterns, renders well as obligation-lanes.
- **(B) Trace-aware invariants** — invariants with read access to trace history;
  "once closed, reads return `Left`" as a structural predicate re-checked each
  step. The smallest step from what stateful testing already is. Needs the least.
- **(C) An explicit past-LTL formula language** — most expressive, most
  machinery; a model checker's job. Overkill for v1.

Obligations (A) are the bounded-response pattern `□(incur → ◇≤N discharge)`
reified as objects — a narrow LTL fragment chosen because it *renders* well, not
because it's minimal. Prototype (A) and (B) against the file-handle example and
pick the smaller one that still yields the citation the ledger wants; don't let
the obligation-lanes mockup decide the semantics. (Note the `instrument`-over-
`run` seam in `01-…` naturally realizes *both* — a synthetic deadline invariant
*is* a trace-aware invariant.)

## Where this puts zizek (prior art)

- **Hedgehog / Hypothesis / QuickCheck stateful**: safety only (invariants,
  postconditions); none claim liveness. The mainstream position — safety in
  scope, liveness out.
- **Quickstrom / Bombadil**: full LTL over traces, RV-LTL verdicts — but built
  *around* an evaluating checker. Temporal logic is the center, not an add-on.
- **TLA+ / P / Quint**: model checkers; liveness via fairness + state-space
  search — the real thing, which is search, not sampling.

Adding the **safety** fragment keeps `zizek` in the Hedgehog family (with an
unusually good report). Adding **zizek-side unbounded liveness** would land it in
an awkward middle — liveness-claiming but sampling-only, monitoring dressed as
falsification. Hence: safety → Arc 2 (its own package); liveness → Arc 3 (engine).

## Settled

1. **Safety fragment is a principled, sound extension** — build it (Arc 2,
   separate package, `instrument`-over-`run`, zero engine change).
2. **Do not ship unbounded liveness as a zizek-side heuristic** — defer to Arc 3
   engine search, or (if wanted sooner) a non-verdict observation only.
3. **Decide the primitive (A vs B) on merits during implementation**, not on the
   ledger's rendering appetite.
4. **Reporting is not a temporal feature** — causal blame/ledger/verdict are
   presentation of engine-produced counterexamples and stay in Arc 1 core
   (`00-arcs.md`, principle a).
