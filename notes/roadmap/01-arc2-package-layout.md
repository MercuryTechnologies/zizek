# Arc 2 package layout — next up

The first Arc 2 work is **structural**: stand up the separate package that the
Arc 2 testing surface will live in, and pin the mechanism by which it extends
`zizek` without modifying it. The obligation surface itself (bounded deadlines,
standing promises, `violated`) is the first tenant; this note is about the
*container* and the *seam*, so the tenant can be built and iterated without ever
touching core.

Prerequisite reading: `00-arcs.md` (why Arc 2 is a separate package) and
`notes/design/temporal-properties.md` (why the scope is the safety fragment and
what's deferred to Arc 3).

## Goal

A second in-repo library — working name **`zizek-obligations`** — that:

- depends on `zizek` (direction is strictly upper → core; no cycle);
- provides the safety obligation surface (`incur` / `discharge` / `violated`,
  `Due = Within | Standing`, `runWithObligations`);
- requires **zero changes to `zizek`** for the safety fragment;
- is free to use `zizek`'s exposed `*.Internal` modules while its own API
  stabilizes (they exist precisely "to reach past the public API"), but for v1
  needs only the **public** `Hegel.Stateful` + `Hegel.Assertion` surface.

## Package structure

Single repo, two packages (a `cabal.project` lists both):

```
zizek                 -- core: FFI, Gen, Property, Stateful, Pool, Report.*  (Arc 1)
zizek-obligations     -- Arc 2 testing surface; build-depends: zizek
```

The second target is a **discrete `zizek-obligations.cabal` package** in the
same repo. This is a deliberate choice over an in-`zizek.cabal` sub-library
stanza: the split is a real release/versioning boundary, so it gets a real
package. The cost — two version streams, two build plans — is accepted as the
honest price of that boundary.

(Rejected: a second `library zizek-obligations` stanza inside `zizek.cabal`, the
way `conformance-utils` is a private sub-library. Cheaper to stand up and one
build plan, but it conflates an internal build split with a package boundary —
and sub-libraries are not the mechanism we want for package separation. Not
used.)

Dependency direction is the load-bearing invariant: `zizek-obligations` →
`zizek`, never the reverse. This is *why* reporting must stay in core (00-arcs
principle a): the default render path in `Hspec`/`Tasty` can't reach up into an
Arc 2 package.

## The seam: `instrument`-over-`run`

The obligation surface is built **on top of** `Stateful.run`, not by forking it.
`run` already exercises the only two hooks the safety fragment needs — it applies
rules, and it checks invariants after every successful step. `instrument`
transforms the `Machine` so obligation behaviour rides those two mechanisms:

```
instrument userMachine = userMachine
  { rules      = map (wrapApply carryLedgerAndBumpStep) userMachine.rules
  , invariants = deadlineInvariant : map liftInv userMachine.invariants
  }

runWithObligations m = Stateful.run (instrument m)   -- run is untouched
```

- **Rule wrap** — each `Rule.apply` is wrapped so the obligation ledger and a
  successful-step counter travel with the model state `s`; a completed rule is a
  successful step, so the counter bumps there. Because both ride `s`, a mid-rule
  `discard`/`Stop` rolls them back *with* the state → the roadmap's phantom-
  obligation hazard (former note's B1) cannot arise.
- **Synthetic deadline invariant** — appended to the machine's invariants, so
  `run` checks it after every successful step, wrapped in the existing in-band-
  failure journaling. It reads the ledger + counter; a `Within N` obligation with
  `incurredAt + N ≤ counter` still open → it throws → an ordinary
  `Counterexample`, journaled at that step. Deadlines never reached before trace
  end never trip it → weak-finite-trace semantics fall out for free.

Properties this buys, all by construction: **no fork** of the alignment-critical
loop; **draws nothing**, so choice-sequence alignment is untouched (live run,
shrink replay, reconstruction replay all agree); discard-rollback and weak-trace
semantics are inherited, not re-implemented.

## Open decision: where `incur`/`discharge` reach the ledger

Because the ledger stays out of `Env` (to keep core obligation-free — see the
rejected alternative below), `incur`/`discharge` must touch the ledger *through
the state* rather than an ambient tracker. Three pure-library shapes; pick during
implementation:

- **Explicit threading** — `incur :: Text -> Due -> PropertyT m Token` returns a
  token the user stores (`s { promised = Map.insert … }`), `discharge` operates on
  stored tokens. Most transparent, most boilerplate. (This is the shape the old
  `01-obligation-api.md` worked example used.)
- **Thin `StateT`-flavoured rule monad** over `PropertyT`, so `incur`/`discharge`
  read as effects but edit the ledger slice of state. Best ergonomics; the
  package defines its own rule shape adapted into `zizek`'s `Rule`.
- **`IORef` tracker + snapshot/restore** around each rule (via the exposed
  `Internal.Control.catchControl`) to recover discard-rollback. Cleanest
  `incur`/`discharge`, but reaches into `*.Internal` and adds snapshot machinery.

Lean: start with explicit threading (zero magic, proves the seam), offer the
`StateT` layer as sugar once the shape is known.

**Rejected: an `Env.obligations` field + `localEnv`** (the old design). It would
require a `zizek` change *and* bake an obligation-shaped hole into core `Env` —
Arc 2 concepts leaking into Arc 1. The state-threaded ledger avoids both.

## Scope

**In:** the safety fragment — `Within N` (bounded deadline; a definite failure at
the deadline step), `Standing` + `violated` (broken only by an explicit call),
and their journal rendering as plain `Annotation` notes (no new `NoteKind`
needed for v1).

**Out (Arc 3 / core changes, explicitly not in this package):**

- Unbounded `Eventually` / give-up / `Result.ProbablyFalse` — needs a new verdict
  constructor on `zizek`'s `Result` (can't be added from an upper package) and
  live-run tallying inside `Runner.check`. Deferred to Arc 3 engine search, or a
  future decision to let the package own its own runner + verdict type.
- Structured obligation `NoteKind`s (`ObligationIncurred`/…) for the eventual
  braid — needs `Report.Note` to grow constructors. Presentation polish;
  deferrable; belongs in core if/when it lands (it's rendering vocabulary, not
  semantics).

## The `zizek` surface this relies on (the stable-ish contract)

For v1 (public only): `Hegel.Stateful` (`Machine (..)`, `Rule (..)`,
`Invariant (..)`, `run`, `respond`), `Hegel.Assertion` (`AssertionFailure (..)`
with its `callStack` field, for `violated` to carry the incur site),
`Hegel.Property` combinators, `Hegel.Gen`.

If the ergonomic shapes want more: `Hegel.Property.Internal`
(`PropertyT (..)`, `Env (..)`, `note`, `nested`, `askEnv`), `Internal.Control`
(`catchControl`), `Internal.Session.DataSource` — all already exposed as unstable
internals. Prefer to stay on the public surface; every internal used is a
coupling to record here.

## Phasing

1. **Stand up the package** — the `zizek-obligations.cabal` package (listed in
   `cabal.project`), an empty module, `build-depends: zizek`, a smoke test.
   Proves the two-package build.
2. **`instrument`-over-`run` + `Within`/`Standing`/`violated`** over the public
   `Stateful` API; explicit-threading tokens. Test: a bounded-deadline machine →
   `Counterexample` at the deadline step; replay alignment (no `Aborted`/
   diverge); `violated` end-to-end (the file-handle machine); discard-then-incur
   → no phantom.
3. **Ergonomic sugar** — the `StateT` rule layer if threading proves noisy;
   obligation-span rendering (a detail-region setting of the shipped ledger —
   see `notes/decisions/report-visual-grammar.md`), no longer gated on the braid.

## Verification

- Prototyping `instrument` first (before the package split) to confirm the
  synthetic-invariant approach reproduces a bounded-deadline counterexample
  end-to-end is a cheap way to de-risk the whole plan — do that before committing
  to the package scaffolding.
- The safety fragment adds no draws, so the regression guard is: a machine mixing
  value-drawing rules with `incur`/`discharge` reproduces its counterexample on
  replay (never `ReplayDiverged`).
