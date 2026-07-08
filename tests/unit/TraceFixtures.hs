-- | Shared fixtures for the trace-rendering suites ("PoolEvents",
-- "TraceModel", "LogRendering"): the synthetic-stream helpers, the
-- transfer/handoff fixture (a composed-report shape), and the eventful engine
-- machine.
module TraceFixtures
  ( -- * Synthetic-stream helpers
    noteAt,
    header,
    eventAt,
    h1,

    -- * The transfer/handoff fixture (composed-report shape, with elision fillers)
    handoffFixture,
    handoffTrace,
    handoffBlame,

    -- * A flat born+touch fixture (no death or handoff)
    flatFixture,

    -- * A pool-free journal (unfocused, no blame)
    noPoolTrace,

    -- * A two-root ledger (unfocused, blame present)
    ledgerFixture,
    ledgerTrace,
    ledgerBlame,

    -- * The eventful engine machine
    Model (..),
    eventfulMachine,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Gen qualified as Gen
import Hegel.Pool (Pool)
import Hegel.Pool qualified as Pool
import Hegel.Property (assert, forAll)
import Hegel.Report (Event (..), Note (..), NoteKind (..), Operation (..), Tick (..), Var (..))
import Hegel.Report.Trace (Trace)
import Hegel.Report.Trace qualified as Trace
import Hegel.Report.Trace.Blame (Blame)
import Hegel.Report.Trace.Blame qualified as Blame
import Hegel.Stateful qualified as Stateful

-- * Synthetic-stream helpers

noteAt :: Tick -> Int -> NoteKind -> Text -> Note
noteAt clock depth kind text = Note {kind, text, loc = Nothing, depth, clock}

header :: Tick -> Int -> Text -> Note
header c i l = noteAt c 0 (StepHeader i l) ("Step " <> T.pack (show i) <> ": " <> l)

eventAt :: Tick -> Var -> Operation -> Event
eventAt clock var kind = Event {clock, var, kind}

h1 :: Var
h1 = Var {pool = 0, id = 7}

-- | The transfer destination: a second pool's value that continues 'h1' via a
-- declared lineage link ('Hegel.Pool.transfer').
h2 :: Var
h2 = Var {pool = 1, id = 9}

-- * The transfer/handoff fixture

-- | open(1), fillers(2,3), write(4), close(5), fillers(6,7), read(8). @close@
-- is a 'Hegel.Pool.transfer': it consumes 'h1' and births 'h2' with 'h1' as
-- its declared lineage, so the value lives on across pools. The failing @read@
-- touches 'h2', so blame cites the handoff (@close@), the earlier @write@, and
-- the birth (@open@); the fillers make elision rows render. This is the
-- reachable composed-report shape — every event mirrors what real engine pool
-- draws produce.
handoffFixture :: ([Note], [Event])
handoffFixture =
  ( [ header (Tick 1) 1 "open",
      header (Tick 3) 2 "noop",
      header (Tick 4) 3 "noop",
      header (Tick 5) 4 "write",
      noteAt (Tick 7) 1 (Drawn [h1]) "h",
      noteAt (Tick 8) 1 Response "ok",
      header (Tick 9) 5 "close",
      -- A transfer's consuming draw is tagged with the source var (h1), the
      -- value it resolved before the handoff births h2.
      noteAt (Tick 12) 1 (Drawn [h1]) "h",
      header (Tick 13) 6 "noop",
      header (Tick 14) 7 "noop",
      header (Tick 15) 8 "read",
      noteAt (Tick 17) 1 (Drawn [h2]) "h",
      noteAt (Tick 18) 1 (Failure Nothing) "read returned stale bytes"
    ],
    [ eventAt (Tick 2) h1 (Born Nothing),
      eventAt (Tick 6) h1 Reused,
      eventAt (Tick 10) h1 Consumed,
      eventAt (Tick 11) h2 (Born (Just h1)),
      eventAt (Tick 16) h2 Reused
    ]
  )

handoffTrace :: Trace
handoffTrace = uncurry Trace.build handoffFixture

handoffBlame :: Blame
handoffBlame = fromJust (Blame.analyze handoffTrace)

-- * A flat born+touch fixture

-- | open(1) births the value, use(2) reuses it, use(3) reuses it and fails an
-- assertion. The value is never consumed or transferred, so its story is flat
-- (born, then touched): a single lineage root, so the report renders as the
-- focused event log (birth and access rows, cites) with no lifecycle glyphs
-- beyond born\/access.
flatFixture :: ([Note], [Event])
flatFixture =
  ( [ header (Tick 1) 1 "open",
      header (Tick 3) 2 "use",
      noteAt (Tick 5) 1 Response "ok",
      header (Tick 6) 3 "use",
      noteAt (Tick 8) 1 (Failure Nothing) "expected fresh handle"
    ],
    [ eventAt (Tick 2) h1 (Born Nothing),
      eventAt (Tick 4) h1 Reused,
      eventAt (Tick 7) h1 Reused
    ]
  )

-- * A pool-free journal

-- | A stateful failure with no pool values at all: three @push@/@pop@ steps
-- with free (non-pool) draws and an annotation, the last failing. 'Blame.analyze'
-- returns 'Nothing' (no pool events), so this is the @Unfocused Nothing@ shape —
-- every step shown, blank gutters, the failing step marked @✗@, no margins.
noPoolFixture :: ([Note], [Event])
noPoolFixture =
  ( [ header (Tick 1) 1 "push",
      noteAt (Tick 2) 1 (Drawn []) "0",
      header (Tick 3) 2 "push",
      noteAt (Tick 4) 1 (Drawn []) "1",
      noteAt (Tick 5) 1 Annotation "sum is now 1",
      header (Tick 6) 3 "pop",
      noteAt (Tick 7) 1 (Failure Nothing) "stack underflow"
    ],
    []
  )

noPoolTrace :: Trace
noPoolTrace = uncurry Trace.build noPoolFixture

-- * A two-root ledger

a1 :: Var
a1 = Var {pool = 0, id = 1}

a2 :: Var
a2 = Var {pool = 0, id = 2}

-- | Two independent accounts (distinct lineage roots, no transfer): each opened
-- and touched, the final @audit@ step touching both and failing. 'Blame.analyze'
-- returns 'Just' (there are pool events) but the failing step spans two roots,
-- so this is the @Unfocused (Just blame)@ shape — every step shown with per-step
-- lifecycle glyphs and kept margins.
ledgerFixture :: ([Note], [Event])
ledgerFixture =
  ( [ header (Tick 1) 1 "open",
      noteAt (Tick 3) 1 (Drawn [a1]) "acct",
      header (Tick 4) 2 "open",
      noteAt (Tick 6) 1 (Drawn [a2]) "acct",
      header (Tick 7) 3 "deposit",
      noteAt (Tick 9) 1 (Drawn [a1]) "acct",
      noteAt (Tick 10) 1 (Drawn []) "5",
      noteAt (Tick 11) 1 Annotation "balance a₁ = 5",
      header (Tick 12) 4 "audit",
      noteAt (Tick 15) 1 (Drawn [a1]) "acct",
      noteAt (Tick 16) 1 (Drawn [a2]) "acct",
      noteAt (Tick 17) 1 (Failure Nothing) "balances disagree"
    ],
    [ eventAt (Tick 2) a1 (Born Nothing),
      eventAt (Tick 5) a2 (Born Nothing),
      eventAt (Tick 8) a1 Reused,
      eventAt (Tick 13) a1 Reused,
      eventAt (Tick 14) a2 Reused
    ]
  )

ledgerTrace :: Trace
ledgerTrace = uncurry Trace.build ledgerFixture

ledgerBlame :: Blame
ledgerBlame = fromJust (Blame.analyze ledgerTrace)

-- * The eventful engine machine

-- | A machine whose failure requires all three pool-event kinds to have
-- occurred: the invariant trips only once a reusable draw /and/ a consuming
-- draw have both happened, so the minimal counterexample must keep at least
-- one 'Born', one 'Reused', and one 'Consumed' event.
data Model = Model
  { pool :: Pool Int,
    reused :: Bool,
    consumed :: Bool
  }

eventfulMachine :: Stateful.Machine Model IO
eventfulMachine =
  Stateful.Machine
    { initial = do
        p <- Pool.new
        pure Model {pool = p, reused = False, consumed = False},
      rules =
        [ Stateful.Rule "register" \m -> do
            n <- forAll (Gen.int & Gen.min 0 & Gen.max 100 & Gen.build)
            liftIO (Pool.add m.pool n)
            pure m,
          Stateful.Rule "reuse" \m -> do
            _ <- forAll (Pool.reuse m.pool)
            pure m {reused = True},
          Stateful.Rule "consume" \m -> do
            _ <- forAll (Pool.consume m.pool)
            Stateful.respond "consumed ok"
            pure m {consumed = True}
        ],
      invariants =
        [ Stateful.Invariant "never_reuse_and_consume" \m ->
            assert (not (m.reused && m.consumed)) "reuse and consume never both happen (bug)"
        ]
    }
