-- | Shared fixtures for the trace-rendering suites ("PoolEvents",
-- "TraceIR", "LedgerRendering"): the synthetic-stream helpers, the
-- use-after-consume fixture (the mockup-A shape), and the eventful engine
-- machine.
module TraceFixtures
  ( -- * Synthetic-stream helpers
    noteAt,
    header,
    eventAt,
    h1,

    -- * The use-after-consume fixture (mockup-A, with elision fillers)
    uacFixture,
    uacTrace,
    uacBlame,

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
import Hegel.Property.Internal (Env (..), askEnv)
import Hegel.Report (Clock (..), Event (..), EventKind (..), Note (..), NoteKind (..), Var (..))
import Hegel.Report.Blame (Blame)
import Hegel.Report.Blame qualified as Blame
import Hegel.Report.Trace (Trace)
import Hegel.Report.Trace qualified as Trace
import Hegel.Stateful qualified as Stateful

-- * Synthetic-stream helpers

noteAt :: Clock -> Int -> NoteKind -> Text -> Note
noteAt clock depth kind text = Note {kind, text, loc = Nothing, depth, clock}

header :: Clock -> Int -> Text -> Note
header c i l = noteAt c 0 (StepHeader i l) ("Step " <> T.pack (show i) <> ": " <> l)

eventAt :: Clock -> Var -> EventKind -> Event
eventAt clock var kind = Event {clock, var, kind}

h1 :: Var
h1 = Var {pool = 0, id = 7}

-- * The use-after-consume fixture

-- | open(1), fillers(2,3), write(4), close(5), fillers(6,7), read(8) — the
-- read is a haunted touch, so blame cites close, write, and open; the
-- fillers make elision rows render.
uacFixture :: ([Note], [Event])
uacFixture =
  ( [ header (Clock 1) 1 "open",
      header (Clock 3) 2 "noop",
      header (Clock 4) 3 "noop",
      header (Clock 5) 4 "write",
      noteAt (Clock 7) 1 Drawn "h",
      noteAt (Clock 8) 1 Response "ok",
      header (Clock 9) 5 "close",
      noteAt (Clock 11) 1 Drawn "h",
      header (Clock 12) 6 "noop",
      header (Clock 13) 7 "noop",
      header (Clock 14) 8 "read",
      noteAt (Clock 16) 1 Drawn "h",
      noteAt (Clock 17) 1 (Failure Nothing) "read returned stale bytes"
    ],
    [ eventAt (Clock 2) h1 (Born Nothing),
      eventAt (Clock 6) h1 Reused,
      eventAt (Clock 10) h1 Consumed,
      eventAt (Clock 15) h1 Reused
    ]
  )

uacTrace :: Trace
uacTrace = uncurry Trace.build uacFixture

uacBlame :: Blame
uacBlame = fromJust (Blame.analyze uacTrace)

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
        env <- askEnv
        p <- liftIO (Pool.new env.testCase)
        pure Model {pool = p, reused = False, consumed = False},
      rules =
        [ Stateful.Rule "register" \m -> do
            n <- forAll (Gen.int & Gen.min 0 & Gen.max 100 & Gen.build)
            liftIO (Pool.add m.pool n)
            pure m,
          Stateful.Rule "reuse" \m -> do
            _ <- forAll (Pool.valuesReusable m.pool)
            pure m {reused = True},
          Stateful.Rule "consume" \m -> do
            _ <- forAll (Pool.valuesConsumed m.pool)
            pure m {consumed = True}
        ],
      invariants =
        [ Stateful.Invariant "never_reuse_and_consume" \m ->
            assert (not (m.reused && m.consumed)) "reuse and consume never both happen (bug)"
        ]
    }
