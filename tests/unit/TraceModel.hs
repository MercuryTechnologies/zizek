-- | Pure pins for the trace model ("Hegel.Report.Trace"), plus one
-- end-to-end run through the engine.
module TraceModel (spec) where

import Data.Maybe (isJust)
import Hegel.Report
  ( Event (..),
    Note (..),
    NoteKind (..),
    Operation (..),
    Report (..),
    Result (..),
    Tick (..),
    Var (..),
  )
import Hegel.Report.Trace (Identity (..), Step (..))
import Hegel.Report.Trace qualified as Trace
import Hegel.Runner (check)
import Hegel.Settings (defaultSettings)
import Hegel.Stateful qualified as Stateful
import Test.Hspec
import TraceFixtures (eventAt, eventfulMachine, h1, header, noteAt)

-- ---------------------------------------------------------------------------
-- Fixture helpers

-- | A single pool value drawn reusably across several steps, failing on the
-- last: open(1) births it, write(4)/peek(5) reuse it, read(8) reuses it and
-- fails. Every event is what real engine pool draws produce (a reusable draw
-- never consumes), so the stream is reachable.
reusedValue :: ([Note], [Event])
reusedValue =
  ( [ header (Tick 1) 1 "open",
      header (Tick 4) 4 "write",
      noteAt (Tick 6) 1 (Drawn []) "h1",
      noteAt (Tick 7) 1 Response "ok",
      header (Tick 8) 5 "peek",
      noteAt (Tick 10) 1 (Drawn []) "h1",
      header (Tick 11) 8 "read",
      noteAt (Tick 13) 1 (Drawn []) "h1",
      noteAt (Tick 14) 1 (Failure Nothing) "read returned stale bytes"
    ],
    [ eventAt (Tick 2) h1 (Born Nothing),
      eventAt (Tick 5) h1 Reused,
      eventAt (Tick 9) h1 Reused,
      eventAt (Tick 12) h1 Reused
    ]
  )

reusedTrace :: Trace.Trace
reusedTrace = uncurry Trace.build reusedValue

-- ---------------------------------------------------------------------------
-- Spec

spec :: Spec
spec = do
  describe "Trace.build" do
    it "splits steps on StepHeader notes" do
      [(s.index, s.rule) | s <- reusedTrace.steps]
        `shouldBe` [(1, "open"), (4, "write"), (5, "peek"), (8, "read")]

    it "marks the failing step and locates the failure" do
      [s.index | s <- reusedTrace.steps, s.failed] `shouldBe` [8]
      fmap (.step) reusedTrace.failure `shouldBe` Just 8
      fmap (.message) reusedTrace.failure `shouldBe` Just "read returned stale bytes"

    it "lifts a rule's last Response note into Step.response" do
      [(s.index, s.response) | s <- reusedTrace.steps]
        `shouldBe` [(1, Nothing), (4, Just "ok"), (5, Nothing), (8, Nothing)]

    it "folds the event stream into a birth-ordered identity" do
      case reusedTrace.identities of
        [i] -> do
          i.var `shouldBe` h1
          i.ordinal `shouldBe` 1
        is -> expectationFailure ("expected one identity, got: " <> show (length is))

    it "lands pre-header events in the prelude step" do
      let notes =
            [ noteAt (Tick 3) 0 Annotation "Initial invariant check.",
              header (Tick 4) 1 "touch"
            ]
          t = Trace.build notes [eventAt (Tick 2) h1 (Born Nothing)]
      [(s.index, s.rule) | s <- t.steps] `shouldBe` [(0, "<initial>"), (1, "touch")]

    it "is total on a non-stateful (headerless) journal" do
      let t = Trace.build [noteAt (Tick 1) 0 (Drawn []) "42"] []
      [(s.index, s.rule) | s <- t.steps] `shouldBe` [(0, "<initial>")]
      t.identities `shouldSatisfy` null

    it "a lineage cycle terminates root (malformed stream)" do
      let a = Var {pool = 0, id = 1}
          b = Var {pool = 0, id = 2}
          t =
            Trace.build
              [header (Tick 1) 1 "loop"]
              [ eventAt (Tick 2) a (Born (Just b)),
                eventAt (Tick 3) b (Born (Just a))
              ]
      -- Totality is the assertion: this must return, whatever it returns.
      Trace.root t a `shouldSatisfy` \v -> v == a || v == b

    it "assigns per-pool ordinals in birth order" do
      let va = Var {pool = 0, id = 3}
          vb = Var {pool = 1, id = 3}
          vc = Var {pool = 0, id = 9}
          t =
            Trace.build
              [header (Tick 1) 1 "setup"]
              [ eventAt (Tick 2) va (Born Nothing),
                eventAt (Tick 3) vb (Born Nothing),
                eventAt (Tick 4) vc (Born Nothing)
              ]
      [(i.var, i.ordinal) | i <- t.identities] `shouldBe` [(va, 1), (vb, 1), (vc, 2)]

    it "resolves a transfer chain's root to the origin var" do
      -- open(1) births x in pool 0; write(2) touches x; close(3) consumes x
      -- and births y in pool 1 with lineage x; read(4) touches y.
      let x = Var {pool = 0, id = 1}
          y = Var {pool = 1, id = 1}
          t =
            Trace.build
              [ header (Tick 1) 1 "open",
                header (Tick 3) 2 "write",
                header (Tick 5) 3 "close",
                header (Tick 8) 4 "read"
              ]
              [ eventAt (Tick 2) x (Born Nothing),
                eventAt (Tick 4) x Reused,
                eventAt (Tick 6) x Consumed,
                eventAt (Tick 7) y (Born (Just x)),
                eventAt (Tick 9) y Reused
              ]
      Trace.root t y `shouldBe` x

    it "lifts pool labels onto identities" do
      let x = Var {pool = 0, id = 1}
          t =
            Trace.build
              [header (Tick 2) 1 "open"]
              [ eventAt (Tick 1) x (Named "h"),
                eventAt (Tick 3) x (Born Nothing)
              ]
      fmap (.label) t.identities `shouldBe` [Just "h"]
      -- A label event is vocabulary, not a touch.
      concatMap (.touches) t.steps `shouldSatisfy` ((== 1) . length)

  describe "Trace.build (draw provenance)" do
    -- A pool draw's 'Drawn' note is tagged with the 'Var'(s) it resolved. That
    -- tag partitions a step's draws: a single-var tag matching a touch is a pool
    -- draw (already represented by that 'Touch', so kept out of 'freeDraws');
    -- anything else — a plain non-pool payload, or a composite multi-pool draw —
    -- is a free draw, surfaced as a detail line. These pin that partition.
    let h2 = Var {pool = 1, id = 9}
        kindsOf t = [tch.kind | s <- t.steps, tch <- s.touches]
        freeDrawsOf t = concatMap (.freeDraws) t.steps

    it "keeps a pool draw out of the free draws (its touch represents it)" do
      let t =
            Trace.build
              [ header (Tick 1) 1 "read",
                noteAt (Tick 3) 1 (Drawn [h1]) "42",
                noteAt (Tick 4) 1 (Failure Nothing) "boom"
              ]
              [eventAt (Tick 2) h1 Reused]
      kindsOf t `shouldBe` [Reused]
      freeDrawsOf t `shouldBe` []

    it "keeps a plain (non-pool) draw as a free draw" do
      let t =
            Trace.build
              [ header (Tick 1) 1 "write",
                noteAt (Tick 3) 1 (Drawn [h1]) "handle",
                noteAt (Tick 4) 1 (Drawn []) "payload",
                noteAt (Tick 5) 1 (Failure Nothing) "boom"
              ]
              [eventAt (Tick 2) h1 Reused]
      -- The pool draw ("handle") is bound to the touch; only the payload is free.
      freeDrawsOf t `shouldBe` ["payload"]

    it "partitions by provenance across an intervening annotation (no clock drift)" do
      -- The retired clock-adjacency scheme misattributed when an @annotate@
      -- landed between a draw's event and its 'Drawn' note. The tag binds
      -- regardless of what sits between them.
      let t =
            Trace.build
              [ header (Tick 1) 1 "read",
                noteAt (Tick 3) 1 Annotation "about to read",
                noteAt (Tick 4) 1 (Drawn [h1]) "42",
                noteAt (Tick 5) 1 (Failure Nothing) "boom"
              ]
              [eventAt (Tick 2) h1 Reused]
      freeDrawsOf t `shouldBe` []

    it "binds a transfer's consuming draw to its source var; the birth draws nothing" do
      let t =
            Trace.build
              [ header (Tick 1) 1 "close",
                noteAt (Tick 4) 1 (Drawn [h1]) "42",
                noteAt (Tick 5) 1 (Failure Nothing) "boom"
              ]
              [ eventAt (Tick 2) h1 Consumed,
                eventAt (Tick 3) h2 (Born (Just h1))
              ]
      kindsOf t `shouldBe` [Consumed, Born (Just h1)]
      freeDrawsOf t `shouldBe` []

    it "leaves a composite multi-pool draw unbound (free), never misattributed" do
      -- One @forAll@ drawing from two pools tags its note with both vars; too
      -- ambiguous to attribute to either, so it stays a free draw.
      let t =
            Trace.build
              [ header (Tick 1) 1 "copy",
                noteAt (Tick 4) 1 (Drawn [h1, h2]) "(1,2)",
                noteAt (Tick 5) 1 (Failure Nothing) "boom"
              ]
              [ eventAt (Tick 2) h1 Reused,
                eventAt (Tick 3) h2 Reused
              ]
      kindsOf t `shouldBe` [Reused, Reused]
      freeDrawsOf t `shouldBe` ["(1,2)"]

  describe "end to end (engine)" do
    it "a real pool machine builds a trace with a failure and identities" do
      report <- check defaultSettings (Stateful.run eventfulMachine)
      case report.result of
        Counterexample {notes, events} -> do
          let t = Trace.build notes events
          t.failure `shouldSatisfy` isJust
          t.identities `shouldSatisfy` (not . null)
        other -> expectationFailure ("expected Counterexample, got: " <> show other)

    it "respond reaches Step.response through a real run" do
      report <- check defaultSettings (Stateful.run eventfulMachine)
      case report.result of
        Counterexample {notes, events} -> do
          let t = Trace.build notes events
          -- Every fired consume step declared its response.
          [s.response | s <- t.steps, s.rule == "consume"] `shouldSatisfy` all (== Just "consumed ok")
          [s | s <- t.steps, s.rule == "consume"] `shouldSatisfy` (not . null)
        other -> expectationFailure ("expected Counterexample, got: " <> show other)
