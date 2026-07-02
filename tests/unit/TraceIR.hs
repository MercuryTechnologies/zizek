-- | Pure pins for the trace IR ("Hegel.Report.Trace") and blame analysis
-- ("Hegel.Report.Blame"), plus one end-to-end run through the engine.
module TraceIR (spec) where

import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.IntSet qualified as IntSet
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Gen qualified as Gen
import Hegel.Pool (Pool)
import Hegel.Pool qualified as Pool
import Hegel.Property (assert, forAll)
import Hegel.Property.Internal (Env (..), askEnv)
import Hegel.Report
  ( Clock (..),
    Event (..),
    EventKind (..),
    Note (..),
    NoteKind (..),
    Report (..),
    Result (..),
    Var (..),
  )
import Hegel.Report.Blame (Fact (..), Observation (..), Phenomenon (..))
import Hegel.Report.Blame qualified as Blame
import Hegel.Report.Trace (Lifeline (..), Step (..), Touch (..))
import Hegel.Report.Trace qualified as Trace
import Hegel.Runner (check)
import Hegel.Settings (defaultSettings)
import Hegel.Stateful qualified as Stateful
import Test.Hspec

-- ---------------------------------------------------------------------------
-- Fixture helpers

noteAt :: Clock -> Int -> NoteKind -> Text -> Note
noteAt clock depth kind text = Note {kind, text, loc = Nothing, depth, clock}

header :: Clock -> Int -> Text -> Note
header c i l = noteAt c 0 (StepHeader i l) ("Step " <> T.pack (show i) <> ": " <> l)

eventAt :: Clock -> Var -> EventKind -> Event
eventAt clock var kind = Event {clock, var, kind}

h1 :: Var
h1 = Var {pool = 0, id = 7}

-- | The mockup-A shape, synthetically: open(1), write(4), close(5), read(8)
-- — with the read a posthumous touch (the synthetic stream can express what
-- engine pool draws cannot yet).
useAfterConsume :: ([Note], [Event])
useAfterConsume =
  ( [ header (Clock 1) 1 "open",
      header (Clock 4) 4 "write",
      noteAt (Clock 6) 1 Drawn "h1",
      noteAt (Clock 7) 1 Response "ok",
      header (Clock 8) 5 "close",
      noteAt (Clock 10) 1 Drawn "h1",
      header (Clock 11) 8 "read",
      noteAt (Clock 13) 1 Drawn "h1",
      noteAt (Clock 14) 1 (Failure Nothing) "read returned stale bytes"
    ],
    [ eventAt (Clock 2) h1 Born,
      eventAt (Clock 5) h1 Reused,
      eventAt (Clock 9) h1 Consumed,
      eventAt (Clock 12) h1 Reused
    ]
  )

uacTrace :: Trace.Trace
uacTrace = uncurry Trace.build useAfterConsume

-- ---------------------------------------------------------------------------
-- Spec

spec :: Spec
spec = do
  describe "Trace.build" do
    it "carries the schema version" do
      uacTrace.version `shouldBe` 1

    it "splits steps on StepHeader notes" do
      [(s.index, s.rule) | s <- uacTrace.steps]
        `shouldBe` [(1, "open"), (4, "write"), (5, "close"), (8, "read")]

    it "marks the failing step and locates the failure" do
      [s.index | s <- uacTrace.steps, s.failed] `shouldBe` [8]
      fmap (.step) uacTrace.failure `shouldBe` Just 8
      fmap (.message) uacTrace.failure `shouldBe` Just "read returned stale bytes"

    it "lifts a rule's last Response note into Step.response" do
      [(s.index, s.response) | s <- uacTrace.steps]
        `shouldBe` [(1, Nothing), (4, Just "ok"), (5, Nothing), (8, Nothing)]

    it "folds the event stream into a birth-ordered lifeline" do
      case uacTrace.lifelines of
        [l] -> do
          l.var `shouldBe` h1
          l.ordinal `shouldBe` 1
          l.bornAt `shouldBe` Just 1
          l.consumedAt `shouldBe` Just 5
          l.touchedAt `shouldBe` [4]
          l.posthumous `shouldBe` [8]
        ls -> expectationFailure ("expected one lifeline, got: " <> show (length ls))

    it "correlates a draw's event with its Drawn note by clock adjacency" do
      let touchesOf i = concat [s.touches | s <- uacTrace.steps, s.index == i]
      -- Step 4's reuse (clock 5) is adjacent to the Drawn note at clock 6.
      fmap (fmap (.text) . (.note)) (touchesOf 4) `shouldBe` [Just "h1"]
      -- Step 1's Born (a Pool.add, not a draw) correlates with nothing.
      fmap (fmap (.text) . (.note)) (touchesOf 1) `shouldBe` [Nothing]

    it "does not correlate across an intervening note" do
      -- Same shape as step 4, but an annotation lands between the event and
      -- the draw's note: adjacency broken, no correlation.
      let notes =
            [ header (Clock 1) 1 "write",
              noteAt (Clock 3) 1 Annotation "in between",
              noteAt (Clock 4) 1 Drawn "h1"
            ]
          t = Trace.build notes [eventAt (Clock 2) h1 Reused]
      concat [fmap (fmap (.text) . (.note)) s.touches | s <- t.steps] `shouldBe` [Nothing]

    it "lands pre-header events in the prelude step" do
      let notes =
            [ noteAt (Clock 3) 0 Annotation "Initial invariant check.",
              header (Clock 4) 1 "touch"
            ]
          t = Trace.build notes [eventAt (Clock 2) h1 Born]
      [(s.index, s.rule) | s <- t.steps] `shouldBe` [(0, "<initial>"), (1, "touch")]
      fmap (.bornAt) t.lifelines `shouldBe` [Just 0]

    it "is total on a non-stateful (headerless) journal" do
      let t = Trace.build [noteAt (Clock 1) 0 Drawn "42"] []
      [(s.index, s.rule) | s <- t.steps] `shouldBe` [(0, "<initial>")]
      t.lifelines `shouldSatisfy` null

    it "assigns per-pool ordinals in birth order" do
      let va = Var {pool = 0, id = 3}
          vb = Var {pool = 1, id = 3}
          vc = Var {pool = 0, id = 9}
          t =
            Trace.build
              [header (Clock 1) 1 "setup"]
              [ eventAt (Clock 2) va Born,
                eventAt (Clock 3) vb Born,
                eventAt (Clock 4) vc Born
              ]
      [(l.var, l.ordinal) | l <- t.lifelines] `shouldBe` [(va, 1), (vb, 1), (vc, 2)]

  describe "Blame.analyze" do
    it "blames the posthumous touch and cites the full ancestry" do
      case Blame.analyze uacTrace of
        Nothing -> expectationFailure "expected blame for the use-after-consume trace"
        Just b -> do
          b.subject `shouldBe` h1
          b.observed.step `shouldBe` 8
          b.observed.fact `shouldBe` HauntedAt h1
          -- Most recent cause first: death, then the write, then birth.
          [(p.step, p.fact) | p <- b.observed.since]
            `shouldBe` [(5, ConsumedAt h1), (4, TouchedAt h1), (1, BornAt h1)]
          b.diagnosis `shouldBe` Just UseAfterConsume

    it "citation closure is the mockup-A step set" do
      case Blame.analyze uacTrace of
        Nothing -> expectationFailure "expected blame"
        Just b -> Blame.citationClosure b `shouldBe` IntSet.fromList [1, 4, 5, 8]

    it "flattens citations from the failing step" do
      case Blame.analyze uacTrace of
        Nothing -> expectationFailure "expected blame"
        Just b ->
          [(c.from, c.to) | c <- Blame.citations b] `shouldBe` [(8, 5), (8, 4), (8, 1)]

    it "yields Nothing when the failing step touched no pool values" do
      let t =
            Trace.build
              [ header (Clock 1) 1 "boom",
                noteAt (Clock 2) 1 (Failure Nothing) "boom"
              ]
              []
      Blame.analyze t `shouldSatisfy` \case
        Nothing -> True
        Just _ -> False

    it "yields Nothing when nothing failed" do
      let (notes, events) = useAfterConsume
          notFailure n = case n.kind of Failure _ -> False; _ -> True
      Blame.analyze (Trace.build (filter notFailure notes) events) `shouldSatisfy` \case
        Nothing -> True
        Just _ -> False

  describe "end to end (engine)" do
    it "a real pool machine builds a blamed trace" do
      report <- check defaultSettings (Stateful.run eventfulMachine)
      case report.result of
        Counterexample {notes, events} -> do
          let t = Trace.build notes events
          t.failure `shouldSatisfy` isJust
          case Blame.analyze t of
            Nothing -> expectationFailure "expected blame from the eventful machine"
            Just b -> do
              -- Every citation points backwards from the failing step.
              [() | c <- Blame.citations b, c.to >= c.from] `shouldBe` []
              -- The revset contains the failing step.
              case t.failure of
                Just f -> IntSet.member f.step (Blame.citationClosure b) `shouldBe` True
                Nothing -> pure ()
              -- The subject is one of the trace's lifelines.
              [l.var | l <- t.lifelines, l.var == b.subject] `shouldBe` [b.subject]
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

-- ---------------------------------------------------------------------------
-- Engine fixture: the PoolEvents machine plus 'respond'

data Model = Model
  { pool :: Pool Int,
    reused :: Bool,
    consumed :: Bool
  }

-- | Fails once a reusable draw /and/ a consuming draw have both happened, so
-- the minimal counterexample keeps a Born, a Reused, and a Consumed event.
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
            Stateful.respond "consumed ok"
            pure m {consumed = True}
        ],
      invariants =
        [ Stateful.Invariant "never_reuse_and_consume" \m ->
            assert (not (m.reused && m.consumed)) "reuse and consume never both happen (bug)"
        ]
    }
