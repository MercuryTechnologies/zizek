-- | Pure pins for the citation ledger ("Hegel.Report.Ledger") and the glyph
-- tables ("Hegel.Report.Glyph"), plus one engine run through the full path.
module LedgerRendering (spec) where

import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.IntSet qualified as IntSet
import Data.List (nub, sortOn)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Gen qualified as Gen
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
import Hegel.Report.Ann (docToText)
import Hegel.Report.Blame (Blame)
import Hegel.Report.Blame qualified as Blame
import Hegel.Report.Glyph (Cell (..), GlyphTable (..))
import Hegel.Report.Glyph qualified as Glyph
import Hegel.Report.Ledger qualified as Ledger
import Hegel.Report.Phrase qualified as Phrase
import Hegel.Report.Trace (Trace)
import Hegel.Report.Trace qualified as Trace
import Hegel.Report.Verdict qualified as Verdict
import Hegel.Runner (check)
import Hegel.Settings (defaultSettings)
import Hegel.Stateful qualified as Stateful
import Test.Hspec

-- ---------------------------------------------------------------------------
-- Fixture: the mockup-A shape (with filler steps so elision rows render)

noteAt :: Clock -> Int -> NoteKind -> Text -> Note
noteAt clock depth kind text = Note {kind, text, loc = Nothing, depth, clock}

header :: Clock -> Int -> Text -> Note
header c i l = noteAt c 0 (StepHeader i l) ("Step " <> T.pack (show i) <> ": " <> l)

eventAt :: Clock -> Var -> EventKind -> Event
eventAt clock var kind = Event {clock, var, kind}

h1 :: Var
h1 = Var {pool = 0, id = 7}

-- | open(1), fillers(2,3), write(4), close(5), fillers(6,7), read(8) — the
-- read is a haunted touch, so blame cites close, write, and open.
uacTrace :: Trace
uacTrace = uncurry Trace.build uacFixture

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

uacBlame :: Blame
uacBlame = fromJust (Blame.analyze uacTrace)

-- | The fixture's raw streams with one extra note spliced in before the
-- failure note (clock order is what matters; the list stays sorted).
fixtureWith :: Note -> ([Note], [Event])
fixtureWith extra = (sortOn (.clock) (extra : notes), events)
  where
    (notes, events) = uacFixture

renderWith :: Ledger.Options -> Text
renderWith opts = docToText (Ledger.ledgerDoc opts uacTrace uacBlame)

-- ---------------------------------------------------------------------------
-- Spec

spec :: Spec
spec = do
  describe "ledgerDoc" do
    it "renders the failure-first unicode ledger (mockup-A shape)" do
      renderWith (Ledger.defaultOptions Glyph.unicode)
        `shouldBe` T.intercalate
          "\n"
          [ "✗ 8  read v₁                    ●─┬─┬─╮",
            "│    read returned stale bytes    │ │ │",
            "┆    ⋯ 2 steps, none touch v₁     ┆ ┆ ┆",
            "◌ 5  close v₁                   ◀─╯ │ │   v₁ was consumed",
            "○ 4  write v₁ → ok              ◀───╯ │   v₁ was touched",
            "┆    ⋯ 2 steps, none touch v₁         ┆",
            "● 1  open v₁                    ◀─────╯   v₁ was created"
          ]

    it "renders the chronological unicode ledger" do
      renderWith (Ledger.defaultOptions Glyph.unicode) {Ledger.direction = Ledger.Chronological}
        `shouldBe` T.intercalate
          "\n"
          [ "● 1  open v₁                    ◀─────╮   v₁ was created",
            "┆    ⋯ 2 steps, none touch v₁         ┆",
            "○ 4  write v₁ → ok              ◀───╮ │   v₁ was touched",
            "◌ 5  close v₁                   ◀─╮ │ │   v₁ was consumed",
            "┆    ⋯ 2 steps, none touch v₁     ┆ ┆ ┆",
            "✗ 8  read v₁                    ●─┴─┴─╯",
            "│    read returned stale bytes    │ │ │"
          ]

    it "renders the failure-first ascii ledger" do
      renderWith (Ledger.defaultOptions Glyph.ascii)
        `shouldBe` T.intercalate
          "\n"
          [ "x 8  read v1                     *-+-+-.",
            "|    read returned stale bytes     | | |",
            ":    ... 2 steps, none touch v1    : : :",
            "% 5  close v1                    <-' | |   v1 was consumed",
            "o 4  write v1 -> ok              <---' |   v1 was touched",
            ":    ... 2 steps, none touch v1        :",
            "* 1  open v1                     <-----'   v1 was created"
          ]

    it "falls back to numeric citations past the rail budget" do
      let out = renderWith (Ledger.defaultOptions Glyph.unicode) {Ledger.railBudget = 2}
      out `shouldSatisfy` T.isInfixOf "← cites 5, 4, 1"
      out `shouldNotSatisfy` T.isInfixOf "◀"

    it "clips the call column at the width budget" do
      let out = renderWith (Ledger.defaultOptions Glyph.unicode) {Ledger.callWidth = 8}
      out `shouldSatisfy` T.isInfixOf "write v⋯"

  describe "layoutRows" do
    it "puts the failing row first (failure-first) with its details beneath in both directions" do
      let rows d =
            Ledger.layoutRows
              (Ledger.defaultOptions Glyph.unicode) {Ledger.direction = d}
              uacTrace
              uacBlame
          kinds d = fmap (.kind) (rows d)
      take 2 (kinds Ledger.FailureFirst)
        `shouldBe` [Ledger.NodeRow, Ledger.DetailRow]
      drop (length (kinds Ledger.Chronological) - 2) (kinds Ledger.Chronological)
        `shouldBe` [Ledger.NodeRow, Ledger.DetailRow]

    it "elides unshown steps explicitly, with counts" do
      let rows = Ledger.layoutRows (Ledger.defaultOptions Glyph.unicode) uacTrace uacBlame
      [r.call | r <- rows, r.kind == Ledger.ElisionRow]
        `shouldBe` ["⋯ 2 steps, none touch v₁", "⋯ 2 steps, none touch v₁"]

  describe "verdictDoc" do
    it "words the use-after-consume fixture as a proof paragraph" do
      -- The paragraph reflows at the layout width; compare the words.
      fmap (unwrap . docToText) (Verdict.verdictDoc Phrase.english Glyph.unicode uacTrace uacBlame)
        `shouldBe` Just
          "Step 8 (read) touched v₁ after its death: v₁ was consumed at step 5 (close), v₁ was touched at step 4 (write), v₁ was created at step 1 (open) — but it failed: read returned stale bytes."

    it "quotes a declared response as the observed outcome" do
      -- Give the failing step a respond note; the outcome clause quotes it.
      let (notes, events) = fixtureWith (noteAt (Clock 16) 1 Response "Just \"a\"")
          t = Trace.build notes events
          b = fromJust (Blame.analyze t)
      fmap (unwrap . docToText) (Verdict.verdictDoc Phrase.english Glyph.unicode t b)
        `shouldSatisfy` maybe False (T.isInfixOf "— but read returned Just \"a\".")

    it "agrees with the rail: every step in the prose is in the citation closure" do
      let stepsInPlan =
            [ i
            | c <- Verdict.plan uacTrace uacBlame,
              i <- case c of
                Verdict.Violated {step = i} -> [i]
                Verdict.Since {step = i} -> [i]
                _ -> []
            ]
      stepsInPlan `shouldSatisfy` all (`IntSet.member` Blame.citationClosure uacBlame)

    it "is Nothing when there is nothing to cite" do
      -- A failure whose step touched a pool value born in the same step:
      -- blame exists, but with no earlier history there are no citations.
      let t =
            Trace.build
              [ header (Clock 1) 1 "boom",
                noteAt (Clock 3) 1 (Failure Nothing) "boom"
              ]
              [eventAt (Clock 2) h1 (Born Nothing)]
          b = fromJust (Blame.analyze t)
      Verdict.verdictDoc Phrase.english Glyph.unicode t b `shouldSatisfy` \case
        Nothing -> True
        Just _ -> False

  describe "review fixes" do
    it "a lineage cycle terminates root and chain (malformed stream)" do
      let a = Var {pool = 0, id = 1}
          b = Var {pool = 0, id = 2}
          t =
            Trace.build
              [header (Clock 1) 1 "loop"]
              [ eventAt (Clock 2) a (Born (Just b)),
                eventAt (Clock 3) b (Born (Just a))
              ]
      -- Totality is the assertion: these must return, whatever they return.
      Trace.chain t a `shouldSatisfy` (not . null)
      Trace.root t a `shouldSatisfy` \v -> v == a || v == b

    it "two same-step touches yield one citation (no orphan rail column)" do
      let t =
            Trace.build
              [ header (Clock 1) 1 "open",
                header (Clock 3) 2 "double",
                header (Clock 6) 3 "boom",
                noteAt (Clock 8) 1 (Failure Nothing) "boom"
              ]
              [ eventAt (Clock 2) h1 (Born Nothing),
                eventAt (Clock 4) h1 Reused,
                eventAt (Clock 5) h1 Reused,
                eventAt (Clock 7) h1 Reused
              ]
          b = fromJust (Blame.analyze t)
      [(c.to) | c <- Blame.citations b] `shouldBe` [2, 1]

    it "pool letters stay distinct past five pools" do
      let names = [Glyph.unicode.valueName Nothing p 1 | p <- [0 .. 9]]
      length (nub names) `shouldBe` length names

  describe "glyph tables" do
    it "ascii preserves semantics within the gutter family" do
      let gutterCells = [NodeBorn, NodeTouch, NodeDeath, NodeFail, EdgeAlive, EdgeDead, EdgeElided, HistoryEnd]
      distinctUnder Glyph.ascii gutterCells `shouldBe` True

    it "ascii preserves semantics within the rail family" do
      let railCells = [RailOrigin, RailHoriz, RailVert, RailElided, RailTeeDown, RailCornerDown, RailCornerUp, RailArrow]
      distinctUnder Glyph.ascii railCells `shouldBe` True

  describe "end to end (engine)" do
    it "a real pool machine renders a ledger with a failure row and rail" do
      report <- check defaultSettings (Stateful.run eventfulMachine)
      case report.result of
        Counterexample {notes, events} -> do
          let trace = Trace.build notes events
          case Blame.analyze trace of
            Nothing -> expectationFailure "expected blame"
            Just blame -> do
              let out = docToText (Ledger.ledgerDoc (Ledger.defaultOptions Glyph.unicode) trace blame)
              out `shouldSatisfy` T.isInfixOf "✗"
              out `shouldSatisfy` T.isInfixOf "●"
        other -> expectationFailure ("expected Counterexample, got: " <> show other)

-- | Undo paragraph reflow for comparison.
unwrap :: Text -> Text
unwrap = T.unwords . T.words

-- | No two distinct cells in a family may render to the same glyph.
distinctUnder :: GlyphTable -> [Cell] -> Bool
distinctUnder table cells =
  and [table.cell a /= table.cell b | (i, a) <- zip [0 :: Int ..] cells, (j, b) <- zip [0 ..] cells, i /= j]

-- ---------------------------------------------------------------------------
-- Engine fixture (the TraceIR machine, kept local so the suites stay
-- independent)

data Model = Model
  { pool :: Pool.Pool Int,
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
