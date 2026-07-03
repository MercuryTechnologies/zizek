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
    Stats (..),
    Var (..),
    renderReportRich,
  )
import Hegel.Report.Ann (docToText)
import Hegel.Report.Blame (Blame, Citation (..), Fact (..))
import Hegel.Report.Blame qualified as Blame
import Hegel.Report.Glyph (Cell (..), GlyphTable (..))
import Hegel.Report.Glyph qualified as Glyph
import Hegel.Report.Ledger qualified as Ledger
import Hegel.Report.Style (Direction (..), Style (..), defaultStyle)
import Hegel.Report.Trace (Trace)
import Hegel.Report.Trace qualified as Trace
import Hegel.Report.Verdict qualified as Verdict
import Hegel.Runner (check)
import Hegel.Settings (defaultSettings)
import Hegel.Stateful qualified as Stateful
import System.Environment (setEnv, unsetEnv)
import System.IO (stdout)
import Test.Hspec
import TraceFixtures (Model (..), eventAt, eventfulMachine, h1, header, noteAt, uacBlame, uacFixture, uacTrace)

-- ---------------------------------------------------------------------------
-- Fixture: the mockup-A shape (with filler steps so elision rows render)

-- | The fixture's raw streams with one extra note spliced in before the
-- failure note (clock order is what matters; the list stays sorted).
fixtureWith :: Note -> ([Note], [Event])
fixtureWith extra = (sortOn (.clock) (extra : notes), events)
  where
    (notes, events) = uacFixture

renderWith :: Style -> Text
renderWith style = docToText (Ledger.ledgerDoc style uacTrace uacBlame)

-- ---------------------------------------------------------------------------
-- Spec

spec :: Spec
spec = do
  describe "ledgerDoc" do
    it "renders the failure-first unicode ledger (mockup-A shape)" do
      renderWith (defaultStyle Glyph.unicode)
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
      renderWith (defaultStyle Glyph.unicode) {direction = Chronological}
        `shouldBe` T.intercalate
          "\n"
          [ "● 1  open v₁                    ◀─────╮   v₁ was created",
            "┆    ⋯ 2 steps, none touch v₁         ┆",
            "○ 4  write v₁ → ok              ◀───╮ │   v₁ was touched",
            "◌ 5  close v₁                   ◀─╮ │ │   v₁ was consumed",
            "┆    ⋯ 2 steps, none touch v₁     ┆ ┆ ┆",
            "✗ 8  read v₁                    ●─┴─┴─╯",
            "│    read returned stale bytes"
          ]

    it "renders the failure-first ascii ledger" do
      renderWith (defaultStyle Glyph.ascii)
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
      let out = renderWith (defaultStyle Glyph.unicode) {railBudget = 2}
      out `shouldSatisfy` T.isInfixOf "← cites 5, 4, 1"
      out `shouldNotSatisfy` T.isInfixOf "◀"

    it "clips the call column at the width budget" do
      let out = renderWith (defaultStyle Glyph.unicode) {callWidth = 8}
      out `shouldSatisfy` T.isInfixOf "write v⋯"

    it "the ascii clip stays inside the budget (multi-char ellipsis)" do
      let out = renderWith (defaultStyle Glyph.ascii) {callWidth = 8}
      -- 8 - 3 = 5 chars of call + "..." = exactly the budget.
      out `shouldSatisfy` T.isInfixOf "write..."
      out `shouldNotSatisfy` T.isInfixOf "write v..."

  describe "layoutRows" do
    it "puts the failing row first (failure-first) with its details beneath in both directions" do
      let rows d =
            Ledger.layoutRows
              (defaultStyle Glyph.unicode) {direction = d}
              uacTrace
              uacBlame
          kinds d = fmap (.kind) (rows d)
      take 2 (kinds FailureFirst)
        `shouldBe` [Ledger.NodeRow, Ledger.DetailRow]
      drop (length (kinds Chronological) - 2) (kinds Chronological)
        `shouldBe` [Ledger.NodeRow, Ledger.DetailRow]

    it "splits a multi-line failure message into one detail row per line" do
      let (notes, events) = uacFixture
          multi =
            [ if n.kind == Failure Nothing
                then n {text = "expected open\ngot closed"} :: Note
                else n
            | n <- notes
            ]
          t = Trace.build multi events
          b = fromJust (Blame.analyze t)
          rows = Ledger.layoutRows (defaultStyle Glyph.unicode) t b
      [r.call | r <- rows, r.kind == Ledger.DetailRow]
        `shouldBe` ["expected open", "got closed"]

    it "elides unshown steps explicitly, with counts" do
      let rows = Ledger.layoutRows (defaultStyle Glyph.unicode) uacTrace uacBlame
      [r.call | r <- rows, r.kind == Ledger.ElisionRow]
        `shouldBe` ["⋯ 2 steps, none touch v₁", "⋯ 2 steps, none touch v₁"]

  describe "verdictDoc" do
    it "words the use-after-consume fixture as a proof paragraph" do
      -- The paragraph reflows at the layout width; compare the words.
      fmap (unwrap . docToText) (Verdict.verdictDoc (defaultStyle Glyph.unicode) uacTrace uacBlame)
        `shouldBe` Just
          "Step 8 (read) touched v₁ after its death: v₁ was consumed at step 5 (close), v₁ was touched at step 4 (write), v₁ was created at step 1 (open) — but it failed: read returned stale bytes."

    it "quotes a declared response as the observed outcome" do
      -- Give the failing step a respond note; the outcome clause quotes it.
      let (notes, events) = fixtureWith (noteAt (Clock 16) 1 Response "Just \"a\"")
          t = Trace.build notes events
          b = fromJust (Blame.analyze t)
      fmap (unwrap . docToText) (Verdict.verdictDoc (defaultStyle Glyph.unicode) t b)
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
      Verdict.verdictDoc (defaultStyle Glyph.unicode) t b `shouldSatisfy` \case
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

  describe "composed report (the wired ladder)" do
    it "rung 1: a pool-free stateful failure keeps today's layout exactly" do
      let report = reportOf [] (fst uacFixture)
      out <- renderReportRich report
      out `shouldNotSatisfy` T.isInfixOf "◀"
      out `shouldNotSatisfy` T.isInfixOf "was consumed at"
      out `shouldNotSatisfy` T.isInfixOf "stored:"

    it "rung 3: pool context composes verdict, ledger, splice, and footer" do
      let (notes, events) = uacFixture
          report = (reportOf events notes) {databaseKey = Just "some-key"}
      out <- renderReportRich report
      out `shouldSatisfy` T.isInfixOf "Step 8 (read) touched v₁ after its death"
      out `shouldSatisfy` T.isInfixOf "◀─────╯"
      -- The freeze-frame panel (fixture notes carry no locs, so its lines
      -- are the structured fallbacks).
      out `shouldSatisfy` T.isInfixOf "  Step 8: read"
      out `shouldSatisfy` T.isInfixOf "✗ read returned stale bytes"
      out `shouldSatisfy` T.isInfixOf "stored: some-key — replays automatically next run"

    it "footnotes keep their after-the-body position on the composed rung" do
      let (notes, events) = uacFixture
          withFootnote = notes <> [noteAt (Clock 18) 0 Footnote "handle table dump: {}"]
      out <- renderReportRich ((reportOf events withFootnote) {databaseKey = Just "k"})
      out `shouldSatisfy` T.isInfixOf "handle table dump: {}"
      -- After the splice, before the reproduction line.
      T.breakOn "handle table dump" out `shouldSatisfy` \(before, rest) ->
        "Step 8: read" `T.isInfixOf` before && "stored: k" `T.isInfixOf` rest

    it "the footer only renders when a database key exists" do
      out <- renderReportRich (uncurry (flip reportOf) uacFixture)
      out `shouldNotSatisfy` T.isInfixOf "stored:"

  describe "glyph preference" do
    it "HEGEL_GLYPHS overrides detection in both directions" do
      setEnv "HEGEL_GLYPHS" "ascii"
      p1 <- Glyph.preference stdout
      setEnv "HEGEL_GLYPHS" "unicode"
      p2 <- Glyph.preference stdout
      unsetEnv "HEGEL_GLYPHS"
      (p1, p2) `shouldBe` (Glyph.PreferAscii, Glyph.PreferUnicode)

    it "sevenBitClean transliterates known glyphs and escapes only the unknown" do
      -- Cells and chrome map to their ascii forms; genuinely foreign user
      -- text (here: a CJK character) falls back to an escape.
      Glyph.sevenBitClean "✗ v₁ ┃ · — 好" `shouldBe` "x v1 | . -- \\x597d"

    it "a full report survives sevenBitClean without escapes (chrome is transliterated)" do
      -- The drift guard for the transliteration map: splice chrome, ledger
      -- glyphs, phrase typography — everything a real report emits must map
      -- to ascii, with \\x escapes reserved for genuinely foreign user text.
      report <- check defaultSettings (Stateful.run transferMachine)
      out <- renderReportRich (report {databaseKey = Just "k"} :: Report)
      Glyph.sevenBitClean out `shouldNotSatisfy` T.isInfixOf "\\x"

  describe "end to end (engine)" do
    it "transfer reconnects the chain on a real machine (citations reach pre-transfer steps)" do
      report <- check defaultSettings (Stateful.run transferMachine)
      case report.result of
        Counterexample {notes, events} -> do
          let t = Trace.build notes events
          case Blame.analyze t of
            Nothing -> expectationFailure "expected blame"
            Just b -> do
              -- The failing read touches the closed-pool var; the chain must
              -- cite the open-pool var's handoff *and* its birth — and the
              -- lineage-continued consumption reads as a transfer.
              let facts = [c.fact | c <- Blame.citations b]
              [() | TransferredAt _ <- facts] `shouldSatisfy` (not . null)
              [() | BornAt _ <- facts] `shouldSatisfy` (not . null)
        other -> expectationFailure ("expected Counterexample, got: " <> show other)

    it "a real pool machine renders a ledger with a failure row and rail" do
      report <- check defaultSettings (Stateful.run eventfulMachine)
      case report.result of
        Counterexample {notes, events} -> do
          let trace = Trace.build notes events
          case Blame.analyze trace of
            Nothing -> expectationFailure "expected blame"
            Just blame -> do
              let out = docToText (Ledger.ledgerDoc (defaultStyle Glyph.unicode) trace blame)
              out `shouldSatisfy` T.isInfixOf "✗"
              out `shouldSatisfy` T.isInfixOf "●"
        other -> expectationFailure ("expected Counterexample, got: " <> show other)

-- | A synthetic stateful counterexample report over the fixture streams.
reportOf :: [Event] -> [Note] -> Report
reportOf events notes =
  Report
    { result =
        Counterexample
          { message = "read returned stale bytes",
            notes,
            events,
            loc = Nothing,
            diff = Nothing
          },
      stats = Stats {valid = 1, invalid = 0},
      databaseKey = Nothing
    }

-- | Undo paragraph reflow for comparison.
unwrap :: Text -> Text
unwrap = T.unwords . T.words

-- | No two distinct cells in a family may render to the same glyph.
distinctUnder :: GlyphTable -> [Cell] -> Bool
distinctUnder table cells =
  and [table.cell a /= table.cell b | (i, a) <- zip [0 :: Int ..] cells, (j, b) <- zip [0 ..] cells, i /= j]

-- | A two-pool transfer machine: read_closed fails on any transferred
-- handle, so the minimal counterexample is open → close (transfer) → read.
transferMachine :: Stateful.Machine (Pool.Pool Int, Pool.Pool Int) IO
transferMachine =
  Stateful.Machine
    { initial = do
        env <- askEnv
        open <- liftIO (Pool.named "h" env.testCase)
        closed <- liftIO (Pool.named "c" env.testCase)
        pure (open, closed),
      rules =
        [ Stateful.Rule "open" \m@(open, _) -> do
            liftIO (Pool.add open 1)
            pure m,
          Stateful.Rule "close" \m@(open, closed) -> do
            _ <- forAll (Pool.transfer open closed)
            pure m,
          Stateful.Rule "read_closed" \m@(_, closed) -> do
            _ <- forAll (Pool.valuesReusable closed)
            assert False "reads of closed handles always fail (bug)"
            pure m
        ],
      invariants = []
    }
