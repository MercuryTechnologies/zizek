-- | Pure pins for the chronological event log ("Hegel.Report.Trace.Log") and
-- the glyph tables ("Hegel.Report.Glyph"), plus one engine run through the full
-- path.
module LogRendering (spec) where

import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.List (nub)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as T
import Golden (shouldRenderAs)
import Hegel.Gen qualified as Gen
import Hegel.Pool qualified as Pool
import Hegel.Property (assert, forAll, forAllWithLabel)
import Hegel.Property.Internal (Env (..), askEnv)
import Hegel.Report
  ( Event (..),
    Note (..),
    NoteKind (..),
    Operation (..),
    Report (..),
    Result (..),
    Stats (..),
    Tick (..),
    Var (..),
    renderReportRich,
  )
import Hegel.Report.Ann (docToText)
import Hegel.Report.Glyph (Cell (..), GlyphTable (..))
import Hegel.Report.Glyph qualified as Glyph
import Hegel.Report.Style (Style (..), defaultStyle)
import Hegel.Report.Trace qualified as Trace
import Hegel.Report.Trace.Blame (Citation (..), Fact (..))
import Hegel.Report.Trace.Blame qualified as Blame
import Hegel.Report.Trace.Log qualified as Log
import Hegel.Runner (check)
import Hegel.Settings (defaultSettings)
import Hegel.Stateful qualified as Stateful
import System.Environment (setEnv, unsetEnv)
import System.IO (stdout)
import Test.Hspec
import TraceFixtures (eventAt, eventfulMachine, flatFixture, h1, handoffBlame, handoffFixture, handoffTrace, header, ledgerBlame, ledgerFixture, ledgerTrace, noPoolTrace, noteAt)

-- ---------------------------------------------------------------------------
-- Fixture: the transfer/handoff shape (with filler steps so elision rows render)

renderWith :: Style -> Text
renderWith style = docToText (Log.logDoc style handoffTrace (Log.Focused handoffBlame))

-- ---------------------------------------------------------------------------
-- Spec

spec :: Spec
spec = do
  describe "logDoc" do
    it "renders the chronological unicode event log (transfer/handoff shape)" do
      renderWith (defaultStyle Glyph.unicode)
        `shouldRenderAs` [ "● @1  open v₁",
                           "┆     ⋯ 2 steps",
                           "○ @4  write v₁ → ok",
                           "◉ @5  close v₁",
                           "┆     ⋯ 2 steps",
                           "✗ @8  read v₁"
                         ]

    it "renders the chronological ascii event log" do
      renderWith (defaultStyle Glyph.ascii)
        `shouldRenderAs` [ "* @1  open v1",
                           ":     ... 2 steps",
                           "o @4  write v1 -> ok",
                           "# @5  close v1",
                           ":     ... 2 steps",
                           "x @8  read v1"
                         ]

    it "renders a setup-born value as a de-numbered origin line, cited as setup" do
      -- The subject is born in machine.initial (before the first step header),
      -- so its birth is step 0: a de-numbered @●  v₁ initialized@ origin line,
      -- cited as @setup@ rather than @\@0@.
      let notes =
            [ -- A depth-0 prelude note establishes the setup segment (step 0),
              -- where machine.initial births the value.
              noteAt (Tick 1) 0 Annotation "setup",
              header (Tick 3) 1 "poke",
              noteAt (Tick 5) 1 (Drawn [h1]) "h",
              header (Tick 6) 2 "poke",
              noteAt (Tick 8) 1 (Drawn [h1]) "h",
              noteAt (Tick 9) 1 (Failure Nothing) "boom"
            ]
          events =
            [ eventAt (Tick 2) h1 (Born Nothing),
              eventAt (Tick 4) h1 Reused,
              eventAt (Tick 7) h1 Reused
            ]
          t = Trace.build notes events
          b = fromJust (Blame.analyze t)
      docToText (Log.logDoc (defaultStyle Glyph.unicode) t (Log.Focused b))
        `shouldRenderAs` [ "●     v₁ initialized",
                           "○ @1  poke v₁",
                           "✗ @2  poke v₁"
                         ]

    it "the focused view suppresses the citation list (the lifeline is the cited set)" do
      -- Focused shows exactly the subject's cited steps as visible rows, so a
      -- citation list would only restate the lifeline. It carries none.
      let out = renderWith (defaultStyle Glyph.unicode)
      out `shouldNotSatisfy` T.isInfixOf "cites"

    it "clips the call column at the width budget" do
      let out = renderWith (defaultStyle Glyph.unicode) {callWidth = 8}
      out `shouldSatisfy` T.isInfixOf "write v⋯"

    it "the ascii clip stays inside the budget (multi-char ellipsis)" do
      let out = renderWith (defaultStyle Glyph.ascii) {callWidth = 8}
      -- 8 - 3 = 5 chars of call + "..." = exactly the budget.
      out `shouldSatisfy` T.isInfixOf "write..."
      out `shouldNotSatisfy` T.isInfixOf "write v..."

  describe "layoutRows" do
    it "puts the failing row last (chronological)" do
      -- Chronological: the oldest step leads, the failing step is the last row.
      -- (The event log carries no diff — the composed report's splice does.)
      let rows = Log.layoutRows (defaultStyle Glyph.unicode) handoffTrace (Log.Focused handoffBlame)
      fmap (\r -> (r.kind, r.stepNo)) (take 1 (reverse rows))
        `shouldBe` [(Log.NodeRow, Just 8)]

    it "inlines a short non-pool draw into the call; pool references stay symbolic" do
      -- A cited step (write) draws a pool handle and a plain payload ("payload").
      -- The pool reference keeps its symbolic name (the "handle" draw text is not
      -- shown); the short payload folds into the call. (Later read reuses the
      -- handle and fails.)
      let notes =
            [ header (Tick 1) 1 "write",
              noteAt (Tick 4) 1 (Drawn [h1]) "handle",
              noteAt (Tick 5) 1 (Drawn []) "payload",
              header (Tick 6) 2 "read",
              noteAt (Tick 9) 1 (Drawn [h1]) "handle",
              noteAt (Tick 10) 1 (Failure Nothing) "boom"
            ]
          events =
            [ eventAt (Tick 2) h1 (Born Nothing),
              eventAt (Tick 3) h1 Reused,
              eventAt (Tick 8) h1 Reused
            ]
          t = Trace.build notes events
          b = fromJust (Blame.analyze t)
          rows = Log.layoutRows (defaultStyle Glyph.unicode) t (Log.Focused b)
      -- The payload inlines into the write NodeRow's call…
      [r.call | r <- rows, r.kind == Log.NodeRow] `shouldSatisfy` any (T.isInfixOf "payload")
      -- …and the pool draw's value text ("handle") is never rendered (symbolic).
      [r.call | r <- rows] `shouldSatisfy` all (not . T.isInfixOf "handle")

    it "drops a multi-line free draw to a detail row rather than inlining it" do
      let notes =
            [ header (Tick 1) 1 "write",
              noteAt (Tick 4) 1 (Drawn [h1]) "handle",
              noteAt (Tick 5) 1 (Drawn []) "big\nvalue",
              header (Tick 6) 2 "read",
              noteAt (Tick 9) 1 (Drawn [h1]) "handle",
              noteAt (Tick 10) 1 (Failure Nothing) "boom"
            ]
          events =
            [ eventAt (Tick 2) h1 (Born Nothing),
              eventAt (Tick 3) h1 Reused,
              eventAt (Tick 8) h1 Reused
            ]
          t = Trace.build notes events
          b = fromJust (Blame.analyze t)
          rows = Log.layoutRows (defaultStyle Glyph.unicode) t (Log.Focused b)
      -- The multi-line value can't inline; it becomes dim detail rows, one per line.
      [r.call | r <- rows, r.kind == Log.DetailRow] `shouldBe` ["big", "value"]
      [r.call | r <- rows, r.kind == Log.NodeRow] `shouldSatisfy` all (not . T.isInfixOf "big")

    it "elides unshown steps explicitly, with counts" do
      let rows = Log.layoutRows (defaultStyle Glyph.unicode) handoffTrace (Log.Focused handoffBlame)
      [r.call | r <- rows, r.kind == Log.ElisionRow]
        `shouldBe` ["⋯ 2 steps", "⋯ 2 steps"]

    it "names the value an elided run concerns (positive qualifier)" do
      -- Subject v₁ is shown at steps 1 and 3; the elided step 2 spawns an
      -- unrelated w₁, so the elision row names what it hides, not what it doesn't.
      let w1 = Var {pool = 1, id = 4}
          notes =
            [ header (Tick 1) 1 "open",
              header (Tick 3) 2 "spawn",
              header (Tick 5) 3 "read",
              noteAt (Tick 7) 1 (Failure Nothing) "boom"
            ]
          events =
            [ eventAt (Tick 2) h1 (Born Nothing),
              eventAt (Tick 4) w1 (Born Nothing),
              eventAt (Tick 6) h1 Reused
            ]
          t = Trace.build notes events
          b = fromJust (Blame.analyze t)
          rows = Log.layoutRows (defaultStyle Glyph.unicode) t (Log.Focused b)
      [r.call | r <- rows, r.kind == Log.ElisionRow] `shouldBe` ["⋯ 1 step (w₁)"]

    it "gives each elided lifeline its own trajectory (off-log section)" do
      -- The failing subject (a₁) is drawn at step 3; an unrelated value (b₁) is
      -- born at step 2 and never touched again, so it's off-log. The
      -- elided-lifelines section reports what it did (`spawn @2`).
      let a1 = h1
          b1 = Var {pool = 1, id = 4}
          notes =
            [ header (Tick 1) 1 "open",
              header (Tick 3) 2 "spawn",
              header (Tick 5) 3 "read",
              noteAt (Tick 7) 1 (Failure Nothing) "boom"
            ]
          events =
            [ eventAt (Tick 2) a1 (Born Nothing),
              eventAt (Tick 4) b1 (Born Nothing),
              eventAt (Tick 6) a1 Reused
            ]
          t = Trace.build notes events
          b = fromJust (Blame.analyze t)
      fmap docToText (Log.elidedLifelinesDoc (defaultStyle Glyph.unicode) t b)
        `shouldSatisfy` maybe False (T.isInfixOf "spawn @2")

  describe "unfocused log" do
    it "renders a pool-free journal: all steps, blank gutters, ✗ on failure, no margins" do
      docToText (Log.logDoc (defaultStyle Glyph.unicode) noPoolTrace (Log.Unfocused Nothing))
        `shouldRenderAs` [ "  @1  push 0",
                           "  @2  push 1",
                           "      sum is now 1",
                           "✗ @3  pop"
                         ]

    it "renders a two-root ledger: all steps shown; all-cited so no citation row" do
      -- Every shown step is cited, so the citation would select nothing — the row
      -- is suppressed (the event log already shows the full evidence).
      docToText (Log.logDoc (defaultStyle Glyph.unicode) ledgerTrace (Log.Unfocused (Just ledgerBlame)))
        `shouldRenderAs` [ "● @1  open v₁",
                           "● @2  open v₂",
                           "○ @3  deposit v₁ 5",
                           "      balance a₁ = 5",
                           "✗ @4  audit v₁ v₂"
                         ]

    it "shows the citation row only for a subset (a shown step is uncited)" do
      -- open a₁, open a₂, a load-bearing but pool-untouching @tick@, then a
      -- failing @settle@ touching both accounts: @tick@ (step 3) is shown but
      -- uncited, so the row is an explicit subset — not suppressed, not "all".
      let a1 = Var {pool = 0, id = 1}
          a2 = Var {pool = 0, id = 2}
          notes =
            [ header (Tick 1) 1 "open",
              header (Tick 3) 2 "open",
              header (Tick 5) 3 "tick",
              header (Tick 6) 4 "settle",
              noteAt (Tick 11) 1 (Failure Nothing) "boom"
            ]
          events =
            [ eventAt (Tick 2) a1 (Born Nothing),
              eventAt (Tick 4) a2 (Born Nothing),
              eventAt (Tick 7) a1 Reused,
              eventAt (Tick 8) a2 Reused
            ]
          t = Trace.build notes events
          b = fromJust (Blame.analyze t)
      docToText (Log.logDoc (defaultStyle Glyph.unicode) t (Log.Unfocused (Just b)))
        `shouldRenderAs` [ "● @1  open v₁",
                           "● @2  open v₂",
                           "  @3  tick",
                           "✗ @4  settle v₁ v₂",
                           "      ↳ cites @1, @2"
                         ]
      docToText (Log.logDoc (defaultStyle Glyph.ascii) t (Log.Unfocused (Just b)))
        `shouldSatisfy` T.isInfixOf "\\_ cites @1, @2"

  describe "glyph tables" do
    it "ascii preserves semantics within the gutter family" do
      let gutterCells = [NodeBorn, NodeTouch, NodeTransfer, NodeDeath, NodeFail, EdgeAlive, EdgeElided, HistoryEnd]
      distinctUnder Glyph.ascii gutterCells `shouldBe` True

    it "pool letters stay distinct past five pools" do
      let names = [Glyph.unicode.valueName Nothing p 1 | p <- [0 .. 9]]
      length (nub names) `shouldBe` length names

  describe "composed report (form selection)" do
    it "spliced timeline: a pool-free stateful failure renders without a reproduction footer" do
      let report = reportOf [] (fst handoffFixture)
      out <- renderReportRich report
      out `shouldNotSatisfy` T.isInfixOf "stored:"

    it "composed trace: pool context composes event log, splice, and footer" do
      let (notes, events) = handoffFixture
          report = (reportOf events notes) {databaseKey = Just "some-key"}
      out <- renderReportRich report
      -- The handoff (close) renders with the transfer glyph in the event log.
      out `shouldSatisfy` T.isInfixOf "◉ @5  close v₁"
      -- Focused suppresses the citation list (the lifeline is the cited set).
      out `shouldNotSatisfy` T.isInfixOf "cites"
      -- The failing step's splice (fixture notes carry no locs, so its lines
      -- are the structured fallbacks); the reason lives here, not in a headline.
      out `shouldSatisfy` T.isInfixOf "  Step 8: read"
      out `shouldSatisfy` T.isInfixOf "✗ read returned stale bytes"
      out `shouldSatisfy` T.isInfixOf "stored: some-key — replays automatically next run"

    it "footnotes keep their after-the-body position on the composed form" do
      let (notes, events) = handoffFixture
          withFootnote = notes <> [noteAt (Tick 20) 0 Footnote "handle table dump: {}"]
      out <- renderReportRich ((reportOf events withFootnote) {databaseKey = Just "k"})
      out `shouldSatisfy` T.isInfixOf "handle table dump: {}"
      -- After the splice, before the reproduction line.
      T.breakOn "handle table dump" out `shouldSatisfy` \(before, rest) ->
        "Step 8: read" `T.isInfixOf` before && "stored: k" `T.isInfixOf` rest

    it "the footer only renders when a database key exists" do
      out <- renderReportRich (uncurry (flip reportOf) handoffFixture)
      out `shouldNotSatisfy` T.isInfixOf "stored:"

    it "a multi-root failure renders unfocused (all steps, no elision)" do
      -- The failing settle touches two lineage roots, so chooseView picks
      -- Unfocused: every step is shown (no focus subject to elide around),
      -- while the blame still supplies per-step margins.
      out <- renderReportRich (uncurry (flip reportOf) ledgerFixture)
      out `shouldSatisfy` T.isInfixOf "open v₁"
      out `shouldSatisfy` T.isInfixOf "open v₂"
      -- Every shown step is cited, so the citation row is suppressed (selects nothing).
      out `shouldNotSatisfy` T.isInfixOf "↳"

    it "delta-only: a call that names its touched values carries no margin fact" do
      -- compare/settle name v₁ v₂ in the call, so a lifecycle fact would only
      -- restate them: the margin stays empty (the glyph and verb carry it).
      let a1 = Var {pool = 0, id = 1}
          a2 = Var {pool = 0, id = 2}
          notes =
            [ header (Tick 1) 1 "open",
              noteAt (Tick 3) 1 (Drawn [a1]) "acct",
              header (Tick 4) 2 "open",
              noteAt (Tick 6) 1 (Drawn [a2]) "acct",
              header (Tick 7) 3 "compare",
              header (Tick 12) 4 "settle",
              noteAt (Tick 17) 1 (Failure Nothing) "boom"
            ]
          events =
            [ eventAt (Tick 2) a1 (Born Nothing),
              eventAt (Tick 5) a2 (Born Nothing),
              eventAt (Tick 8) a1 Reused,
              eventAt (Tick 9) a2 Reused,
              eventAt (Tick 13) a1 Reused,
              eventAt (Tick 14) a2 Reused
            ]
          t = Trace.build notes events
          b = fromJust (Blame.analyze t)
      let out = docToText (Log.logDoc (defaultStyle Glyph.unicode) t (Log.Unfocused (Just b)))
      out `shouldSatisfy` T.isInfixOf "○ @3  compare v₁ v₂"
      out `shouldNotSatisfy` T.isInfixOf "accessed"

    it "a flat single-value pool failure renders as a focused log (no lead)" do
      out <- renderReportRich (uncurry (flip reportOf) flatFixture)
      -- A single-value failure is the focused log: born row in the event log, no
      -- margin fact, and (being focused) no citation list.
      out `shouldSatisfy` T.isInfixOf "● @1  open v₁"
      out `shouldNotSatisfy` T.isInfixOf "cites"
      -- The failing step's reason is spliced (structured fallback here).
      out `shouldSatisfy` T.isInfixOf "Step 3: use"
      -- No cite-relocation row (focused suppresses the citation list).
      out `shouldNotSatisfy` T.isInfixOf "↳"

  describe "glyph preference" do
    it "HEGEL_GLYPHS overrides detection in both directions" do
      setEnv "HEGEL_GLYPHS" "ascii"
      p1 <- Glyph.preference stdout
      setEnv "HEGEL_GLYPHS" "unicode"
      p2 <- Glyph.preference stdout
      unsetEnv "HEGEL_GLYPHS"
      (p1, p2) `shouldBe` (Glyph.PreferAscii, Glyph.PreferUnicode)

    it "sevenBitClean transliterates known glyphs and escapes only the unknown" do
      -- Every untabled splice-chrome glyph (┏ ━ ┃ ⋮ from "Source", ✗ from the
      -- in-band failure mark), plus the typography (· — –) and subscripts, maps
      -- to its ascii form; only genuinely foreign user text (here: a CJK
      -- character) falls back to an escape. The direct drift guard for the
      -- hand-maintained chrome list in "Hegel.Report.Glyph".
      Glyph.sevenBitClean "✗ ┏ ━ ┃ ⋮ · — – v₁ 好"
        `shouldBe` "x + - | : . -- - v1 \\x597d"

    it "a full report survives sevenBitClean without escapes (chrome is transliterated)" do
      -- The drift guard for the transliteration map: splice chrome, event log
      -- glyphs, phrase typography — everything a real report emits must map
      -- to ascii, with \\x escapes reserved for genuinely foreign user text.
      report <- check defaultSettings (Stateful.run transferMachine)
      out <- renderReportRich (report {databaseKey = Just "k"} :: Report)
      Glyph.sevenBitClean out `shouldNotSatisfy` T.isInfixOf "\\x"

    it "a flat single-value report survives sevenBitClean" do
      -- The focused-log chrome and phrase typography must transliterate too.
      out <- renderReportRich (uncurry (flip reportOf) flatFixture)
      Glyph.sevenBitClean out `shouldNotSatisfy` T.isInfixOf "\\x"

  describe "forAllWithLabel" do
    it "journals the label with the drawn value (name=value)" do
      report <-
        check defaultSettings do
          n <- forAllWithLabel "qty" (Gen.int & Gen.min 5 & Gen.max 5 & Gen.build)
          assert (n /= (5 :: Int)) "boom"
      case report.result of
        Counterexample {notes} -> fmap (.text) notes `shouldSatisfy` elem "qty=5"
        _ -> expectationFailure "expected a counterexample"

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

    it "a real pool machine renders an event log with birth and failure rows" do
      report <- check defaultSettings (Stateful.run eventfulMachine)
      case report.result of
        Counterexample {notes, events} -> do
          let trace = Trace.build notes events
          case Blame.analyze trace of
            Nothing -> expectationFailure "expected blame"
            Just blame -> do
              let out = docToText (Log.logDoc (defaultStyle Glyph.unicode) trace (Log.Focused blame))
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
