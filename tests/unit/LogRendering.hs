-- | Pure pins for the flat chronological event log ("Hegel.Report.Layout")
-- and the glyph tables ("Hegel.Report.Style"), plus one engine run through
-- the full path.
module LogRendering (spec) where

import Data.Default.Class (def)
import Data.Function ((&))
import Data.List (nub)
import Data.Text (Text)
import Data.Text qualified as T
import Golden (shouldRenderAs)
import Hegel.Gen qualified as Gen
import Hegel.Pool qualified as Pool
import Hegel.Property (assert, forAll, forAllWithLabel)
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
import Hegel.Report.Layout qualified as Layout
import Hegel.Report.Style (GlyphTable (..), Style (..), defaultStyle)
import Hegel.Report.Style qualified as Style
import Hegel.Report.Trace qualified as Trace
import Hegel.Runner (check)
import Hegel.Stateful qualified as Stateful
import System.Environment (setEnv, unsetEnv)
import System.IO (stdout)
import Test.Hspec
import TraceFixtures (eventAt, eventfulMachine, flatFixture, h1, handoffFixture, handoffTrace, header, ledgerFixture, ledgerTrace, noPoolTrace, noteAt)

-- ---------------------------------------------------------------------------
-- Fixture: the transfer/handoff shape (with filler steps so elision rows render)

renderWith :: Style -> Text
renderWith style = docToText (Layout.logDoc style handoffTrace)

-- ---------------------------------------------------------------------------
-- Spec

spec :: Spec
spec = do
  describe "logDoc" do
    it "renders the chronological unicode event log (transfer/handoff shape)" do
      renderWith (defaultStyle Style.unicode)
        `shouldRenderAs` [ "  Step 1: open v₁",
                           "          2 steps elided",
                           "  Step 4: write v₁ → ok",
                           "  Step 5: close v₁",
                           "          2 steps elided",
                           "✗ Step 8: read v₁"
                         ]

    it "renders the chronological ascii event log" do
      renderWith (defaultStyle Style.ascii)
        `shouldRenderAs` [ "  Step 1: open v1",
                           "          2 steps elided",
                           "  Step 4: write v1 -> ok",
                           "  Step 5: close v1",
                           "          2 steps elided",
                           "x Step 8: read v1"
                         ]

    it "renders setup-step notes as a detail line (no de-numbered origin row)" do
      -- The subject is born in machine.initial, before the first step
      -- header, so its birth lands in the setup step, index 0, which
      -- contributes a detail line but never a call row of its own.
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
      docToText (Layout.logDoc (defaultStyle Style.unicode) t)
        `shouldRenderAs` [ "          setup",
                           "  Step 1: poke v₁",
                           "✗ Step 2: poke v₁"
                         ]

    it "clips the call column at the width budget" do
      let out = renderWith (defaultStyle Style.unicode) {callWidth = 8}
      out `shouldSatisfy` T.isInfixOf "write v⋯"

    it "the ascii clip stays inside the budget (multi-char ellipsis)" do
      let out = renderWith (defaultStyle Style.ascii) {callWidth = 8}
      -- 8 - 3 = 5 chars of call + "..." = exactly the budget.
      out `shouldSatisfy` T.isInfixOf "write..."
      out `shouldNotSatisfy` T.isInfixOf "write v..."

  describe "layoutRows" do
    it "puts the failing row last (chronological)" do
      -- Chronological: the oldest step leads, the failing step is the last row.
      -- (The event log carries no diff — the composed report's splice does.)
      let rows = Layout.layoutRows (defaultStyle Style.unicode) handoffTrace
      fmap (\r -> (r.kind, r.stepNo)) (take 1 (reverse rows))
        `shouldBe` [(Layout.NodeRow, Just 8)]

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
          rows = Layout.layoutRows (defaultStyle Style.unicode) t
      -- The payload inlines into the write NodeRow's call...
      [r.call | r <- rows, r.kind == Layout.NodeRow] `shouldSatisfy` any (T.isInfixOf "payload")
      -- ...and the pool draw's value text ("handle") is never rendered (symbolic).
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
          rows = Layout.layoutRows (defaultStyle Style.unicode) t
      -- The multi-line value can't inline; it becomes dim detail rows, one per line.
      [r.call | r <- rows, r.kind == Layout.DetailRow] `shouldBe` ["big", "value"]
      [r.call | r <- rows, r.kind == Layout.NodeRow] `shouldSatisfy` all (not . T.isInfixOf "big")

    it "elides unshown steps explicitly, with counts" do
      let rows = Layout.layoutRows (defaultStyle Style.unicode) handoffTrace
      [r.call | r <- rows, r.kind == Layout.ElisionRow]
        `shouldBe` ["2 steps elided", "2 steps elided"]

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
          rows = Layout.layoutRows (defaultStyle Style.unicode) t
      [r.call | r <- rows, r.kind == Layout.ElisionRow] `shouldBe` ["1 step elided (w₁)"]

  describe "flat log" do
    it "renders a pool-free journal: all steps, blank gutters, ✗ on failure" do
      docToText (Layout.logDoc (defaultStyle Style.unicode) noPoolTrace)
        `shouldRenderAs` [ "  Step 1: push 0",
                           "  Step 2: push 1",
                           "          sum is now 1",
                           "✗ Step 3: pop"
                         ]

    it "renders a two-root ledger: every step kept, nothing elided" do
      -- The failing audit touches both accounts, so both are relevant roots —
      -- every step touches one of them, so nothing is left to elide.
      docToText (Layout.logDoc (defaultStyle Style.unicode) ledgerTrace)
        `shouldRenderAs` [ "  Step 1: open v₁",
                           "  Step 2: open v₂",
                           "  Step 3: deposit v₁ 5",
                           "          balance a₁ = 5",
                           "✗ Step 4: audit v₁ v₂"
                         ]

  describe "glyph tables" do
    it "pool letters stay distinct past five pools" do
      let names = [Style.unicode.valueName Nothing p 1 | p <- [0 .. 9]]
      length (nub names) `shouldBe` length names

  describe "composed report" do
    it "spliced timeline: a pool-free stateful failure renders without a reproduction footer" do
      let report = reportOf [] (fst handoffFixture)
      out <- renderReportRich report
      out `shouldNotSatisfy` T.isInfixOf "stored under"

    it "composed trace: pool context composes event log, splice, and footer" do
      let (notes, events) = handoffFixture
          report = (reportOf events notes) {databaseKey = Just "some-key"}
      out <- renderReportRich report
      -- The handoff (close) renders as an ordinary kept row (no lifecycle glyph).
      out `shouldSatisfy` T.isInfixOf "  Step 5: close v₁"
      out `shouldNotSatisfy` T.isInfixOf "cites"
      -- The failing step's splice (fixture notes carry no locs, so its lines
      -- are the structured fallbacks); the reason lives here, not in a headline.
      out `shouldSatisfy` T.isInfixOf "  Step 8: read"
      out `shouldSatisfy` T.isInfixOf "✗ read returned stale bytes"
      out `shouldSatisfy` T.isInfixOf "stored under some-key and replays automatically next run"

    it "footnotes keep their after-the-body position on the composed form" do
      let (notes, events) = handoffFixture
          withFootnote = notes <> [noteAt (Tick 20) 0 Footnote "handle table dump: {}"]
      out <- renderReportRich ((reportOf events withFootnote) {databaseKey = Just "k"})
      out `shouldSatisfy` T.isInfixOf "handle table dump: {}"
      -- After the splice, before the reproduction line.
      T.breakOn "handle table dump" out `shouldSatisfy` \(pre, rest) ->
        "Step 8: read" `T.isInfixOf` pre && "stored under k" `T.isInfixOf` rest

    it "the footer only renders when a database key exists" do
      out <- renderReportRich (uncurry (flip reportOf) handoffFixture)
      out `shouldNotSatisfy` T.isInfixOf "stored under"

    it "a multi-root failure renders every step, with nothing elided" do
      -- The failing settle touches two lineage roots, so both are relevant —
      -- there is no step left that touches neither.
      out <- renderReportRich (uncurry (flip reportOf) ledgerFixture)
      out `shouldSatisfy` T.isInfixOf "open v₁"
      out `shouldSatisfy` T.isInfixOf "open v₂"
      out `shouldNotSatisfy` T.isInfixOf "↳"

    it "a flat single-value pool failure renders as a flat log" do
      out <- renderReportRich (uncurry (flip reportOf) flatFixture)
      out `shouldSatisfy` T.isInfixOf "  Step 1: open v₁"
      out `shouldNotSatisfy` T.isInfixOf "cites"
      -- The failing step's reason is spliced (structured fallback here).
      out `shouldSatisfy` T.isInfixOf "Step 3: use"
      out `shouldNotSatisfy` T.isInfixOf "↳"

  describe "glyph preference" do
    it "HEGEL_GLYPHS overrides detection in both directions" do
      setEnv "HEGEL_GLYPHS" "ascii"
      p1 <- Style.preference stdout
      setEnv "HEGEL_GLYPHS" "unicode"
      p2 <- Style.preference stdout
      unsetEnv "HEGEL_GLYPHS"
      (p1, p2) `shouldBe` (Style.PreferAscii, Style.PreferUnicode)

    it "sevenBitClean transliterates known glyphs and escapes only the unknown" do
      -- Every untabled splice-chrome glyph (┏ ━ ┃ ⋮ from "Source", ✗ from the
      -- in-band failure mark), plus the typography (· — –) and subscripts, maps
      -- to its ascii form; only genuinely foreign user text (here: a CJK
      -- character) falls back to an escape. The direct drift guard for the
      -- hand-maintained chrome list in "Hegel.Report.Style".
      Style.sevenBitClean "✗ ┏ ━ ┃ ⋮ · — – v₁ 好"
        `shouldBe` "x + - | : . -- - v1 \\x597d"

    it "a full report survives sevenBitClean without escapes (chrome is transliterated)" do
      -- The drift guard for the transliteration map: splice chrome, event log
      -- glyphs, phrase typography — everything a real report emits must map
      -- to ascii, with \\x escapes reserved for genuinely foreign user text.
      report <- check def (Stateful.run transferMachine)
      out <- renderReportRich (report {databaseKey = Just "k"} :: Report)
      Style.sevenBitClean out `shouldNotSatisfy` T.isInfixOf "\\x"

    it "a flat single-value report survives sevenBitClean" do
      -- The flat-log chrome and phrase typography must transliterate too.
      out <- renderReportRich (uncurry (flip reportOf) flatFixture)
      Style.sevenBitClean out `shouldNotSatisfy` T.isInfixOf "\\x"

  describe "forAllWithLabel" do
    it "journals the label with the drawn value (name=value)" do
      report <-
        check def do
          n <- forAllWithLabel "qty" (Gen.int & Gen.min 5 & Gen.max 5 & Gen.build)
          assert (n /= (5 :: Int)) "boom"
      case report.result of
        Counterexample {notes} -> fmap (.text) notes `shouldSatisfy` elem "qty=5"
        _ -> expectationFailure "expected a counterexample"

  describe "end to end (engine)" do
    it "a real pool machine renders an event log with a failing row" do
      report <- check def (Stateful.run eventfulMachine)
      case report.result of
        Counterexample {notes, events} -> do
          let trace = Trace.build notes events
              out = docToText (Layout.logDoc (defaultStyle Style.unicode) trace)
          out `shouldSatisfy` T.isInfixOf "✗"
          out `shouldSatisfy` T.isInfixOf "register"
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

-- | A two-pool transfer machine: read_closed fails on any transferred
-- handle, so the minimal counterexample is open → close (transfer) → read.
transferMachine :: Stateful.Machine (Pool.Pool Int, Pool.Pool Int) IO
transferMachine =
  Stateful.Machine
    { initial = do
        open <- Pool.named "h"
        closed <- Pool.named "c"
        pure (open, closed),
      rules =
        [ Stateful.Rule "open" \m@(open, _) -> do
            Pool.add open 1
            pure m,
          Stateful.Rule "close" \m@(open, closed) -> do
            _ <- forAll (Pool.transfer open closed)
            pure m,
          Stateful.Rule "read_closed" \m@(_, closed) -> do
            _ <- forAll (Pool.reuse closed)
            assert False "reads of closed handles always fail (bug)"
            pure m
        ],
      invariants = []
    }
