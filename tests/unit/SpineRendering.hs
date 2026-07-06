-- | Pure pins for the citation spine ("Hegel.Report.Trace.Spine") and the glyph
-- tables ("Hegel.Report.Glyph"), plus one engine run through the full path.
module SpineRendering (spec) where

import Control.Monad.IO.Class (liftIO)
import Data.List (nub)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as T
import Golden (shouldRenderAs)
import Hegel.Pool qualified as Pool
import Hegel.Property (assert, forAll)
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
import Hegel.Report.Trace.Lead qualified as Lead
import Hegel.Report.Trace.Spine qualified as Spine
import Hegel.Runner (check)
import Hegel.Settings (defaultSettings)
import Hegel.Stateful qualified as Stateful
import System.Environment (setEnv, unsetEnv)
import System.IO (stdout)
import Test.Hspec
import TraceFixtures (eventAt, eventfulMachine, flatBlame, flatFixture, flatTrace, h1, handoffBlame, handoffFixture, handoffTrace, header, noteAt)

-- ---------------------------------------------------------------------------
-- Fixture: the transfer/handoff shape (with filler steps so elision rows render)

renderWith :: Style -> Text
renderWith style = docToText (Spine.spineDoc style handoffTrace handoffBlame)

-- ---------------------------------------------------------------------------
-- Spec

spec :: Spec
spec = do
  describe "spineDoc" do
    it "renders the chronological unicode spine (transfer/handoff shape)" do
      renderWith (defaultStyle Glyph.unicode)
        `shouldRenderAs` [ "● 1  open v₁                    v₁ was created",
                           "┆    ⋯ 2 steps, none touch v₁",
                           "○ 4  write v₁ → ok              v₁ was accessed",
                           "○ 5  close v₁                   v₁ was transferred",
                           "┆    ⋯ 2 steps, none touch v₁",
                           "✗ 8  read v₁                    ← cites 1, 4, 5"
                         ]

    it "renders the chronological ascii spine" do
      renderWith (defaultStyle Glyph.ascii)
        `shouldRenderAs` [ "* 1  open v1                      v1 was created",
                           ":    ... 2 steps, none touch v1",
                           "o 4  write v1 -> ok               v1 was accessed",
                           "o 5  close v1                     v1 was transferred",
                           ":    ... 2 steps, none touch v1",
                           "x 8  read v1                      <- cites 1, 4, 5"
                         ]

    it "renders the numeric citation list, cited steps ascending" do
      let out = renderWith (defaultStyle Glyph.unicode)
      out `shouldSatisfy` T.isInfixOf "← cites 1, 4, 5"

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
      -- (The spine carries no diff — the composed report's splice does.)
      let rows = Spine.layoutRows (defaultStyle Glyph.unicode) handoffTrace handoffBlame
      fmap (\r -> (r.kind, r.stepNo)) (take 1 (reverse rows))
        `shouldBe` [(Spine.NodeRow, Just 8)]

    it "renders a non-pool draw as a free-draw detail row; pool references stay symbolic" do
      -- A cited history step (write) that draws a pool handle and a plain payload
      -- ("payload"): the pool reference keeps its symbolic name (no inline value),
      -- while the payload, bound to no touch, drops to a free-draw detail row.
      -- (The later read reuses the handle and fails.)
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
          rows = Spine.layoutRows (defaultStyle Glyph.unicode) t b
      -- The pool draw is not shown inline (no "=handle") and not a free draw.
      [r.call | r <- rows, r.kind == Spine.NodeRow] `shouldSatisfy` all (not . T.isInfixOf "handle")
      [r.call | r <- rows, r.kind == Spine.DetailRow] `shouldSatisfy` elem "payload"

    it "elides unshown steps explicitly, with counts" do
      let rows = Spine.layoutRows (defaultStyle Glyph.unicode) handoffTrace handoffBlame
      [r.call | r <- rows, r.kind == Spine.ElisionRow]
        `shouldBe` ["⋯ 2 steps, none touch v₁", "⋯ 2 steps, none touch v₁"]

    it "gives each elided lifeline its own trajectory (off-spine section)" do
      -- The failing subject (a₁) is drawn at step 3; an unrelated value (b₁) is
      -- born at step 2 and never touched again, so it's off-spine. The
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
      fmap docToText (Spine.elidedLifelinesDoc (defaultStyle Glyph.unicode) t b)
        `shouldSatisfy` maybe False (T.isInfixOf "spawn @2")

  describe "leadDoc" do
    it "words the failing value's history in rule names, chronological, sans failing step" do
      fmap (unwrap . docToText) (Lead.leadDoc (defaultStyle Glyph.unicode) flatTrace flatBlame)
        `shouldBe` Just "↳ v₁: open @1 · use @2"

    it "uses the ascii lead glyph and the plain value name under the ascii table" do
      fmap docToText (Lead.leadDoc (defaultStyle Glyph.ascii) flatTrace flatBlame)
        `shouldSatisfy` maybe False (T.isInfixOf "\\-> v1: open @1")

    it "is Nothing when the value passed through no earlier step" do
      let t =
            Trace.build
              [ header (Tick 1) 1 "boom",
                noteAt (Tick 3) 1 (Failure Nothing) "boom"
              ]
              [eventAt (Tick 2) h1 (Born Nothing)]
          b = fromJust (Blame.analyze t)
      Lead.leadDoc (defaultStyle Glyph.unicode) t b `shouldSatisfy` \case
        Nothing -> True
        Just _ -> False

  describe "review fixes" do
    it "a lineage cycle terminates root and chain (malformed stream)" do
      let a = Var {pool = 0, id = 1}
          b = Var {pool = 0, id = 2}
          t =
            Trace.build
              [header (Tick 1) 1 "loop"]
              [ eventAt (Tick 2) a (Born (Just b)),
                eventAt (Tick 3) b (Born (Just a))
              ]
      -- Totality is the assertion: these must return, whatever they return.
      Trace.chain t a `shouldSatisfy` (not . null)
      Trace.root t a `shouldSatisfy` \v -> v == a || v == b

    it "two same-step touches yield one citation (no orphan link column)" do
      let t =
            Trace.build
              [ header (Tick 1) 1 "open",
                header (Tick 3) 2 "double",
                header (Tick 6) 3 "boom",
                noteAt (Tick 8) 1 (Failure Nothing) "boom"
              ]
              [ eventAt (Tick 2) h1 (Born Nothing),
                eventAt (Tick 4) h1 Reused,
                eventAt (Tick 5) h1 Reused,
                eventAt (Tick 7) h1 Reused
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

  describe "composed report (form selection)" do
    it "spliced timeline: a pool-free stateful failure keeps the pre-trace layout byte-for-byte" do
      let report = reportOf [] (fst handoffFixture)
      out <- renderReportRich report
      out `shouldNotSatisfy` T.isInfixOf "◀"
      out `shouldNotSatisfy` T.isInfixOf "was consumed at"
      out `shouldNotSatisfy` T.isInfixOf "stored:"

    it "composed trace: pool context composes spine, splice, and footer" do
      let (notes, events) = handoffFixture
          report = (reportOf events notes) {databaseKey = Just "some-key"}
      out <- renderReportRich report
      -- The birth and handoff facts live in the spine margin.
      out `shouldSatisfy` T.isInfixOf "v₁ was created"
      out `shouldSatisfy` T.isInfixOf "v₁ was transferred"
      -- Numeric citations, cited steps ascending (reading order).
      out `shouldSatisfy` T.isInfixOf "← cites 1, 4, 5"
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

    it "a flat pool failure degrades to the timeline plus a lead" do
      out <- renderReportRich (uncurry (flip reportOf) flatFixture)
      -- The lead names the failing value's history in rule names,
      out `shouldSatisfy` T.isInfixOf "↳ v₁: open @1 · use @2"
      -- the step timeline is still there,
      out `shouldSatisfy` T.isInfixOf "Step 3: use"
      -- and there is no spine (no link arrows) or headline contrast.
      out `shouldNotSatisfy` T.isInfixOf "◀"
      out `shouldNotSatisfy` T.isInfixOf " — but "

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
      -- The drift guard for the transliteration map: splice chrome, spine
      -- glyphs, phrase typography — everything a real report emits must map
      -- to ascii, with \\x escapes reserved for genuinely foreign user text.
      report <- check defaultSettings (Stateful.run transferMachine)
      out <- renderReportRich (report {databaseKey = Just "k"} :: Report)
      Glyph.sevenBitClean out `shouldNotSatisfy` T.isInfixOf "\\x"

    it "a degraded report's lead survives sevenBitClean" do
      -- The lead glyph (↳) and separator (·) must transliterate too.
      out <- renderReportRich (uncurry (flip reportOf) flatFixture)
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

    it "a real pool machine renders a spine with birth and failure rows" do
      report <- check defaultSettings (Stateful.run eventfulMachine)
      case report.result of
        Counterexample {notes, events} -> do
          let trace = Trace.build notes events
          case Blame.analyze trace of
            Nothing -> expectationFailure "expected blame"
            Just blame -> do
              let out = docToText (Spine.spineDoc (defaultStyle Glyph.unicode) trace blame)
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
