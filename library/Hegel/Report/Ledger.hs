-- | The citation ledger: the R3 trace layout — a single-trunk slice of the
-- failing value's story with a mid-line citation rail, rendered from the
-- trace and blame IR ("Hegel.Report.Trace", "Hegel.Report.Blame").
--
-- Layout rules pinned by the design note
-- (@notes\/roadmap\/01-stateful-trace-rendering.md@, R3):
--
-- * The revset is the failure's citation closure; everything else is elided,
--   explicitly (@⋯ n steps@ rows, @~@ history terminator, @▸ lifelines
--   elided@ footer) — never silently.
-- * Only the failing step gets drawn rail edges, one column per citation up
--   to the rail budget; overflow falls back to a numeric citation list.
-- * The rail sits mid-line — between the call column and the annotations —
--   so each justification lands at its arrowhead (tenet 6: text strictly
--   right of geometry).
-- * The call column is clipped at a width budget (the accepted cost of the
--   mid-line rail); the full values live in the failure details and splice.
--
-- Layout emits abstract 'Cell's; glyphs are applied last via the
-- 'Glyph.GlyphTable' (tenet 3).
--
-- Designed for qualified import:
--
-- > import Hegel.Report.Ledger qualified as Ledger
module Hegel.Report.Ledger
  ( -- * Row model
    RowKind (..),
    Row (..),
    layoutRows,

    -- * Rendering
    ledgerDoc,
  )
where

import Data.IntSet qualified as IntSet
import Data.List (nub, sortOn)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Report.Ann (Ann (..))
import Hegel.Report.Ann qualified as Ann
import Hegel.Report.Blame (Blame (..), Observation (..))
import Hegel.Report.Blame qualified as Blame
import Hegel.Report.Glyph (Cell (..), GlyphTable (..), displayName)
import Hegel.Report.Phrase (PhraseTable (..))
import Hegel.Report.Phrase qualified as Phrase
import Hegel.Report.Style (Direction (..), Style (..))
import Hegel.Report.Trace (Lifeline (..), Step (..), Touch (..), Trace)
import Hegel.Report.Trace qualified as Trace
import Prettyprinter (Doc)
import Prettyprinter qualified as PP

-- * Options

-- The style record lives in "Hegel.Report.Style"; the ledger is one of its
-- consumers (with the layout knobs that happen to be ledger-specific).

-- * Row model

data RowKind
  = -- | A shown step (the failing step or a cited one).
    NodeRow
  | -- | A dim detail line under the failing step (diff \/ message).
    DetailRow
  | -- | @⋯ n steps@ between two shown steps.
    ElisionRow
  | -- | @~@ — history continues past the view.
    TerminatorRow
  | -- | @▸ k lifelines elided …@.
    FooterRow
  deriving stock (Show, Eq)

-- | One ledger row: abstract cells for the geometry regions, prepared text
-- for the prose regions. Exported for layout pins.
data Row = Row
  { kind :: !RowKind,
    gutter :: !Cell,
    stepNo :: !(Maybe Int),
    call :: !Text,
    rail :: [Cell],
    annot :: !Text
  }
  deriving stock (Show, Eq)

-- * Layout

-- | Lay the trace out as ledger rows. Total: with no citations the ledger is
-- just the failing row and its details.
layoutRows :: Style -> Trace -> Blame -> [Row]
layoutRows opts trace blame = orient <> footerRows
  where
    -- The failing row keeps its detail lines directly beneath it in both
    -- directions; only the history reverses.
    orient = case opts.direction of
      FailureFirst -> failingBlock <> historyRows
      Chronological -> reverse historyRows <> failingBlock

    table = opts.glyphs
    closure = Blame.citationClosure blame
    cited = blame.observed.since
    k = length cited
    drawRail = k > 0 && k <= opts.railBudget
    -- One association: (column, citation), column 1-based in 'since' order
    -- (nearest cause innermost — 'since' is most-recent-first). Everything
    -- rail-related projects from it.
    indexedCites = zip [1 :: Int ..] cited
    columnOf s = listToMaybe [c | (c, o) <- indexedCites, o.step == s]
    -- Shown steps, failure first.
    shown = sortOn (Down . (.index)) [s | s <- trace.steps, IntSet.member s.index closure]
    (failingSteps, citedSteps) = splitAt 1 shown

    failingBlock = concatMap failingRows failingSteps
    failingRows s =
      Row
        { kind = NodeRow,
          gutter = NodeFail,
          stepNo = Just s.index,
          call = callText s,
          rail = if drawRail then originRail else [],
          annot =
            if drawRail || null cited
              then ""
              else numericCites
        }
        : detailRows

    numericCites =
      table.cell NumericCite <> " " <> opts.phrases.cites [T.pack (show c.step) | c <- cited]

    detailRows =
      [ Row {kind = DetailRow, gutter = EdgeAlive, stepNo = Nothing, call = t, rail = detailRail, annot = ""}
      | t <- details
      ]
    -- Failure-first: the details sit between the rail's origin and its
    -- targets below, so the columns pass through. Chronological: the failing
    -- row *terminates* the rail, so rows beneath it carry no rail cells.
    detailRail = case opts.direction of
      FailureFirst -> verticals allColumns
      Chronological -> []
    details = case trace.failure of
      -- One detail row per physical line: an unsplit multi-line message
      -- would defeat the column-width arithmetic.
      Just f -> maybe (T.lines f.message) (fmap diffLine) f.diff
      Nothing -> []
    diffLine = Ann.lineDiffText

    -- Cited steps and the elisions between them, walking back through time.
    historyRows = go (listToMaybe (fmap (.index) failingSteps)) citedSteps
      where
        go _ [] = terminator
        go prev (s : rest) =
          elisionRowsBetween prev s.index
            <> [citedRow s]
            <> go (Just s.index) rest
    terminator =
      [ Row {kind = TerminatorRow, gutter = HistoryEnd, stepNo = Nothing, call = "", rail = [], annot = ""}
      | earliestShown <- take 1 (reverse (fmap (.index) shown)),
        any (\s -> s.index < earliestShown) trace.steps
      ]

    citedRow s =
      Row
        { kind = NodeRow,
          gutter = gutterFor s,
          stepNo = Just s.index,
          call = callText s,
          rail = if drawRail then maybe [] citedRail (columnOf s.index) else [],
          annot = maybe "" factText (listToMaybe [o.fact | (_, o) <- indexedCites, o.step == s.index])
        }
    gutterFor s
      | (rootLife >>= (.bornAt)) == Just s.index = NodeBorn
      -- Death glyphs mean death: a consumption continued by a transfer
      -- renders as a touch (the words at the arrowhead carry the handoff).
      | any (\l -> l.consumedAt == Just s.index && not (Trace.continues trace l.var)) chainLives = NodeDeath
      | otherwise = NodeTouch

    elisionRowsBetween mUpper lower =
      [ Row
          { kind = ElisionRow,
            gutter = EdgeElided,
            stepNo = Nothing,
            call = table.cell Ellipsis <> " " <> elisionLabel between,
            rail = elidedVerticals (activeColumnsBelow lower),
            annot = ""
          }
      | upper <- maybe [] pure mUpper,
        let between = [s | s <- trace.steps, s.index > lower, s.index < upper],
        not (null between)
      ]
    elisionLabel between =
      opts.phrases.elidedSteps
        (length between)
        (if any touchesSubject between then Nothing else Just subjectName)
    touchesSubject s = any (\t -> Trace.root trace t.var == subjectRoot) s.touches

    -- Rail geometry -------------------------------------------------------
    cornerCell = case opts.direction of
      FailureFirst -> RailCornerUp
      Chronological -> RailCornerDown
    originTee = case opts.direction of
      FailureFirst -> RailTeeDown
      Chronological -> RailTeeUp
    originCorner = case opts.direction of
      FailureFirst -> RailCornerDown
      Chronological -> RailCornerUp

    originRail =
      RailOrigin
        : concat [[RailHoriz, if c < k then originTee else originCorner] | c <- [1 .. k]]
    citedRail c =
      [RailArrow]
        <> replicate (2 * c - 1) RailHoriz
        <> [cornerCell]
        <> concat [[Blank, RailVert] | _ <- [c + 1 .. k]]
    verticals = verticalsWith RailVert
    elidedVerticals = verticalsWith RailElided
    verticalsWith cell cols =
      case cols of
        [] -> []
        _ -> Blank : concat [[Blank, if c `elem` cols then cell else Blank] | c <- [1 .. k]]
    allColumns = if drawRail then [1 .. k] else []
    -- Columns still travelling at rows below (further back in time than)
    -- the given step: citations whose row is at or before it.
    activeColumnsBelow lower =
      if drawRail
        then [c | (c, o) <- indexedCites, o.step <= lower]
        else []

    -- Text ----------------------------------------------------------------
    -- A transferred value keeps one identity: names, gutter states, and
    -- "touches the subject" checks all resolve through the lineage root.
    subjectRoot = Trace.root trace blame.subject
    rootLife = Trace.lifeline trace subjectRoot
    chainLives = mapMaybe (Trace.lifeline trace) (Trace.chain trace blame.subject)
    nameOf = displayName table trace
    subjectName = nameOf blame.subject

    callText s = clip (T.unwords (s.rule : touchNames) <> respText)
      where
        touchNames = nub (fmap (nameOf . (.var)) s.touches)
        respText = maybe "" (\r -> " " <> table.cell ResponseArrow <> " " <> Phrase.firstLine r) s.response
    clip t
      | T.length t <= opts.callWidth = t
      | otherwise = T.take (opts.callWidth - T.length ell) t <> ell
      where
        ell = table.cell Ellipsis

    factText fact = opts.phrases.caused fact (nameOf (Blame.factVar fact))

    footerRows =
      [ Row
          { kind = FooterRow,
            gutter = Blank,
            stepNo = Nothing,
            call = "",
            rail = [],
            annot =
              table.cell ElidedMark
                <> " "
                <> opts.phrases.elidedLifelines
                  (length others)
                  (nub (fmap (nameOf . (.var)) others))
                  extraSteps
          }
      | let others = [l | l <- trace.lifelines, Trace.root trace l.var /= subjectRoot],
        not (null others)
      ]
    extraSteps =
      case length [s | s <- trace.steps, s.index > 0, not (IntSet.member s.index closure)] of
        0 -> Nothing
        n -> Just n

-- * Rendering

-- | Render the rows as an aligned document: gutter, step number, clipped
-- call column, the rail region, annotations at the arrowheads.
ledgerDoc :: Style -> Trace -> Blame -> Doc Ann
ledgerDoc opts trace blame = PP.vsep (fmap rowDoc rows)
  where
    rows = layoutRows opts trace blame
    table = opts.glyphs

    stepW = maximum (1 : [length (show i) | Row {stepNo = Just i} <- rows])
    callW = maximum (0 : [T.length r.call | r <- rows, r.kind /= FooterRow])
    railW = maximum (0 : [railWidth r.rail | r <- rows])
    railWidth cells = sum (fmap (T.length . table.cell) cells)

    rowDoc r
      -- The footer is prose, not a ledger line: no ghost columns.
      | r.kind == FooterRow = PP.annotate ElidedAnn (PP.pretty r.annot)
      | otherwise = foldMap snd (dropTrailing segments)
      where
        segments =
          [ (gutterTxt, gutterDoc),
            (" ", " "),
            (stepTxt, PP.annotate StepNoAnn (PP.pretty stepTxt)),
            ("  ", "  "),
            (callPadded, callDoc),
            ("  ", "  "),
            (railPadded, PP.annotate (RailAnn 0) (PP.pretty railPadded)),
            ("   ", "   "),
            (r.annot, annotDoc)
          ]
        gutterTxt = table.cell r.gutter
        gutterDoc = PP.annotate (if r.gutter == NodeFail then FailureMark else LaneAnn 0) (PP.pretty gutterTxt)
        stepTxt = T.justifyRight stepW ' ' (maybe "" (T.pack . show) r.stepNo)
        callPadded = T.justifyLeft callW ' ' r.call
        callDoc = case r.kind of
          DetailRow -> PP.annotate (diffAnn r.call) (PP.pretty callPadded)
          ElisionRow -> PP.annotate ElidedAnn (PP.pretty callPadded)
          _ -> respAnnotated callPadded
        railTxt = foldMap table.cell r.rail
        railPadded = railTxt <> T.replicate (railW - T.length railTxt) " "
        annotDoc = case r.kind of
          FooterRow -> PP.annotate ElidedAnn (PP.pretty r.annot)
          _ -> PP.annotate NoteAnn (PP.pretty r.annot)

    -- Prefix sniffing co-located with its producer ('Ann.lineDiffText');
    -- the full structured fix rides the recorded Row debt (A2).
    diffAnn t
      | "- " `T.isPrefixOf` t = DiffRemoved
      | "+ " `T.isPrefixOf` t = DiffAdded
      | otherwise = ElidedAnn

    -- Colour the response tail separately from the call head.
    respAnnotated t = case T.breakOn (" " <> table.cell ResponseArrow <> " ") t of
      (_, "") -> PP.pretty t
      (call', resp) -> PP.pretty call' <> PP.annotate ResponseAnn (PP.pretty resp)

    -- Drop trailing all-whitespace segments so lines carry no trailing
    -- spaces (they churn golden pins and diffs).
    dropTrailing :: [(Text, Doc Ann)] -> [(Text, Doc Ann)]
    dropTrailing = reverse . dropWhile (T.all (== ' ') . fst) . reverse
