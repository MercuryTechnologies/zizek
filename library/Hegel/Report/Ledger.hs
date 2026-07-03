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
  ( -- * Options
    Direction (..),
    Options (..),
    defaultOptions,

    -- * Row model
    RowKind (..),
    Row (..),
    layoutRows,

    -- * Rendering
    ledgerDoc,
  )
where

import Data.IntSet qualified as IntSet
import Data.List (nub, sortOn)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Diff (LineDiff (..))
import Hegel.Internal.Event (Var (..))
import Hegel.Report.Ann (Ann (..))
import Hegel.Report.Blame (Blame (..), Fact (..), Observation (..))
import Hegel.Report.Blame qualified as Blame
import Hegel.Report.Glyph (Cell (..), GlyphTable (..))
import Hegel.Report.Trace (Lifeline (..), Step (..), Touch (..), Trace)
import Hegel.Report.Trace qualified as Trace
import Prettyprinter (Doc)
import Prettyprinter qualified as PP

-- * Options

-- | Reading order. 'FailureFirst' puts the failure at eye level (jj's
-- @\@@-at-top instinct) with relevance decaying downward into elided
-- history; 'Chronological' reads as a story and matches the plain
-- renderer's order. The default is decided on live gallery traces.
data Direction = FailureFirst | Chronological
  deriving stock (Show, Eq)

data Options = Options
  { glyphs :: !GlyphTable,
    direction :: !Direction,
    -- | Maximum drawn rail columns; more citations than this fall back to
    -- the numeric citation list on the failing row.
    railBudget :: !Int,
    -- | Call-column clip budget (@rule args → response@), in characters.
    callWidth :: !Int
  }

defaultOptions :: GlyphTable -> Options
defaultOptions table =
  Options {glyphs = table, direction = FailureFirst, railBudget = 3, callWidth = 40}

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
layoutRows :: Options -> Trace -> Blame -> [Row]
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
    -- Citation column, 1-based: position in 'since' order (nearest cause
    -- innermost — 'since' is most-recent-first).
    columnOf s = lookup s (zip (fmap (.step) cited) [1 ..])
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
      table.cell NumericCite <> " cites " <> T.intercalate ", " [T.pack (show c.step) | c <- cited]

    detailRows =
      [ Row {kind = DetailRow, gutter = EdgeAlive, stepNo = Nothing, call = t, rail = verticals allColumns, annot = ""}
      | t <- details
      ]
    details = case trace.failure of
      Just f -> maybe [f.message] (fmap diffLine) f.diff
      Nothing -> []
    diffLine = \case
      LineSame t -> "  " <> t
      LineRemoved t -> "- " <> t
      LineAdded t -> "+ " <> t

    -- Cited steps and the elisions between them, walking back through time.
    historyRows = go (fmap (.index) failingSteps) citedSteps
      where
        go _ [] = terminator
        go prev (s : rest) =
          elisionRowsBetween (listToMaybe' prev) s.index
            <> [citedRow s]
            <> go [s.index] rest
        listToMaybe' = \case
          [] -> Nothing
          (x : _) -> Just x
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
          rail = if drawRail then citedRail (fromMaybe 1 (columnOf s.index)) else [],
          annot = maybe "" factText (lookup s.index [(c.step, c.fact) | c <- cited])
        }
    gutterFor s
      | (rootLife >>= (.bornAt)) == Just s.index = NodeBorn
      | Just s.index `elem` fmap (.consumedAt) chainLives = NodeDeath
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
      T.pack (show (length between))
        <> (if length between == 1 then " step" else " steps")
        <> if any (touchesSubject) between
          then ""
          else ", none touch " <> subjectName
    touchesSubject s = any (\t -> Trace.root trace t.var == subjectRoot) s.touches

    -- Rail geometry -------------------------------------------------------
    (teeCell, cornerCell) = case opts.direction of
      FailureFirst -> (RailTeeDown, RailCornerUp)
      Chronological -> (RailTeeUp, RailCornerDown)
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
    verticals cols =
      case cols of
        [] -> []
        _ -> Blank : concat [[Blank, if c `elem` cols then RailVert else Blank] | c <- [1 .. k]]
    elidedVerticals cols =
      case cols of
        [] -> []
        _ -> Blank : concat [[Blank, if c `elem` cols then RailElided else Blank] | c <- [1 .. k]]
    allColumns = if drawRail then [1 .. k] else []
    -- Columns still travelling at rows below (further back in time than)
    -- the given step: citations whose row is at or before it.
    activeColumnsBelow lower =
      if drawRail
        then [c | (c, s) <- zip [1 ..] (fmap (.step) cited), s <= lower]
        else []

    -- Text ----------------------------------------------------------------
    -- A transferred value keeps one identity: names, gutter states, and
    -- "touches the subject" checks all resolve through the lineage root.
    subjectRoot = Trace.root trace blame.subject
    rootLife = Trace.lifeline trace subjectRoot
    chainLives = mapMaybe (Trace.lifeline trace) (Trace.chain trace blame.subject)
    poolOrdinals = nub [l.var.pool | l <- trace.lifelines]
    nameOf v =
      let r = Trace.root trace v
          life = Trace.lifeline trace r
          poolOrd = fromMaybe 0 (lookup r.pool (zip poolOrdinals [0 ..]))
       in table.valueName (life >>= (.label)) poolOrd (maybe 0 (.ordinal) life)
    subjectName = nameOf blame.subject

    callText s = clip (T.unwords (s.rule : touchNames) <> respText)
      where
        touchNames = nub (fmap (nameOf . (.var)) s.touches)
        respText = maybe "" (\r -> " " <> table.cell ResponseArrow <> " " <> firstLine r) s.response
        firstLine = T.takeWhile (/= '\n')
    clip t
      | T.length t <= opts.callWidth = t
      | otherwise = T.take (opts.callWidth - 1) t <> table.cell Ellipsis

    factText = \case
      BornAt v -> nameOf v <> " born here"
      TouchedAt v -> "touched " <> nameOf v
      ConsumedAt v -> "consumed " <> nameOf v
      HauntedAt v -> "touched " <> nameOf v <> " after its death"

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
                <> T.pack (show (length others))
                <> (if length others == 1 then " lifeline" else " lifelines")
                <> " elided ("
                <> T.unwords (nub (fmap (nameOf . (.var)) others))
                <> extraSteps
                <> ")"
          }
      | let others = [l | l <- trace.lifelines, Trace.root trace l.var /= subjectRoot],
        not (null others)
      ]
    extraSteps =
      case length [s | s <- trace.steps, s.index > 0, not (IntSet.member s.index closure)] of
        0 -> ""
        n -> " · " <> T.pack (show n) <> (if n == 1 then " step" else " steps")

-- * Rendering

-- | Render the rows as an aligned document: gutter, step number, clipped
-- call column, the rail region, annotations at the arrowheads.
ledgerDoc :: Options -> Trace -> Blame -> Doc Ann
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
