-- | A failing value's history rendered as an aligned vertical reference.
--
-- Intended to be imported with qualification:
--
-- > import Hegel.Report.Trace.Spine qualified as Spine
module Hegel.Report.Trace.Spine
  ( -- * Row model
    RowKind (..),
    Row (..),
    layoutRows,

    -- * Rendering
    spineDoc,
  )
where

import Data.IntSet qualified as IntSet
import Data.List (nub, sortOn)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Report.Ann (Ann (..))
import Hegel.Report.Ann qualified as Ann
import Hegel.Report.Glyph (Cell (..), GlyphTable (..), displayName)
import Hegel.Report.Phrase (PhraseTable (..))
import Hegel.Report.Phrase qualified as Phrase
import Hegel.Report.Style (LinkMode (..), Style (..))
import Hegel.Report.Trace (Lifeline (..), Step (..), Touch (..), Trace)
import Hegel.Report.Trace qualified as Trace
import Hegel.Report.Trace.Blame (Blame (..), Observation (..))
import Hegel.Report.Trace.Blame qualified as Blame
import Hegel.Report.Trace.Lead qualified as Lead
import Prettyprinter (Doc)
import Prettyprinter qualified as PP

data RowKind
  = -- | A failing or otherwise cited step.
    NodeRow
  | -- | A dim detail line under the failing step.
    DetailRow
  | -- | @⋯ n steps@ elided between two rendered steps.
    ElisionRow
  | -- | @~@ — history continues past the view.
    TerminatorRow
  | -- | @▸ k lifelines elided …@.
    FooterRow
  deriving stock (Show, Eq)

-- | One row of the spine, containing abstract cells for the geometry regions
-- and prepared text for the prose regions.
data Row = Row
  { kind :: !RowKind,
    gutter :: !Cell,
    stepNo :: !(Maybe Int),
    call :: !Text,
    link :: [Cell],
    annot :: !Text,
    detailAnn :: !(Maybe Ann)
  }
  deriving stock (Show, Eq)

-- * Layout

-- | Lay the trace out as spine rows.
layoutRows :: Style -> Trace -> Blame -> [Row]
layoutRows opts trace blame = orient <> footerRows
  where
    -- Failure-first: the failing row (with its detail lines) sits at the top,
    -- history reading back through time beneath it.
    orient = failingBlock <> historyRows

    table = opts.glyphs
    closure = Blame.citationClosure blame
    cited = blame.observed.since
    k = length cited
    linkWanted = case opts.linkMode of
      Links -> True
      Numeric -> False
    drawLink = linkWanted && k > 0 && k <= opts.linkBudget
    -- One association, everything link-related projects from it.
    indexedCites = zip [1 :: Int ..] cited
    columnOf s = listToMaybe [c | (c, o) <- indexedCites, o.step == s]
    shown = sortOn (Down . (.index)) [s | s <- trace.steps, IntSet.member s.index closure]
    (failingSteps, citedSteps) = splitAt 1 shown

    failingBlock = concatMap failingRows failingSteps
    failingRows s =
      Row
        { kind = NodeRow,
          gutter = NodeFail,
          stepNo = Just s.index,
          call = callText s,
          link = if drawLink then originLink else [],
          annot =
            if drawLink || null cited
              then ""
              else numericCites,
          detailAnn = Nothing
        }
        : detailRows

    numericCites =
      table.cell NumericCite <> " " <> opts.phrases.cites [T.pack (show c.step) | c <- cited]

    detailRows =
      [ Row {kind = DetailRow, gutter = EdgeAlive, stepNo = Nothing, call = t, link = detailLink, annot = "", detailAnn = mAnn}
      | (t, mAnn) <- details
      ]
    -- The details sit between the link's origin and its targets below, so the
    -- columns pass through.
    detailLink = verticals allColumns
    -- Each detail line as (rendered text, structured diff annotation).
    --
    -- A diff carries its 'Ann' structurally; a plain message carries none.
    details :: [(Text, Maybe Ann)]
    details = case trace.failure of
      -- One detail row per physical line: an unsplit multi-line message
      -- would defeat the column-width arithmetic.
      Just f -> maybe [(l, Nothing) | l <- T.lines f.message] (fmap diffLine) f.diff
      Nothing -> []
    diffLine d = (Ann.lineDiffText d, Just (Ann.lineDiffAnn d))

    -- Cited steps and the elisions between them, walking back through time.
    historyRows = go (listToMaybe (fmap (.index) failingSteps)) citedSteps
      where
        go _ [] = terminator
        go prev (s : rest) =
          elisionRowsBetween prev s.index
            <> [citedRow s]
            <> drawnRows s
            <> go (Just s.index) rest
    terminator =
      [ Row {kind = TerminatorRow, gutter = HistoryEnd, stepNo = Nothing, call = "", link = [], annot = "", detailAnn = Nothing}
      | earliestShown <- take 1 (reverse (fmap (.index) shown)),
        any (\s -> s.index < earliestShown) trace.steps
      ]

    citedRow s =
      Row
        { kind = NodeRow,
          gutter = gutterFor s,
          stepNo = Just s.index,
          call = callText s,
          link = if drawLink then maybe [] citedLink (columnOf s.index) else [],
          annot = maybe "" factText (listToMaybe [o.fact | (_, o) <- indexedCites, o.step == s.index]),
          detailAnn = Nothing
        }
    drawnRows s =
      [ Row
          { kind = DetailRow,
            gutter = EdgeAlive,
            stepNo = Nothing,
            call = l,
            link = if drawLink then verticalsWith LinkVertical [c | (c, o) <- indexedCites, o.step < s.index] else [],
            annot = "",
            detailAnn = Nothing
          }
      | d <- s.freeDraws,
        l <- T.lines d
      ]

    gutterFor s
      | (rootLife >>= (.bornAt)) == Just s.index = NodeBorn
      -- Death glyphs mean death: a consumption continued by a transfer
      -- renders as an access.
      | any (\l -> l.consumedAt == Just s.index && not (Trace.continues trace l.var)) chainLives = NodeDeath
      | otherwise = NodeTouch

    elisionRowsBetween mUpper lower =
      [ Row
          { kind = ElisionRow,
            gutter = EdgeElided,
            stepNo = Nothing,
            call = table.cell Ellipsis <> " " <> elisionLabel between,
            link = elidedVerticals (activeColumnsBelow lower),
            annot = "",
            detailAnn = Nothing
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

    cornerCell = LinkCornerUp
    originTee = LinkTeeDown
    originCorner = LinkCornerDown

    originLink =
      LinkOrigin
        : concat [[LinkHorizontal, if c < k then originTee else originCorner] | c <- [1 .. k]]
    citedLink c =
      [LinkArrow]
        <> replicate (2 * c - 1) LinkHorizontal
        <> [cornerCell]
        <> concat [[Blank, LinkVertical] | _ <- [c + 1 .. k]]
    verticals = verticalsWith LinkVertical
    elidedVerticals = verticalsWith LinkElided
    verticalsWith cell cols =
      case cols of
        [] -> []
        _ -> Blank : concat [[Blank, if c `elem` cols then cell else Blank] | c <- [1 .. k]]
    allColumns = if drawLink then [1 .. k] else []
    activeColumnsBelow lower =
      if drawLink
        then [c | (c, o) <- indexedCites, o.step <= lower]
        else []

    subjectRoot = Trace.root trace blame.subject
    rootLife = Trace.lifeline trace subjectRoot
    chainLives = Trace.chainLifelines trace blame.subject
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

    factText fact = opts.phrases.observed fact (nameOf (Blame.factVar fact))

    -- Each lifeline the spine elided (a value whose root differs from the
    -- subject's) gets its own compact trajectory — @▸ h₂: open \@3@ — rather
    -- than a bare count, so the reader sees what the off-spine values did. Cap
    -- the list; the overflow collapses back to the counted summary.
    footerRows = fmap footerRow (leadLines <> overflowLines)
      where
        footerCap = 3 :: Int
        mark = table.cell ElidedMark
        -- Dedupe by lineage root: a transferred value is several lifelines but
        -- one story, and would otherwise be counted (and led) more than once.
        elidedRoots =
          nub [Trace.root trace l.var | l <- trace.lifelines, Trace.root trace l.var /= subjectRoot]
        leads = [(r, t) | r <- elidedRoots, Just t <- [Lead.trajectory opts trace r]]
        (shownLeads, hiddenLeads) = splitAt footerCap leads
        leadLines = [mark <> " " <> t | (_, t) <- shownLeads]
        overflowLines =
          [ mark <> " " <> opts.phrases.elidedLifelines (length hiddenLeads) (fmap (nameOf . fst) hiddenLeads) extraSteps
          | not (null hiddenLeads)
          ]
        footerRow annot =
          Row {kind = FooterRow, gutter = Blank, stepNo = Nothing, call = "", link = [], annot, detailAnn = Nothing}
    extraSteps =
      case length [s | s <- trace.steps, s.index > 0, not (IntSet.member s.index closure)] of
        0 -> Nothing
        n -> Just n

-- * Rendering

-- | Render the rows as an aligned document.
spineDoc :: Style -> Trace -> Blame -> Doc Ann
spineDoc opts trace blame = PP.vsep (fmap rowDoc rows)
  where
    rows = layoutRows opts trace blame
    table = opts.glyphs

    stepW = maximum (1 : [length (show i) | Row {stepNo = Just i} <- rows])
    callW = maximum (0 : [T.length r.call | r <- rows, r.kind /= FooterRow])
    linkW = maximum (0 : [linkWidth r.link | r <- rows])
    linkWidth cells = sum (fmap (T.length . table.cell) cells)

    rowDoc r
      -- The footer is prose, not a spine line: no ghost columns.
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
            (linkPadded, PP.annotate (LinkAnn 0) (PP.pretty linkPadded)),
            ("   ", "   "),
            (r.annot, annotDoc)
          ]
        gutterTxt = table.cell r.gutter
        gutterDoc = PP.annotate (if r.gutter == NodeFail then FailureMark else StrandAnn 0) (PP.pretty gutterTxt)
        stepTxt = T.justifyRight stepW ' ' (maybe "" (T.pack . show) r.stepNo)
        callPadded = T.justifyLeft callW ' ' r.call
        callDoc = case r.kind of
          -- A diff detail row carries its structured 'Ann'; a plain message
          -- detail line has none and renders dim.
          DetailRow -> PP.annotate (fromMaybe ElidedAnn r.detailAnn) (PP.pretty callPadded)
          ElisionRow -> PP.annotate ElidedAnn (PP.pretty callPadded)
          _ -> respAnnotated callPadded
        linkTxt = foldMap table.cell r.link
        linkPadded = linkTxt <> T.replicate (linkW - T.length linkTxt) " "
        annotDoc = case r.kind of
          FooterRow -> PP.annotate ElidedAnn (PP.pretty r.annot)
          _ -> PP.annotate NoteAnn (PP.pretty r.annot)

    -- Colour the response tail separately from the call head.
    respAnnotated t = case T.breakOn (" " <> table.cell ResponseArrow <> " ") t of
      (_, "") -> PP.pretty t
      (call', resp) -> PP.pretty call' <> PP.annotate ResponseAnn (PP.pretty resp)

    -- Drop trailing all-whitespace segments so lines carry no trailing
    -- spaces (they churn golden pins and diffs).
    dropTrailing :: [(Text, Doc Ann)] -> [(Text, Doc Ann)]
    dropTrailing = reverse . dropWhile (T.all (== ' ') . fst) . reverse
