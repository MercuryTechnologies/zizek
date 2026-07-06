-- | A failing value's history rendered as an aligned vertical spine, oldest
-- step at the top and the failing step at the bottom — chronological, matching
-- the timeline form.
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
    elidedLifelinesDoc,
  )
where

import Data.IntSet qualified as IntSet
import Data.List (nub, sort, sortOn)
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Report.Ann (Ann (..))
import Hegel.Report.Glyph (Cell (..), GlyphTable (..), displayName)
import Hegel.Report.Phrase (PhraseTable (..))
import Hegel.Report.Phrase qualified as Phrase
import Hegel.Report.Style (Style (..))
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
  | -- | A dim detail line under a step (a free draw).
    DetailRow
  | -- | @⋯ n steps@ elided between two rendered steps.
    ElisionRow
  | -- | @~@ — older history precedes the view.
    TerminatorRow
  deriving stock (Show, Eq)

-- | One row of the spine: the gutter glyph, the step number, the call text, and
-- the right-margin gloss.
data Row = Row
  { kind :: !RowKind,
    gutter :: !Cell,
    stepNo :: !(Maybe Int),
    call :: !Text,
    -- | The row's right-margin text: the blame fact on a cited row, the
    -- @← cites …@ list on the failing row.
    margin :: !Text
  }
  deriving stock (Show, Eq)

-- * Layout

-- | Lay the trace out as spine rows, chronological: the terminator (if older
-- history precedes the view) then the shown steps oldest → newest, with the
-- failing step last. The failing step's diff is not shown here — the composed
-- report renders the source splice (which carries it) after the spine.
layoutRows :: Style -> Trace -> Blame -> [Row]
layoutRows opts trace blame = terminator <> go Nothing shownAsc
  where
    table = opts.glyphs
    closure = Blame.citationClosure blame
    cited = blame.observed.since
    -- Shown steps oldest → newest; the failing step has the largest index.
    shownAsc = sortOn (.index) [s | s <- trace.steps, IntSet.member s.index closure]
    failingIx = listToMaybe (sortOn Down (fmap (.index) shownAsc))

    -- Walk oldest → newest, inserting elision rows for the gaps between shown
    -- steps.
    go _ [] = []
    go prev (s : rest) =
      elisionBetween prev s.index
        <> stepBlock s
        <> go (Just s.index) rest
    stepBlock s
      | Just s.index == failingIx = [failingRow s]
      | otherwise = citedRow s : drawnRows s

    -- @~@ at the top when real steps (not the machine-setup prelude, step 0)
    -- older than the oldest shown step exist — i.e. the spine begins partway
    -- through the run because earlier steps don't concern the failing value.
    terminator =
      [ Row {kind = TerminatorRow, gutter = HistoryEnd, stepNo = Nothing, call = "", margin = ""}
      | oldestShown <- take 1 (fmap (.index) shownAsc),
        any (\s -> s.index > 0 && s.index < oldestShown) trace.steps
      ]

    failingRow s =
      Row
        { kind = NodeRow,
          gutter = NodeFail,
          stepNo = Just s.index,
          call = callText s,
          margin = if null cited then "" else numericCites
        }
    -- Cited-step numbers, ascending to match the top → bottom reading order.
    numericCites =
      table.cell NumericCite <> " " <> opts.phrases.cites (fmap (T.pack . show) (sort (nub [o.step | o <- cited])))

    citedRow s =
      Row
        { kind = NodeRow,
          gutter = gutterFor s,
          stepNo = Just s.index,
          call = callText s,
          margin = maybe "" factText (listToMaybe [o.fact | o <- cited, o.step == s.index])
        }
    drawnRows :: Step -> [Row]
    drawnRows s =
      [ Row {kind = DetailRow, gutter = EdgeAlive, stepNo = Nothing, call = l, margin = ""}
      | d <- s.freeDraws,
        l <- T.lines d
      ]

    gutterFor s
      | (rootLife >>= (.bornAt)) == Just s.index = NodeBorn
      -- Death glyphs mean death: a consumption continued by a transfer
      -- renders as an access.
      | any (\l -> l.consumedAt == Just s.index && not (Trace.continues trace l.var)) chainLives = NodeDeath
      | otherwise = NodeTouch

    -- Elision rows for steps strictly between the previous shown step and this
    -- one.
    elisionBetween mLo hi =
      [ Row
          { kind = ElisionRow,
            gutter = EdgeElided,
            stepNo = Nothing,
            call = table.cell Ellipsis <> " " <> elisionLabel between,
            margin = ""
          }
      | lo <- maybe [] pure mLo,
        let between = [s | s <- trace.steps, s.index > lo, s.index < hi],
        not (null between)
      ]
    elisionLabel between =
      opts.phrases.elidedSteps
        (length between)
        (if any touchesSubject between then Nothing else Just subjectName)
    touchesSubject s = any (\t -> Trace.root trace t.var == subjectRoot) s.touches

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

-- | The off-spine lifelines: each pool value whose lineage root differs from
-- the failing value's, as a compact trajectory (@▸ h₂: open \@3@). Rendered as
-- its own section after the splice in the composed report; 'Nothing' when the
-- counterexample has no off-spine values. Capped, with a counted overflow.
elidedLifelinesDoc :: Style -> Trace -> Blame -> Maybe (Doc Ann)
elidedLifelinesDoc opts trace blame
  | null allLines = Nothing
  | otherwise = Just (PP.vsep (fmap (PP.annotate ElidedAnn . PP.pretty) allLines))
  where
    table = opts.glyphs
    footerCap = 3 :: Int
    mark = table.cell ElidedMark
    subjectRoot = Trace.root trace blame.subject
    nameOf = displayName table trace
    closure = Blame.citationClosure blame
    -- Dedupe by lineage root: a transferred value is several lifelines but one
    -- story, and would otherwise be led more than once.
    elidedRoots =
      nub [Trace.root trace l.var | l <- trace.lifelines, Trace.root trace l.var /= subjectRoot]
    leads = [(r, t) | r <- elidedRoots, Just t <- [Lead.trajectory opts trace r]]
    (shownLeads, hiddenLeads) = splitAt footerCap leads
    leadLines = [mark <> " " <> t | (_, t) <- shownLeads]
    overflowLines =
      [ mark <> " " <> opts.phrases.elidedLifelines (length hiddenLeads) (fmap (nameOf . fst) hiddenLeads) extraSteps
      | not (null hiddenLeads)
      ]
    extraSteps =
      case length [s | s <- trace.steps, s.index > 0, not (IntSet.member s.index closure)] of
        0 -> Nothing
        n -> Just n
    allLines = leadLines <> overflowLines

-- * Rendering

-- | Render the rows as an aligned document.
spineDoc :: Style -> Trace -> Blame -> Doc Ann
spineDoc opts trace blame = PP.vsep (fmap rowDoc rows)
  where
    rows = layoutRows opts trace blame
    table = opts.glyphs

    stepW = maximum (1 : [length (show i) | Row {stepNo = Just i} <- rows])
    callW = maximum (0 : [T.length r.call | r <- rows])

    rowDoc r = foldMap snd (dropTrailing segments)
      where
        segments =
          [ (gutterTxt, gutterDoc),
            (" ", " "),
            (stepTxt, PP.annotate StepNoAnn (PP.pretty stepTxt)),
            ("  ", "  "),
            (callPadded, callDoc),
            ("   ", "   "),
            (r.margin, PP.annotate NoteAnn (PP.pretty r.margin))
          ]
        gutterTxt = table.cell r.gutter
        gutterDoc = PP.annotate (if r.gutter == NodeFail then FailureMark else StrandAnn 0) (PP.pretty gutterTxt)
        stepTxt = T.justifyRight stepW ' ' (maybe "" (T.pack . show) r.stepNo)
        callPadded = T.justifyLeft callW ' ' r.call
        callDoc = case r.kind of
          -- Detail and elision rows are supporting context, rendered dim.
          DetailRow -> PP.annotate ElidedAnn (PP.pretty callPadded)
          ElisionRow -> PP.annotate ElidedAnn (PP.pretty callPadded)
          _ -> respAnnotated callPadded

    -- Colour the response tail separately from the call head.
    respAnnotated t = case T.breakOn (" " <> table.cell ResponseArrow <> " ") t of
      (_, "") -> PP.pretty t
      (call', resp) -> PP.pretty call' <> PP.annotate ResponseAnn (PP.pretty resp)

    -- Drop trailing all-whitespace segments so lines carry no trailing
    -- spaces (they churn golden pins and diffs).
    dropTrailing :: [(Text, Doc Ann)] -> [(Text, Doc Ann)]
    dropTrailing = reverse . dropWhile (T.all (== ' ') . fst) . reverse
