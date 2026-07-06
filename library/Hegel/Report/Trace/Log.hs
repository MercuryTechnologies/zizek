-- | A stateful counterexample rendered as an aligned, chronological event log:
-- oldest step at the top, the failing step at the bottom. One grammar, two
-- 'View's — 'Focused' on a single pool value's story (others elided), and
-- 'Unfocused' with every step shown (no pool value, or several at once). The
-- single-value lifeline is the focused special case of the general log.
--
-- Intended to be imported with qualification:
--
-- > import Hegel.Report.Trace.Log qualified as Log
module Hegel.Report.Trace.Log
  ( -- * View
    View (..),

    -- * Row model
    RowKind (..),
    Row (..),
    layoutRows,

    -- * Rendering
    logDoc,
    elidedLifelinesDoc,
  )
where

import Data.IntSet qualified as IntSet
import Data.List (nub, sort, sortOn)
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Internal.Event (Var)
import Hegel.Report.Ann (Ann (..))
import Hegel.Report.Glyph (Cell (..), GlyphTable (..), displayName)
import Hegel.Report.Note (Note (..), NoteKind (..))
import Hegel.Report.Phrase (PhraseTable (..))
import Hegel.Report.Phrase qualified as Phrase
import Hegel.Report.Style (Style (..))
import Hegel.Report.Trace (Lifeline (..), Step (..), Touch (..), Trace)
import Hegel.Report.Trace qualified as Trace
import Hegel.Report.Trace.Blame (Blame (..), Observation (..))
import Hegel.Report.Trace.Blame qualified as Blame
import Prettyprinter (Doc)
import Prettyprinter qualified as PP

-- | How the event log is laid out:
--
-- * 'Focused' — a single failing pool value's story: steps that don't touch it
--   are elided, off-lineage values go to the footer.
-- * 'Unfocused' — every step shown (no pool subject, or several at once); the
--   optional 'Blame' still supplies gutter glyphs and margins when present.
data View
  = Focused !Blame
  | Unfocused !(Maybe Blame)

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

-- | Lay the trace out as event-log rows, chronological (oldest step first, the
-- failing step last). 'Focused' elides steps that don't touch the subject;
-- 'Unfocused' shows them all. The failing step's diff is not shown here — the
-- composed report renders the source splice (which carries it) after the log.
layoutRows :: Style -> Trace -> View -> [Row]
layoutRows opts trace = \case
  Focused blame -> focusedRows opts trace blame
  Unfocused mBlame -> unfocusedRows opts trace mBlame

-- | The single-subject focused log (today's spine): the terminator (if older
-- history precedes the view) then the subject's steps oldest → newest, others
-- elided, failing step last.
focusedRows :: Style -> Trace -> Blame -> [Row]
focusedRows opts trace blame = terminator <> go Nothing shownAsc
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
      | d <- snd (stepCall opts trace s),
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

    callText s = fst (stepCall opts trace s)

    factText fact = opts.phrases.observed fact (nameOf (Blame.factVar fact))

-- | The unfocused log: every real step shown in order (no single subject to
-- focus on). Used when the failure has no pool value (@Unfocused Nothing@) or
-- touches several at once (@Unfocused (Just blame)@). Gutter glyphs and margins
-- come from the blame when present; without it rows carry a blank gutter and no
-- margin. The prelude (step 0) contributes detail lines only — no call row.
unfocusedRows :: Style -> Trace -> Maybe Blame -> [Row]
unfocusedRows opts trace mBlame = concatMap stepRows (sortOn (.index) trace.steps)
  where
    table = opts.glyphs
    nameOf = displayName table trace
    failingIx = fmap (.step) trace.failure
    cited = maybe [] (\b -> b.observed.since) mBlame

    stepRows s
      | s.index == 0 = detailRows s
      | otherwise = callRow s : detailRows s

    callRow s =
      Row
        { kind = NodeRow,
          gutter = gutterFor s,
          stepNo = Just s.index,
          call = fst (stepCall opts trace s),
          margin = marginFor s
        }

    -- Detail lines: the step's narrative annotations and any free draws that did
    -- not inline, dim, in journal order (annotations follow their draws in a
    -- rule body, so draw-then-annotation reads chronologically).
    detailRows s =
      [ Row {kind = DetailRow, gutter = Blank, stepNo = Nothing, call = l, margin = ""}
      | line <- snd (stepCall opts trace s) <> annotations s,
        l <- T.lines line
      ]
    annotations :: Step -> [Text]
    annotations s = [n.text | n <- s.notes, n.kind == Annotation]

    -- Gutter precedence across all values touched at this step:
    -- ✗ (failing) > death > born > access > blank (no pool touch).
    gutterFor s
      | Just s.index == failingIx = NodeFail
      | any (\t -> consumedHere t s.index) s.touches = NodeDeath
      | any (\t -> bornHere t s.index) s.touches = NodeBorn
      | not (null s.touches) = NodeTouch
      | otherwise = Blank
    bornHere t ix = (Trace.lifeline trace (Trace.root trace t.var) >>= (.bornAt)) == Just ix
    -- Death glyphs mean death: a consumption continued by a transfer is an access.
    consumedHere t ix =
      case Trace.lifeline trace t.var of
        Just l -> l.consumedAt == Just ix && not (Trace.continues trace l.var)
        Nothing -> False

    -- Margins mirror focused mode: cited steps carry their fact, the failing
    -- step carries the @← cites …@ list. No blame → no margins.
    marginFor s
      | Just s.index == failingIx, not (null cited) = numericCites
      | Just fact <- listToMaybe [o.fact | o <- cited, o.step == s.index] = factText fact
      | otherwise = ""
    numericCites =
      table.cell NumericCite <> " " <> opts.phrases.cites (fmap (T.pack . show) (sort (nub [o.step | o <- cited])))
    factText fact = opts.phrases.observed fact (nameOf (Blame.factVar fact))

-- The rendered call plus the free draws that did NOT inline (→ detail rows).
-- Free (non-pool) draws fold into the call in journal order — @write h₁ "0"@ —
-- while each is single-line and the call still fits the width budget; the first
-- that doesn't (and all after it, to keep order) become dim detail rows. Pool
-- references stay symbolic (no inline value). Shared by both views.
stepCall :: Style -> Trace -> Step -> (Text, [Text])
stepCall opts trace s = (clip opts (T.unwords (s.rule : touchNames <> inlined) <> respText), detail)
  where
    table = opts.glyphs
    nameOf = displayName table trace
    touchNames = nub (fmap (nameOf . (.var)) s.touches)
    respText = maybe "" (\r -> " " <> table.cell ResponseArrow <> " " <> Phrase.firstLine r) s.response
    (inlined, detail) = inlineDraws opts (T.unwords (s.rule : touchNames)) respText s.freeDraws

inlineDraws :: Style -> Text -> Text -> [Text] -> ([Text], [Text])
inlineDraws opts headText respTail = fit headText
  where
    budget = opts.callWidth - T.length respTail
    fit _ [] = ([], [])
    fit acc (d : ds)
      | not ("\n" `T.isInfixOf` d),
        T.length acc' <= budget =
          let (i, l) = fit acc' ds in (d : i, l)
      | otherwise = ([], d : ds)
      where
        acc' = acc <> " " <> d

clip :: Style -> Text -> Text
clip opts t
  | T.length t <= opts.callWidth = t
  | otherwise = T.take (opts.callWidth - T.length ell) t <> ell
  where
    ell = opts.glyphs.cell Ellipsis

-- | Every step a value's lineage (across 'Hegel.Pool.transfer') touched — its
-- 'Born', 'Reused', and 'Consumed' steps — paired with the rule name, distinct
-- and chronological, excluding the prelude (step 0 is machine setup, not a rule
-- the reader can navigate to). Backs the elided-lifeline footer.
trajectorySteps :: Trace -> Var -> [(Int, Text)]
trajectorySteps trace v =
  [(i, s.rule) | i <- stepIxs, Just s <- [Trace.step trace i]]
  where
    stepIxs =
      IntSet.toAscList . IntSet.fromList $
        [ i
        | l <- Trace.chainLifelines trace v,
          i <- maybe [] pure l.bornAt <> l.touchedAt <> maybe [] pure l.consumedAt,
          i > 0
        ]

-- | A value's history as a single line — @name: rule \@i · rule \@i@ — or
-- 'Nothing' when its lineage touched no navigable step. Used per elided
-- lifeline in the footer.
trajectory :: Style -> Trace -> Var -> Maybe Text
trajectory opts trace v = case trajectorySteps trace v of
  [] -> Nothing
  entries ->
    Just (opts.phrases.trajectory (displayName opts.glyphs trace v) [(r, T.pack (show i)) | (i, r) <- entries])

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
    leads = [(r, t) | r <- elidedRoots, Just t <- [trajectory opts trace r]]
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
logDoc :: Style -> Trace -> View -> Doc Ann
logDoc opts trace view = PP.vsep (fmap rowDoc rows)
  where
    rows = layoutRows opts trace view
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
        -- Pad the call to the column width only when a margin follows it;
        -- otherwise the padding is dead trailing whitespace (dropTrailing can't
        -- strip it — the segment isn't all-whitespace).
        callPadded
          | T.null r.margin = r.call
          | otherwise = T.justifyLeft callW ' ' r.call
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
