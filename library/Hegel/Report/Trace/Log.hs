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

import Data.Function (on)
import Data.IntSet qualified as IntSet
import Data.List (groupBy, nub, sort, sortOn)
import Data.List.NonEmpty qualified as NE
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
import Hegel.Report.Trace.Blame (Blame (..), Claim (..), Fact (..), Observation (..))
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
  | -- | @↳ cites …@ — the failing step's citations, on their own row below it.
    CiteRow
  deriving stock (Show, Eq)

-- | One row of the event log: the gutter glyph, the step number, the call text, and
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

-- | The single-subject focused log, the K=1 special case: the terminator (if older
-- history precedes the view) then the subject's steps oldest → newest, others
-- elided, failing step last.
focusedRows :: Style -> Trace -> Blame -> [Row]
focusedRows opts trace blame = terminator <> go Nothing shownAsc
  where
    table = opts.glyphs
    closure = Blame.citationClosure blame
    -- Focused is only reached at K=1 (one lineage root), so a single claim.
    cited = (NE.head blame.subjects).since
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
      -- The subject's birth in machine setup (step 0) is not a navigable rule:
      -- render it as a de-numbered origin line (@●  v₁ initialized@) rather than
      -- a @<initial>@ pseudo-step with a redundant @… created@ margin.
      | s.index == 0 = [originRow]
      | otherwise = citedRow s : drawnRows s

    -- @~@ at the top when real steps (not the machine-setup prelude, step 0)
    -- older than the oldest shown step exist — i.e. the event log begins partway
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
          -- Focused shows exactly the subject's cited steps as visible rows, so a
          -- @← cites@ list would only restate the lifeline. Suppressed.
          margin = ""
        }

    -- The de-numbered origin line for a value born in machine setup.
    originRow =
      Row
        { kind = NodeRow,
          gutter = NodeBorn,
          stepNo = Nothing,
          call = opts.phrases.origin subjectName,
          margin = ""
        }

    citedRow s =
      Row
        { kind = NodeRow,
          gutter = gutterFor s,
          stepNo = Just s.index,
          call = callText s,
          margin = factsDoc opts trace (callText s) [o.fact | o <- cited, o.step == s.index]
        }
    -- Detail lines for a focused row: free draws only. A shown step's narrative
    -- annotations are not repeated here — in focused mode the failing step's
    -- annotations are spliced into source, and the other shown steps are the
    -- subject's terse lifeline. (Unfocused 'detailRows' does carry annotations.)
    drawnRows :: Step -> [Row]
    drawnRows s =
      [ Row {kind = DetailRow, gutter = Blank, stepNo = Nothing, call = l, margin = ""}
      | d <- snd (stepCall opts trace s),
        l <- T.lines d
      ]

    gutterFor s
      | (rootLife >>= (.bornAt)) == Just s.index = NodeBorn
      | any ((== Just NodeDeath) . consumeGlyph trace s.index) chainLives = NodeDeath
      | any ((== Just NodeTransfer) . consumeGlyph trace s.index) chainLives = NodeTransfer
      | otherwise = NodeTouch

    -- Elision rows for steps strictly between the previous shown step and this
    -- one.
    elisionBetween mLo hi =
      [ Row
          { kind = ElisionRow,
            gutter = Gap,
            stepNo = Nothing,
            call = elisionLabel between,
            margin = ""
          }
      | lo <- maybe [] pure mLo,
        let between = [s | s <- trace.steps, s.index > lo, s.index < hi],
        not (null between)
      ]
    -- Name the value(s) the elided run concerns — a positive qualifier (what is
    -- hidden), not the redundant "none touch <subject>". The subject can't appear
    -- (a step touching it would be shown, not elided); Nothing for a non-pool run.
    elisionLabel between =
      opts.phrases.elidedSteps (length between) concerns
      where
        concerns =
          case filter (/= subjectRoot) (nub [Trace.root trace t.var | s <- between, t <- s.touches]) of
            [] -> Nothing
            roots -> Just (T.intercalate ", " (fmap nameOf roots))

    subject = Blame.primary blame
    subjectRoot = Trace.root trace subject
    rootLife = Trace.lifeline trace subjectRoot
    chainLives = Trace.chainLifelines trace subject
    nameOf = displayName table trace
    subjectName = nameOf subject

    callText s = fst (stepCall opts trace s)

-- | The unfocused log: every real step shown in order (no single subject to
-- focus on). Used when the failure has no pool value (@Unfocused Nothing@) or
-- touches several at once (@Unfocused (Just blame)@). Gutter glyphs and margins
-- come from the blame when present; without it rows carry a blank gutter and no
-- margin. The prelude (step 0) contributes detail lines only — no call row.
unfocusedRows :: Style -> Trace -> Maybe Blame -> [Row]
unfocusedRows opts trace mBlame = concatMap stepRows (sortOn (.index) trace.steps)
  where
    table = opts.glyphs
    failingIx = fmap (.step) trace.failure
    -- Every implicated value's story (one claim per root touched at the failing
    -- step); empty when the failure has no pool value.
    claims = maybe [] (NE.toList . (.subjects)) mBlame

    stepRows s
      | s.index == 0 = detailRows s
      | Just s.index == failingIx = callRow s : citeRows s <> detailRows s
      | otherwise = callRow s : detailRows s

    -- The failing step's cited-step references, on their own ↳ row below it
    -- (unfocused shows every step, so the citation selects the evidence among
    -- them). Empty when the failure has no pool value.
    citeRows s =
      [ Row
          { kind = CiteRow,
            gutter = Blank,
            stepNo = Nothing,
            call = table.cell CiteLead <> " " <> body,
            margin = ""
          }
      | not (null claims),
        Just body <- [citesBody opts trace s.index citedSteps]
      ]

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
      | any ((== Just NodeDeath) . touchGlyph s.index) s.touches = NodeDeath
      | any (\t -> bornHere t s.index) s.touches = NodeBorn
      | any ((== Just NodeTransfer) . touchGlyph s.index) s.touches = NodeTransfer
      | not (null s.touches) = NodeTouch
      | otherwise = Blank
    bornHere t ix = (Trace.lifeline trace (Trace.root trace t.var) >>= (.bornAt)) == Just ix
    -- Classify a touch's consume at this step (death vs handoff) via its lifeline.
    touchGlyph ix t = Trace.lifeline trace t.var >>= consumeGlyph trace ix

    -- Margins mirror focused mode, unioned across claims: each step carries the
    -- fact(s) of the value(s) implicated there; the failing step carries the
    -- @← cites …@ list over every claim. No blame → no margins.
    marginFor s
      -- The failing step's citation moves to its own ↳ row (see 'citeRows'); the
      -- margin here carries only cross-value facts, if any.
      | Just s.index == failingIx = ""
      | otherwise = factsDoc opts trace (fst (stepCall opts trace s)) [o.fact | c <- claims, o <- c.since, o.step == s.index]
    citedSteps = [o.step | c <- claims, o <- c.since]

-- | A step reference for the log: real steps read @\@N@; the machine-setup
-- pseudo-step (0) reads @setup@ (it is not a navigable rule). Shared by the
-- step-number column and the citation margins so every step reference agrees.
citeToken :: Int -> Text
citeToken 0 = "setup"
citeToken n = "@" <> T.pack (show n)

-- | The citation clause for the ↳ cite row (@cites @1, @4@), or 'Nothing' when
-- the row should be suppressed. Unfocused shows every step, so citing /every/
-- prior step selects nothing — the default (all shown steps are evidence), so
-- the row is dropped. It earns its place only for a strict subset, where a shown
-- step is load-bearing but not cited. A setup birth keeps the row (the set then
-- includes @0@, which never equals the real-steps set). No arrow sigil — the
-- 'CiteLead' glyph precedes it in the row.
citesBody :: Style -> Trace -> Int -> [Int] -> Maybe Text
citesBody opts trace failingStep steps
  | null uniq = Nothing
  | not (null priorSteps), uniq == priorSteps = Nothing
  | otherwise = Just (opts.phrases.cites (fmap citeToken uniq))
  where
    uniq = sort (nub steps)
    priorSteps = sort [s.index | s <- trace.steps, s.index > 0, s.index < failingStep]

-- | Classify a lifeline's consume at a step: 'NodeDeath' for a lineage-ending
-- consume, 'NodeTransfer' for a consume continued by a transfer (a handoff, its
-- own glyph — never a death), 'Nothing' if the lifeline is not consumed here.
-- Both views' gutters agree on death-vs-handoff through this one predicate.
consumeGlyph :: Trace -> Int -> Lifeline -> Maybe Cell
consumeGlyph trace ix l
  | l.consumedAt /= Just ix = Nothing
  | Trace.continues trace l.var = Just NodeTransfer
  | otherwise = Just NodeDeath

-- | Render the lifecycle fact(s) implicated at a step, for its margin. Delta-only:
-- drop any fact whose value the call text already names (the gutter glyph and the
-- rule verb carry it). Same-verb facts merge into one clause with joined names
-- (@a₁, a₂ accessed@); different verbs join with @·@, strongest first (facts are
-- ordered by 'Blame.factWeight', which is injective, so equal weight ⇒ same verb).
-- Empty when nothing survives. Shared by both views (focused passes ≤1 fact).
factsDoc :: Style -> Trace -> Text -> [Fact] -> Text
factsDoc opts trace callTxt facts =
  T.intercalate
    " · "
    [ opts.phrases.observed f (fmap (nameOf . Blame.factVar) grp)
    | grp@(f : _) <- groupBy ((==) `on` Blame.factWeight) (sortOn (Down . Blame.factWeight) kept)
    ]
  where
    nameOf = displayName opts.glyphs trace
    kept = [f | f <- facts, not (nameOf (Blame.factVar f) `T.isInfixOf` callTxt)]

-- The rendered call plus the free draws that did NOT inline (→ detail rows).
-- Free (non-pool) draws fold into the call in journal order (@write h₁ "0"@)
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

-- | The off-log lifelines: each pool value whose lineage root differs from
-- the failing value's, as a compact trajectory (@▸ h₂: open \@3@). Rendered as
-- its own section after the splice in the composed report; 'Nothing' when the
-- counterexample has no off-log values. Capped, with a counted overflow.
elidedLifelinesDoc :: Style -> Trace -> Blame -> Maybe (Doc Ann)
elidedLifelinesDoc opts trace blame
  | null allLines = Nothing
  | otherwise = Just (PP.vsep (fmap (PP.annotate ElidedAnn . PP.pretty) allLines))
  where
    table = opts.glyphs
    footerCap = 3 :: Int
    mark = table.cell ElidedMark
    subjectRoot = Trace.root trace (Blame.primary blame)
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

    stepW = maximum (1 : [T.length (citeToken i) | Row {stepNo = Just i} <- rows])
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
        stepTxt = T.justifyRight stepW ' ' (maybe "" citeToken r.stepNo)
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
          -- The citation row is evidence below the failing step, not dim context.
          CiteRow -> PP.annotate NoteAnn (PP.pretty callPadded)
          _ -> respAnnotated callPadded

    -- Color the response tail separately from the call head.
    respAnnotated t = case T.breakOn (" " <> table.cell ResponseArrow <> " ") t of
      (_, "") -> PP.pretty t
      (call', resp) -> PP.pretty call' <> PP.annotate ResponseAnn (PP.pretty resp)

    -- Drop trailing all-whitespace segments so lines carry no trailing
    -- spaces (they churn golden pins and diffs).
    dropTrailing :: [(Text, Doc Ann)] -> [(Text, Doc Ann)]
    dropTrailing = reverse . dropWhile (T.all (== ' ') . fst) . reverse
