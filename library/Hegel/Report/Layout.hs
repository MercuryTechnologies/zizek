-- | The flat, chronological event log: one row per step, a bare @✗@/blank
-- gutter, and touch-irrelevant runs collapsed into a single elision row.
--
-- Intended to be imported with qualification:
--
-- > import Hegel.Report.Layout (Row (..))
-- > import Hegel.Report.Layout qualified as Layout
module Hegel.Report.Layout
  ( RowKind (..),
    Row (..),
    layoutRows,
    logDoc,
    displayName,
  )
where

import Data.List (nub, sortOn)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Internal.Event (Var (..))
import Hegel.Report.Ann (Ann (..))
import Hegel.Report.Note (Note (..), NoteKind (..))
import Hegel.Report.Style (Cell (..), GlyphTable (..), PhraseTable (..), Style (..), firstLine)
import Hegel.Report.Trace (Identity (..), Step (..), Touch (..), Trace)
import Hegel.Report.Trace qualified as Trace
import Prettyprinter (Doc)
import Prettyprinter qualified as PP

-- | What kind of row this is, for how its call text renders.
data RowKind
  = -- | A step's own call, dim only when it failed.
    NodeRow
  | -- | A dim detail line under a step: a free draw or an annotation.
    DetailRow
  | -- | @N steps elided@ between two rendered steps.
    ElisionRow
  deriving stock (Show, Eq)

-- | One row of the event log: the gutter glyph, the step number, and the
-- call text.
data Row = Row
  { kind :: !RowKind,
    gutter :: !Cell,
    stepNo :: !(Maybe Int),
    call :: !Text
  }
  deriving stock (Show, Eq)

-- * Layout

-- | The lineage roots the failing step's own touches resolve to, the
-- relevance key every other step is judged against. Empty when the failure
-- touched no pool value, which keeps every step.
relevantRoots :: Trace -> [Var]
relevantRoots trace =
  case trace.failure >>= \f -> Trace.step trace f.step of
    Nothing -> []
    Just failing -> nub [Trace.root trace t.var | t <- failing.touches]

-- | Lay the trace out as event-log rows, oldest step first and the failing
-- step last. A step is kept when it failed, when the failure touched no
-- pool value at all, or when it shares a lineage root with the failing
-- step's own touches. Any other run of steps collapses into one
-- 'ElisionRow'.
layoutRows :: Style -> Trace -> [Row]
layoutRows opts trace = preludeRows <> go Nothing kept
  where
    roots = relevantRoots trace
    relevantStep s = s.failed || null roots || any onRoot s.touches
    onRoot t = Trace.root trace t.var `elem` roots

    -- Real steps only (index > 0), oldest first; step 0 is machine setup and
    -- contributes detail lines only, never a call row.
    realSteps = sortOn (.index) [s | s <- trace.steps, s.index > 0]
    kept = filter relevantStep realSteps
    preludeRows = concatMap detailRows [s | s <- trace.steps, s.index == 0]

    go _ [] = []
    go mLo (s : rest) =
      elisionBetween mLo s.index
        <> (stepRow s : detailRows s)
        <> go (Just s.index) rest

    -- Elision rows for the gap between the previous kept step and this one.
    elisionBetween mLo hi =
      [ Row {kind = ElisionRow, gutter = Blank, stepNo = Nothing, call = elisionLabel between}
      | lo <- maybe [] pure mLo,
        let between = [st | st <- realSteps, st.index > lo, st.index < hi],
        not (null between)
      ]
    -- Names the value(s) the elided run touched. Nothing when the run
    -- touched nothing at all.
    elisionLabel between =
      opts.phrases.elidedSteps (length between) concerns
      where
        hidden = nub [nameOf (Trace.root trace t.var) | s <- between, t <- s.touches]
        concerns
          | null hidden = Nothing
          | otherwise = Just (T.intercalate ", " hidden)

    stepRow s =
      Row
        { kind = NodeRow,
          gutter = if s.failed then NodeFail else Blank,
          stepNo = Just s.index,
          call = fst (stepCall opts trace s)
        }

    -- Detail lines: free draws that didn't inline, then the step's
    -- annotations, in journal order.
    detailRows s =
      [ Row {kind = DetailRow, gutter = Blank, stepNo = Nothing, call = l}
      | line <- snd (stepCall opts trace s) <> annotations s,
        l <- T.lines line
      ]
    annotations :: Step -> [Text]
    annotations s = [n.text | n <- s.notes, n.kind == Annotation]

    nameOf = displayName opts.glyphs trace

-- | A step reference for the log, matching the source splice's @Step N:@
-- header vocabulary.
stepToken :: Int -> Text
stepToken n = "Step " <> T.pack (show n) <> ":"

-- | The rendered call, plus the free draws that did NOT inline (→ detail
-- rows). Free (non-pool) draws fold into the call in journal order
-- (@write h₁ "0"@) while each is single-line and the call still fits the
-- width budget; the first that doesn't (and all after it, to keep order)
-- become dim detail rows. Pool references stay symbolic (no inline value).
stepCall :: Style -> Trace -> Step -> (Text, [Text])
stepCall opts trace s = (clip opts (T.unwords (s.rule : touchNames <> inlined) <> respText), detail)
  where
    table = opts.glyphs
    nameOf = displayName table trace
    touchNames = nub (fmap (nameOf . (.var)) s.touches)
    respText = maybe "" (\r -> " " <> table.cell ResponseArrow <> " " <> firstLine r) s.response
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

-- | A value's display name, resolved through its lineage root:
--
-- The pool's 'Hegel.Pool.named' label (or an automatic assignment) plus a
-- numeric identifier in order of a pooled variable's introduction.
--
-- Defined as @\\tbl trace -> \\v -> ...@ so callers that bind
-- @nameOf = displayName tbl trace@ share the precomputed per-trace name
-- table across every lookup.
displayName :: GlyphTable -> Trace -> Var -> Text
displayName tbl trace =
  \v -> Map.findWithDefault (compute (Trace.root trace v)) (Trace.root trace v) precomputed
  where
    poolOrds :: Map.Map Int Int
    poolOrds = Map.fromList (zip (nub [i.var.pool | i <- trace.identities]) [0 ..])
    compute :: Var -> Text
    compute r =
      let ident = Trace.identity trace r
       in tbl.valueName
            (ident >>= (.label))
            (Map.findWithDefault 0 r.pool poolOrds)
            (maybe 0 (.ordinal) ident)
    precomputed :: Map.Map Var Text
    precomputed = Map.fromList [(Trace.root trace i.var, compute (Trace.root trace i.var)) | i <- trace.identities]

-- * Rendering

-- | Render the rows as an aligned document.
logDoc :: Style -> Trace -> Doc Ann
logDoc opts trace = PP.vsep (fmap rowDoc rows)
  where
    rows = layoutRows opts trace
    table = opts.glyphs
    stepW = maximum (1 : [T.length (stepToken i) | Row {stepNo = Just i} <- rows])

    rowDoc r = foldMap snd (dropTrailing segments)
      where
        segments =
          [ (gutterTxt, gutterDoc),
            (" ", " "),
            (stepTxt, PP.annotate StepNoAnn (PP.pretty stepTxt)),
            (" ", " "),
            (r.call, callDoc)
          ]
        gutterTxt = table.cell r.gutter
        gutterDoc = PP.annotate (if r.gutter == NodeFail then FailureMark else StrandAnn 0) (PP.pretty gutterTxt)
        stepTxt = T.justifyRight stepW ' ' (maybe "" stepToken r.stepNo)
        callDoc = case r.kind of
          -- Detail and elision rows are supporting context, rendered dim.
          DetailRow -> PP.annotate ElidedAnn (PP.pretty r.call)
          ElisionRow -> PP.annotate ElidedAnn (PP.pretty r.call)
          NodeRow -> respAnnotated r.call

    -- Color the response tail separately from the call head.
    respAnnotated t = case T.breakOn (" " <> table.cell ResponseArrow <> " ") t of
      (_, "") -> PP.pretty t
      (call', resp) -> PP.pretty call' <> PP.annotate ResponseAnn (PP.pretty resp)

    -- Drop trailing all-whitespace segments so lines carry no trailing
    -- spaces (they churn golden pins and diffs).
    dropTrailing :: [(Text, Doc Ann)] -> [(Text, Doc Ann)]
    dropTrailing = reverse . dropWhile (T.all (== ' ') . fst) . reverse
