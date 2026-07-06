-- | Recovering the journal's step structure at the render boundary, and the
-- structured (non-source-spliced) rendering of the recovered tree.
--
-- The journal itself is a flat, depth-stamped @['Note']@ — an append-only
-- streaming sink that stays correct across exception boundaries (see the
-- design rationale in @notes\/01-stateful-test-reporting.md@). The tree is a
-- rendering concern, recovered here by a pure fold over the depth stamps.
module Hegel.Report.Journal
  ( -- * Regrouping
    groupByDepth,
    numberDraws,

    -- * Structured rendering
    journalDocs,
    footnoteDocs,
    noteLineDoc,
    locDoc,
    headlineBlock,
  )
where

import Data.Text (Text)
import Data.Traversable (mapAccumL)
import Data.Tree (Forest, Tree (..))
import GHC.Stack (SrcLoc (..))
import Hegel.Diff (Diff)
import Hegel.Report.Ann (Ann (..), diffDocs)
import Hegel.Report.Note (Note (..), NoteKind (..))
import Prettyprinter (Doc, (<+>))
import Prettyprinter qualified as PP

-- | Regroup a depth-stamped journal into a forest: a note becomes a child of
-- the nearest preceding shallower note. Order is preserved — flattening the
-- forest pre-order yields the input journal.
--
-- Depths need not be contiguous: a note that jumps levels (say 0 to 2) still
-- attaches under the nearest preceding shallower note, keeping its own
-- (deeper) stamp. The renderer indents by depth /difference/, so such
-- orphans land at the same column as before regrouping.
groupByDepth :: [Note] -> Forest Note
groupByDepth = fst . go 0
  where
    -- Consume notes at @level@ or deeper; return the forest built from them
    -- and the remaining (shallower) notes.
    go :: Int -> [Note] -> (Forest Note, [Note])
    go level = \case
      [] -> ([], [])
      n : rest
        | n.depth < level -> ([], n : rest)
        | otherwise ->
            let (children, rest') = go (n.depth + 1) rest
                (siblings, rest'') = go level rest'
             in (Node n children : siblings, rest'')

-- | Number the 'Drawn' notes of a forest, 1-based, __scoped to their
-- siblings__: the counter restarts for each node's children, so a draw's
-- index is its position among the draws of its own step — the same @forAll@
-- across repeated firings of one rule keeps the same index. A flat journal
-- (the non-stateful case) is all siblings, so it keeps the global 1..n
-- numbering. Non-draws get 'Nothing' and do not advance the counter.
numberDraws :: Forest Note -> Forest (Maybe Int, Note)
numberDraws = snd . mapAccumL numberTree 1
  where
    numberTree :: Int -> Tree Note -> (Int, Tree (Maybe Int, Note))
    numberTree i (Node n children) =
      let (i', x) = case n.kind of
            Drawn _ -> (i + 1, (Just i, n))
            _ -> (i, (Nothing, n))
          -- Children number among themselves, from a fresh counter.
          children' = snd (mapAccumL numberTree 1 children)
       in (i', Node x children')

-- | Render the journal: notes regrouped into their step tree ('groupByDepth')
-- and rendered subtree by subtree, draws numbered pre-order ('numberDraws').
-- Footnotes are hoisted to the end at a fixed indent, discarding both their
-- position and their depth.
journalDocs :: [Note] -> [Doc Ann]
journalDocs notes = treeDocs <> footnoteDocs notes
  where
    inline = filter (\n -> n.kind /= Footnote) notes
    -- Roots sit at their stamped depth (not a fixed level) so orphan depth
    -- jumps keep the same columns as before the regrouping.
    treeDocs :: [Doc Ann]
    treeDocs =
      [ PP.indent ((n.depth + 1) * 2) (noteTreeDoc t)
      | t@(Node (_, n) _) <- numberDraws (groupByDepth inline)
      ]

-- | Footnotes ('Footnote' kind) rendered at a fixed indent, in order — hoisted
-- to the end of a report body, their position and depth discarded. Shared by
-- 'journalDocs', 'Hegel.Report.Stateful', and 'Hegel.Report'.
footnoteDocs :: [Note] -> [Doc Ann]
footnoteDocs notes =
  [PP.indent 2 (PP.annotate NoteAnn (PP.pretty n.text)) | n <- notes, n.kind == Footnote]

-- | Render one journal subtree: the note itself, then each child subtree
-- indented by its depth /difference/ (one level = two spaces, so contiguous
-- and orphan depths alike land at @(depth + 1) * 2@ overall).
noteTreeDoc :: Tree (Maybe Int, Note) -> Doc Ann
noteTreeDoc (Node x@(_, n) children) = PP.vsep (noteLineDoc x : childDocs)
  where
    childDocs :: [Doc Ann]
    childDocs =
      [ PP.indent (2 * (c.depth - n.depth)) (noteTreeDoc t)
      | t@(Node (_, c) _) <- children
      ]

-- | Render one numbered note in its structured (non-source-spliced) form:
-- a @Draw N:@ line, an annotation line, or an in-band failure block.
noteLineDoc :: (Maybe Int, Note) -> Doc Ann
noteLineDoc (mIx, n) = case mIx of
  -- An index means a 'Drawn' note: only draws are numbered.
  Just i -> PP.annotate DrawnAnn ("Draw" <+> PP.pretty i <> ":" <+> PP.align (PP.pretty n.text))
  Nothing -> case n.kind of
    Failure diff -> failureNoteDoc diff n
    -- 'Annotation'; 'Footnote' and unnumbered 'Drawn' are unreachable
    -- (footnotes are hoisted before grouping, draws always numbered).
    _ -> PP.annotate NoteAnn (PP.pretty n.text)

-- | Render an in-band 'Failure' note: a marked headline, the structured diff
-- (if any) indented under it, then the source location. Rendered at the
-- note's tree position, so the offsets are relative: @+4@ for the diff (one
-- nesting level plus two to clear the @✗ @ marker), @+2@ for the location.
failureNoteDoc :: Maybe Diff -> Note -> Doc Ann
failureNoteDoc diff n =
  PP.vsep $
    (PP.annotate FailureMark "✗" <+> PP.annotate MessageAnn (PP.pretty n.text))
      : fmap (PP.indent 4) (maybe [] (\d -> [PP.vsep (diffDocs d)]) diff)
        <> fmap (PP.indent 2) (maybe [] (\l -> [PP.annotate LocAnn ("at" <+> locDoc l)]) n.loc)

-- | @file:line@ of a source location.
locDoc :: SrcLoc -> Doc Ann
locDoc sl = PP.pretty sl.srcLocFile <> ":" <> PP.pretty sl.srcLocStartLine

-- | The failure headline plus its indented diff\/location block, headline first.
-- Shared by the ordinary report body ("Hegel.Report") and the composed trace
-- report's 'Failure'-less prelude ("Hegel.Report.Trace.Compose") so the two
-- agree on the block's shape.
headlineBlock :: Text -> Maybe Diff -> Maybe SrcLoc -> [Doc Ann]
headlineBlock message diff loc =
  PP.annotate MessageAnn (PP.pretty message)
    : fmap
      (PP.indent 2)
      ( maybe [] (\d -> [PP.vsep (diffDocs d)]) diff
          <> maybe [] (\l -> [PP.annotate LocAnn ("at" <+> locDoc l)]) loc
      )
