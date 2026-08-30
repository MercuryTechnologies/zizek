-- | Concurrent combinator/fork branches, spliced into their source
-- declarations.
module Hegel.Report.Concurrent
  ( concurrentGroupsDoc,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Report.Ann (Ann (..))
import Hegel.Report.Discovery (Declarations)
import Hegel.Report.Journal (Group (..), noteLineAtDepth, toGroups)
import Hegel.Report.Note (Note (..), NoteKind (..), isBranchFailure)
import Hegel.Report.Source
  ( Annotation,
    Declaration,
    ppFailureLocation,
    ppInlinedValue,
    renderListings,
  )
import Hegel.Report.Span (spanFromSrcLoc)
import Hegel.Report.Style (PhraseTable (..))
import Prettyprinter (Doc, (<+>))
import Prettyprinter qualified as PP

-- | Splice a concurrent journal's branches into source.
concurrentGroupsDoc :: PhraseTable -> Declarations -> [Note] -> Maybe (Doc Ann)
concurrentGroupsDoc phrases decls notes
  | null listings && null summary = Nothing
  | otherwise = Just (PP.vsep (summary <> headers <> listings))
  where
    (groups, _footers) = toGroups notes
    n = length groups
    failing = filter groupHasBranchFailure groups
    passed = filter (not . groupHasBranchFailure) groups
    belowThreshold = n <= branchSpliceThreshold
    shown = if belowThreshold then groups else failing

    -- Per-branch header lines (and any note that couldn't splice), each
    -- paired with the value fragments that did splice.
    (headers, fragments) = foldMap (perBranchDoc decls) shown
    listings = PP.punctuate PP.line (renderListings fragments)

    summary
      | not belowThreshold,
        not (null passed) =
          [PP.indent 4 (PP.annotate ElidedAnn (PP.pretty (phrases.elidedBranches (length passed))))]
      | otherwise = []

-- | One branch's header lines, with a ✗ mark when it failed, paired with its
-- spliced value fragments.
perBranchDoc :: Declarations -> Group -> ([Doc Ann], [Declaration Annotation])
perBranchDoc decls g = (renderedHeader, fragments)
  where
    label = groupHeaderLabel g
    results = [(nt, spliceNote decls label x) | x@(_, nt) <- g.root : g.body]
    structured = [d | (_, Left d) <- results]
    fragments = [f | (_, Right f) <- results]
    anchored
      | groupHasBranchFailure g,
        d : ds <- structured =
          (d <+> PP.annotate FailureMark "✗") : ds
      | otherwise = structured
    renderedHeader
      | null fragments || length structured > 1 = [PP.vsep anchored]
      | otherwise = []

-- | The branch count at or above which passing branches stop splicing and
-- collapse into a summary line. Below it, every branch splices.
branchSpliceThreshold :: Int
branchSpliceThreshold = 4

-- | Does this group's subtree carry a branch's own in-band 'BranchFailure'?
groupHasBranchFailure :: Group -> Bool
groupHasBranchFailure g = any (isBranchFailure . snd) (g.root : g.body)

-- | The label a group's header note carries:
--
-- e.g. @"Branch 1"@ for 'Hegel.Property.Branch' or @"Fork 1"@ for 'Hegel.Property.Fork'.
groupHeaderLabel :: Group -> Maybe Text
groupHeaderLabel g = case (snd g.root).kind of
  BranchHeader _ -> Just (snd g.root).text
  _ -> Nothing

-- | Splice one note into its enclosing source declaration, labeled by its
-- group's header text when it has one.
-- 
-- Falls back to the structured journal line when the note has no location or
-- its source cannot be read.
spliceNote ::
  Declarations ->
  Maybe Text ->
  (Maybe Int, Note) ->
  Either (Doc Ann) (Declaration Annotation)
spliceNote decls label x@(_, n) =
  maybe (Left (noteLineAtDepth x)) Right do
    sl <- n.loc
    let sp = spanFromSrcLoc sl
        tag d = maybe d (\l -> PP.annotate BranchLabelAnn (PP.pretty (l <> ": ")) <> d) label
    case n.kind of
      BranchFailure diff ->
        ppFailureLocation decls (tag . PP.pretty <$> T.lines n.text) diff sp
      _ ->
        ppInlinedValue decls (tag . PP.annotate AnnotationValue . PP.pretty <$> T.lines n.text) sp
