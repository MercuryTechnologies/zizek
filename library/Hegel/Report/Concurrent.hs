-- | A concurrent combinator's or fork's branches, spliced into their source
-- declarations. Backs 'Hegel.Report.renderReportRich' for journals produced
-- by "Hegel.Property.Branch" and "Hegel.Property.Fork".
--
-- Below 'branchSpliceThreshold' branches, every branch splices its own source
-- lines in full: a branch that ran to completion without failing still shows
-- its draws and its use of a shared 'Pool'. At or above the threshold, only
-- failing branches splice and the rest collapse to a count, so a large
-- fan-out such as @replicateConcurrently 100 ...@ doesn't flood the report
-- with near-identical passing branches.
--
-- Branches sharing source, as every branch of a homogeneous fan-out does,
-- merge into one listing instead of repeating the same lines once per
-- branch, with each stacked line labeled by its branch number so
-- attribution survives the merge.
--
-- A branch's own @Branch N@ header line renders only when it still has an
-- unspliced note to anchor, or no spliced content at all; once its content
-- is fully spliced, the per-line labels inside the merged listing already
-- announce it.
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

-- | Splice a concurrent journal's branches into source. Below
-- 'branchSpliceThreshold' branches, every branch splices; at or above it,
-- only failing branches splice, with a summary line counting the rest ahead
-- of them. 'Nothing' when nothing was left to splice, the same
-- degrade-to-plain convention 'Hegel.Report.plainRichDoc' follows.
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
    -- paired with the value fragments that did splice. A 'BranchHeader' note
    -- carries no location, so it always falls to the structured line and
    -- never enters the merge below: header rendering stays per-branch,
    -- decoupled from the merge across every branch's fragments.
    (headers, fragments) = foldMap (perBranchDoc decls) shown

    -- One merge across every shown branch's value fragments: branches
    -- sharing source (file, line) collapse into one listing with their
    -- labeled lines stacked; branches with distinct source stay separate.
    -- Punctuated so two listings from different files get a blank line
    -- between their boxes, matching the gap 'composed' puts between sections.
    listings = PP.punctuate PP.line (renderListings fragments)

    summary
      | not belowThreshold,
        not (null passed) =
          [PP.indent 4 (PP.annotate ElidedAnn (PP.pretty (phrases.elidedBranches (length passed))))]
      | otherwise = []

-- | One branch's header lines, with a ✗ mark when it failed, paired with its
-- spliced value fragments.
--
-- The header is suppressed when the branch's content fully spliced, since
-- the per-line @Branch N:@ labels inside the merged listing already
-- attribute it. It is kept when there is nothing else to show for the
-- branch, or a note that couldn't splice still needs it as an anchor.
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

-- | The label a group's header note carries, e.g. @"Branch 1"@ for
-- 'Hegel.Property.Branch' or @"Fork 1"@ for 'Hegel.Property.Fork'. Read
-- straight from the header note's own text, so a fork's group renders under
-- @Fork N@ without a distinct 'NoteKind'.
--
-- 'Nothing' when the group's root isn't a header at all: a
-- 'Hegel.Property.Fork.spawn' call alongside genuine top-level code gives
-- its own top-level notes a depth-0 group rooted at an ordinary note rather
-- than a 'BranchHeader'. Those render like any other splice, with no label.
groupHeaderLabel :: Group -> Maybe Text
groupHeaderLabel g = case (snd g.root).kind of
  BranchHeader _ -> Just (snd g.root).text
  _ -> Nothing

-- | Splice one note into its enclosing source declaration, labeled by its
-- group's header text when it has one, so a merge across groups (see
-- 'concurrentGroupsDoc') keeps each stacked line attributable: a
-- 'BranchFailure' gets the arrows\/message\/diff treatment, draws and
-- annotations get their text inlined under the line that produced them.
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
