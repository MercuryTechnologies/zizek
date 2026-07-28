-- | The failing step of a stateful (step-structured) journal, spliced into its
-- source declaration — the source splice the composed event-log report renders
-- after the log (the log carries every other step's story). Backs
-- 'Hegel.Report.renderReportRich' for step journals; eyeball via the @gallery@
-- example (`just gallery`).
module Hegel.Report.Stateful
  ( failingGroupDoc,
    noteFiles,
    JournalShape (..),
    classifyJournal,
  )
where

import Data.Maybe (isJust)
import Data.Text qualified as T
import GHC.Stack (SrcLoc (..))
import Hegel.Report.Ann (Ann (..))
import Hegel.Report.Discovery (Declarations)
import Hegel.Report.Journal (Group (..), noteLineAtDepth, toGroups)
import Hegel.Report.Note (Note (..), NoteKind (..), isBranchFailure, isBranchHeader, isFailureNote)
import Hegel.Report.Source
  ( Annotation,
    Declaration,
    ppFailureLocation,
    ppInlinedValue,
    renderListings,
  )
import Hegel.Report.Span (spanFromSrcLoc)
import Prettyprinter (Doc, (<+>))
import Prettyprinter qualified as PP

-- | Files referenced by the journal, for the caller's single
-- 'Hegel.Report.Discovery.loadDeclarations' pass.
noteFiles :: [Note] -> [FilePath]
noteFiles notes = [l.srcLocFile | n <- notes, Just l <- [n.loc]]

-- | Which failure-report layout a journal's shape calls for.
data JournalShape
  = -- | Ordinary draws\/annotations at the top level: 'Hegel.Report.plainRichDoc'.
    PlainShape
  | -- | A 'Hegel.Stateful.Machine' run: the composed event-log report.
    StatefulShape
  | -- | A 'Hegel.Property.Branch' combinator's branches: the per-branch
    -- source splice.
    ConcurrentShape
  deriving stock (Show, Eq)

-- | Classify a journal by the structural markers its producer left, not by
-- 'Note.depth'. A depth-0 'StepHeader' or an in-band 'Failure' marks a
-- stateful run; a 'BranchHeader' or 'BranchFailure' marks a concurrent
-- combinator's.
--
-- Stateful takes precedence, so a concurrent combinator run from inside a
-- stateful rule still composes as a stateful report, with its branches
-- rendered as nested detail inside the failing step.
classifyJournal :: [Note] -> JournalShape
classifyJournal notes
  | any isFailureNote notes || any isStepHeaderAtDepth0 notes = StatefulShape
  | any isBranchFailure notes || any isBranchHeader notes = ConcurrentShape
  | otherwise = PlainShape
  where
    -- Depth alone can't tell which nested user produced a journal, since
    -- every one of them stamps depth the same way; the depth-0 guard here
    -- excludes a stateful run nested inside a concurrent branch, whose
    -- depth-0 note is a 'BranchHeader' instead.
    isStepHeaderAtDepth0 :: Note -> Bool
    isStepHeaderAtDepth0 n = n.depth == 0 && case n.kind of StepHeader {} -> True; _ -> False

-- | The failing step alone, spliced — the composed event-log report's source
-- splice (the log carries every other step's story).
-- 'Nothing' when no group carries the in-band 'Failure'.
failingGroupDoc :: Declarations -> [Note] -> Maybe (Doc Ann)
failingGroupDoc decls notes =
  case [g | g <- fst (toGroups notes), groupHasFailure g] of
    (g : _) -> Just (groupDoc decls g)
    [] -> Nothing

-- | Does this group's subtree carry the in-band 'Failure'?
groupHasFailure :: Group -> Bool
groupHasFailure g = any (isFailureNote . snd) (g.root : g.body)

-- | Render one group of the journal: structured lines for the header and any
-- unspliced notes (in journal order), then the group's merged source listings.
-- Only the failure-carrying group splices; all other groups render exactly as
-- the plain layout.
groupDoc :: Declarations -> Group -> Doc Ann
groupDoc decls g = PP.vsep (anchored <> listings)
  where
    results =
      [ ( n,
          if groupHasFailure g && isJust n.loc
            then spliceNote decls x
            else Left (noteLineAtDepth x)
        )
      | x@(_, n) <- g.root : g.body
      ]
    structured = [d | (_, Left d) <- results]
    fragments = [f | (_, Right f) <- results]
    -- The spliced listing replaces the in-band ✗ block, so re-anchor the
    -- failure in the event log: suffix the group's first structured line
    -- (normally the step header) with the mark.
    anchored
      | or [isFailureNote n | (n, Right _) <- results],
        d : ds <- structured =
          (d <+> PP.annotate FailureMark "✗") : ds
      | otherwise = structured
    -- Punctuated so two listings from different files get a blank line
    -- between their boxes, matching the gap 'composed' puts between sections.
    listings = PP.punctuate PP.line (renderListings fragments)

-- | Splice one note into its enclosing source declaration: 'Failure' notes
-- get the arrows\/message\/diff treatment, draws and annotations get their
-- text inlined under the line that produced them. Falls back to the
-- structured journal line when the note has no location or its source
-- cannot be read.
spliceNote ::
  Declarations ->
  (Maybe Int, Note) ->
  Either (Doc Ann) (Declaration Annotation)
spliceNote decls x@(_, n) =
  maybe (Left (noteLineAtDepth x)) Right do
    sl <- n.loc
    let sp = spanFromSrcLoc sl
    case n.kind of
      Failure diff ->
        ppFailureLocation decls (PP.pretty <$> T.lines n.text) diff sp
      _ ->
        ppInlinedValue decls ((PP.annotate AnnotationValue . PP.pretty) <$> T.lines n.text) sp
