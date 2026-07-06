-- | The failing step of a stateful (step-structured) journal, spliced into its
-- source declaration — the source splice the composed event-log report renders
-- after the log (the log carries every other step's story). Backs
-- 'Hegel.Report.renderReportRich' for step journals; eyeball via the @gallery@
-- example (`just gallery`). Layout rationale (and the deleted @Aggregate@
-- alternative) is recorded in @notes\/decisions\/stateful-reporting.md@.
module Hegel.Report.Stateful
  ( failingGroupDoc,
    noteFiles,
    isStepJournal,
  )
where

import Data.List (partition)
import Data.Maybe (isJust)
import Data.Text qualified as T
import Data.Tree (Tree (..), flatten)
import GHC.Stack (SrcLoc (..))
import Hegel.Report.Ann (Ann (..))
import Hegel.Report.Discovery (Declarations)
import Hegel.Report.Journal (groupByDepth, noteLineDoc, numberDraws)
import Hegel.Report.Note (Note (..), NoteKind (..), hasInBandFailure, isFailureNote)
import Hegel.Report.Source
  ( Annotation,
    Declaration,
    applyContext,
    defaultContext,
    mergeDeclarations,
    mergeFileDeclarations,
    ppDeclaration,
    ppFailureLocation,
    ppInlinedValue,
  )
import Hegel.Report.Span (spanFromSrcLoc)
import Prettyprinter (Doc, (<+>))
import Prettyprinter qualified as PP

-- | Files referenced by the journal, for the caller's single
-- 'Hegel.Report.Discovery.loadDeclarations' pass.
noteFiles :: [Note] -> [FilePath]
noteFiles notes = [l.srcLocFile | n <- notes, Just l <- [n.loc]]

-- | Is this journal step-structured (a stateful report)? True when it
-- carries an in-band 'Failure' or any nested note — 'hasInBandFailure' alone
-- misses a 'Failure'-less stateful counterexample, and @depth > 0@ alone
-- misses a failure in @machine.initial@ (journaled at depth 0).
isStepJournal :: [Note] -> Bool
isStepJournal notes = hasInBandFailure notes || any (\n -> n.depth > 0) notes

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

-- | One depth-0 subtree of the journal: a step header (or bare top-level
-- note) plus its flattened body, draw numbers pre-assigned journal-wide so
-- fallback lines agree with the plain renderer.
data Group = Group
  { root :: (Maybe Int, Note),
    body :: [(Maybe Int, Note)]
  }

toGroups :: [Note] -> ([Group], [Note])
toGroups notes = (fmap toGroup (numberDraws (groupByDepth inline)), footers)
  where
    (footers, inline) = partition (\n -> n.kind == Footnote) notes
    toGroup (Node r children) = Group {root = r, body = concatMap flatten children}

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
            else Left (fallbackLine x)
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
    listings =
      [ PP.indent 4 (ppDeclaration (applyContext defaultContext d))
      | d <- mergeFileDeclarations (mergeDeclarations fragments)
      ]

-- | A note's structured journal line at its depth indent — identical to the
-- plain renderer's line for that note.
fallbackLine :: (Maybe Int, Note) -> Doc Ann
fallbackLine x@(_, n) = PP.indent ((n.depth + 1) * 2) (noteLineDoc x)

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
  maybe (Left (fallbackLine x)) Right do
    sl <- n.loc
    let sp = spanFromSrcLoc sl
    case n.kind of
      Failure diff ->
        ppFailureLocation decls (fmap PP.pretty (T.lines n.text)) diff sp
      _ ->
        ppInlinedValue decls (fmap (PP.annotate AnnotationValue . PP.pretty) (T.lines n.text)) sp
