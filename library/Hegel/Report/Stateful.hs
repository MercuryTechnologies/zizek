-- | Source-spliced rendering of stateful (step-structured) failure journals:
-- two 'Layout's over the same journal. 'Timeline' backs
-- 'Hegel.Report.renderReportRich' for step journals; 'Aggregate' is under
-- evaluation against real failures via the @demo-stateful-rich@ harness. See
-- the doc-rendering plan referenced in @notes\/01-stateful-test-reporting.md@.
module Hegel.Report.Stateful
  ( Layout (..),
    statefulDoc,
    noteFiles,
    isStepJournal,
  )
where

import Data.Char qualified as Char
import Data.Either (partitionEithers)
import Data.List (partition)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (Tree (..), flatten)
import GHC.Stack (SrcLoc (..))
import Hegel.Diff (Diff)
import Hegel.Report.Ann (Ann (..), diffDocs)
import Hegel.Report.Discovery (Declarations)
import Hegel.Report.Journal (groupByDepth, locDoc, noteLineDoc, numberDraws)
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

-- | Stateful rich layouts: two projections of the same journal, kept side by
-- side to evaluate against real failures. 'Timeline' is the default-to-be;
-- 'Aggregate' serves the cross-step reading (value evolution, whole-run
-- forensics).
data Layout
  = -- | __Chronological__: the structured timeline spine, unchanged from the
    -- plain renderer; the step that carries the in-band 'Failure' splices
    -- __all__ of its notes (draws and annotations included) — full source
    -- context at the failure site, no repetition elsewhere.
    Timeline
  | -- | __Machine-centric__: a compact timeline, then each fired
    -- declaration's source once, with the values from __every__ step
    -- aggregated under the line that drew them, labeled by step.
    Aggregate

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

-- | Render a step-structured failure under the given 'Layout'. Pure; the
-- caller loads 'Declarations' once (see 'noteFiles'). Notes that cannot be
-- spliced — no location, unreadable source, or excluded by the layout —
-- fall back to their structured journal line, per-note; when nothing at all
-- splices, the output equals the plain structured layout.
--
-- Mirrors 'Hegel.Report.failureDoc''s branches: a journal carrying an
-- in-band 'Failure' suppresses the top-level headline\/diff\/location block
-- (the 'Failure' note carries them); a 'Failure'-less step journal (e.g. an
-- exception mid-loop) keeps them.
statefulDoc :: Layout -> Declarations -> Text -> [Note] -> Maybe SrcLoc -> Maybe Diff -> Doc Ann
statefulDoc layout decls message notes loc diff
  | hasInBandFailure notes = body
  | otherwise = PP.vsep (PP.annotate MessageAnn (PP.pretty message) : topBlock <> [body])
  where
    body = case layout of
      Timeline -> PP.vsep (fmap (groupDoc decls) groups <> footerDocs)
      Aggregate -> aggregateDoc decls groups footerDocs
    (groups, footers) = toGroups notes
    footerDocs = [PP.indent 2 (PP.annotate NoteAnn (PP.pretty n.text)) | n <- footers]
    topBlock :: [Doc Ann]
    topBlock =
      fmap
        (PP.indent 2)
        ( maybe [] (\d -> [PP.vsep (diffDocs d)]) diff
            <> maybe [] (\l -> [PP.annotate LocAnn ("at" <+> locDoc l)]) loc
        )

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

-- | Render one group for the 'Timeline' spine: structured lines for the
-- header and any unspliced notes (in journal order), then the group's merged
-- source listings. Only the failure-carrying group splices; all other groups
-- render exactly as the plain layout.
groupDoc :: Declarations -> Group -> Doc Ann
groupDoc decls g = PP.vsep (anchored <> listings)
  where
    results =
      [ ( n,
          if groupHasFailure g && isJust n.loc
            then spliceNote decls Nothing x
            else Left (fallbackLine x)
        )
      | x@(_, n) <- g.root : g.body
      ]
    structured = [d | (_, Left d) <- results]
    fragments = [f | (_, Right f) <- results]
    -- The spliced listing replaces the in-band ✗ block, so re-anchor the
    -- failure on the spine: suffix the group's first structured line
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

-- | The 'Aggregate' layout: a compact timeline (one line per group,
-- @✗@-suffixed on the failing one, unspliced notes nested under their
-- entry), then each fired declaration's listing once, values labeled by
-- step.
aggregateDoc :: Declarations -> [Group] -> [Doc Ann] -> Doc Ann
aggregateDoc decls groups footerDocs =
  PP.vsep (fmap fst perGroup <> listings <> footerDocs)
  where
    perGroup =
      [ (PP.vsep (headerLine g : structured), fragments)
      | g <- groups,
        let (structured, fragments) =
              partitionEithers
                [ if isJust n.loc
                    then spliceNote decls (Just (stepLabel g)) x
                    else Left (fallbackLine x)
                | x@(_, n) <- g.body
                ]
      ]
    listings =
      [ PP.indent 2 (ppDeclaration (applyContext defaultContext d))
      | d <- mergeFileDeclarations (mergeDeclarations (concatMap snd perGroup))
      ]
    -- Suffix only for failures in the body: a 'Failure' root renders its own
    -- ✗ block via 'fallbackLine' already.
    headerLine g
      | any (isFailureNote . snd) g.body =
          fallbackLine g.root <+> PP.annotate FailureMark "✗"
      | otherwise = fallbackLine g.root

-- | The label prefixed to a group's values in the 'Aggregate' layout,
-- parsed from the header text (@Step N: rule@ → @step N:@).
--
-- Textual contract with 'Hegel.Stateful.run'\'s step notes; made structural
-- (a payload-bearing note kind) by the doc-rendering task's final milestone.
stepLabel :: Group -> Doc Ann
stepLabel g = case T.stripPrefix "Step " (snd g.root).text of
  Just rest -> PP.pretty ("step " <> T.takeWhile Char.isDigit rest <> ":")
  Nothing -> "initial:"

-- | A note's structured journal line at its depth indent — identical to the
-- plain renderer's line for that note.
fallbackLine :: (Maybe Int, Note) -> Doc Ann
fallbackLine x@(_, n) = PP.indent ((n.depth + 1) * 2) (noteLineDoc x)

-- | Splice one note into its enclosing source declaration: 'Failure' notes
-- get the arrows\/message\/diff treatment, draws and annotations get their
-- text inlined under the line that produced them (first line prefixed with
-- the label, when given). Falls back to the structured journal line when
-- the note has no location or its source cannot be read.
spliceNote ::
  Declarations ->
  Maybe (Doc Ann) ->
  (Maybe Int, Note) ->
  Either (Doc Ann) (Declaration Annotation)
spliceNote decls mlabel x@(_, n) =
  maybe (Left (fallbackLine x)) Right do
    sl <- n.loc
    let sp = spanFromSrcLoc sl
    case n.kind of
      Failure diff ->
        ppFailureLocation decls (labelFirst (fmap PP.pretty (T.lines n.text))) diff sp
      _ ->
        ppInlinedValue decls (labelFirst (fmap (PP.annotate AnnotationValue . PP.pretty) (T.lines n.text))) sp
  where
    labelFirst = \case
      [] -> []
      (d : ds) -> maybe d (<+> d) mlabel : ds
