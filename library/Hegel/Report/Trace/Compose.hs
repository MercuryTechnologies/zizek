-- | Assembles the composed trace report: the chronological event log, the
-- failing step's source splice, the off-log lifelines, footnotes, and the
-- reproduction footer. Every section is a projection of the same 'Trace' and
-- 'Blame'.
--
-- Intended to be imported with qualification:
--
-- > import Hegel.Report.Trace.Compose qualified as Compose
module Hegel.Report.Trace.Compose
  ( composedDoc,
  )
where

import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Stack (SrcLoc)
import Hegel.Diff (Diff)
import Hegel.Report.Ann (Ann (..))
import Hegel.Report.Discovery (Declarations)
import Hegel.Report.Journal (footnoteDocs, headlineBlock)
import Hegel.Report.Note (Note, hasInBandFailure)
import Hegel.Report.Phrase (PhraseTable (..))
import Hegel.Report.Stateful (failingGroupDoc)
import Hegel.Report.Style (Style (..))
import Hegel.Report.Trace (Trace)
import Hegel.Report.Trace.Log (View (..))
import Hegel.Report.Trace.Log qualified as Log
import Prettyprinter (Doc)
import Prettyprinter qualified as PP

-- | The composed report (sans the @failed after …@ line, which the caller
-- prepends): the chronological event log, the failing step's source splice,
-- (focused only) the off-log lifelines, footnotes, and the reproduction
-- footer. Sections separate with one blank line.
--
-- Chronological: the log reads oldest → failing step, flowing straight into
-- that step's source splice — which carries the diff, so the log stays a pure
-- lifeline and does not repeat it (see "Hegel.Report.Trace.Log").
--
-- Two shapes, by 'View':
--
-- * 'Focused' — one pool value's story: log (others elided), splice, off-log
--   lifelines, footnotes, footer.
-- * 'Unfocused' — every step shown. A 'Failure'-less journal (an exception
--   mid-loop, or a failure in @machine.initial@ at depth 0) has no in-band
--   @✗@ to anchor the reason, so a headline\/diff\/location block leads;
--   an in-band failure suppresses it (the splice carries it).
composedDoc ::
  Style ->
  Declarations ->
  Trace ->
  View ->
  [Note] ->
  Text ->
  Maybe SrcLoc ->
  Maybe Diff ->
  Maybe Text ->
  Doc Ann
composedDoc style decls trace view notes message loc diff databaseKey =
  PP.vsep (PP.punctuate PP.line (catMaybes sections))
  where
    sections = case view of
      Focused blame ->
        [ Just (Log.logDoc style trace (Focused blame)),
          failingGroupDoc decls notes,
          Log.elidedLifelinesDoc style trace blame,
          footnotesDoc notes,
          footerDoc style.phrases databaseKey
        ]
      Unfocused mBlame ->
        [ preludeBlock,
          Just (Log.logDoc style trace (Unfocused mBlame)),
          failingGroupDoc decls notes,
          footnotesDoc notes,
          footerDoc style.phrases databaseKey
        ]

    -- A Failure-less journal keeps the top-level headline/diff/location (the
    -- log has no ✗ row and there is no splice, so nothing else states the
    -- reason); an in-band failure suppresses it to avoid double-rendering.
    preludeBlock
      | hasInBandFailure notes = Nothing
      | otherwise = Just (PP.vsep (headlineBlock message diff loc))

-- | Footnote notes, rendered after the report body (their documented
-- position, regardless of form).
footnotesDoc :: [Note] -> Maybe (Doc Ann)
footnotesDoc notes = case footnoteDocs notes of
  [] -> Nothing
  ds -> Just (PP.vsep ds)

-- | The reproduction footer: present only when the run persisted under a
-- database key (replay is automatic on the next run; there is no CLI to point
-- at a key by hand yet). Words from the phrase table, like everything else.
footerDoc :: PhraseTable -> Maybe Text -> Maybe (Doc Ann)
footerDoc phrases = fmap (PP.annotate LocAnn . PP.pretty . phrases.stored)
