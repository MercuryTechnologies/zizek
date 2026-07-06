-- | Assembles the composed trace report: the chronological citation spine, the
-- failing step's source splice, the off-spine lifelines, footnotes, and the
-- reproduction footer. Every section is a projection of the same 'Trace' and
-- 'Blame'.
--
-- Intended to be imported with qualification:
--
-- > import Hegel.Report.Trace.Compose qualified as Compose
module Hegel.Report.Trace.Compose
  ( composedDoc,
    footnotesDoc,
    footerDoc,
  )
where

import Data.Maybe (catMaybes)
import Data.Text (Text)
import Hegel.Report.Ann (Ann (..))
import Hegel.Report.Discovery (Declarations)
import Hegel.Report.Journal (footnoteDocs)
import Hegel.Report.Note (Note)
import Hegel.Report.Phrase (PhraseTable (..))
import Hegel.Report.Stateful (failingGroupDoc)
import Hegel.Report.Style (Style (..))
import Hegel.Report.Trace (Trace)
import Hegel.Report.Trace.Blame (Blame)
import Hegel.Report.Trace.Spine qualified as Spine
import Prettyprinter (Doc)
import Prettyprinter qualified as PP

-- | The composed report (sans the @failed after …@ line, which the caller
-- prepends): the chronological citation spine, the failing step's source
-- splice, the off-spine lifelines, footnotes, and the reproduction footer.
-- Sections separate with one blank line.
--
-- Chronological: the spine reads oldest → failing step, flowing straight into
-- that step's source splice — which carries the diff, so the spine stays a pure
-- lifeline and does not repeat it (see "Hegel.Report.Trace.Spine").
composedDoc :: Style -> Declarations -> Trace -> Blame -> [Note] -> Maybe Text -> Doc Ann
composedDoc style decls trace blame notes databaseKey =
  PP.vsep (PP.punctuate PP.line (catMaybes sections))
  where
    sections =
      [ Just (Spine.spineDoc style trace blame),
        failingGroupDoc decls notes,
        Spine.elidedLifelinesDoc style trace blame,
        footnotesDoc notes,
        footerDoc style.phrases databaseKey
      ]

-- | Footnote notes, rendered after the report body (their documented
-- position, regardless of form).
footnotesDoc :: [Note] -> Maybe (Doc Ann)
footnotesDoc notes = case footnoteDocs notes of
  [] -> Nothing
  ds -> Just (PP.vsep ds)

-- | The reproduction footer: present only when the run persisted under a
-- database key (pointing anywhere else would be dishonest — replay is
-- automatic on the next run, there is no CLI yet). Words from the phrase
-- table, like everything else.
footerDoc :: PhraseTable -> Maybe Text -> Maybe (Doc Ann)
footerDoc phrases = fmap (PP.annotate LocAnn . PP.pretty . phrases.stored)
