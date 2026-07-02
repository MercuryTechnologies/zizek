-- | Assembles the composed trace report: the verdict headline, the citation
-- spine, the failing step's source splice, footnotes, and the reproduction
-- footer. Every section is a projection of the same 'Trace' and 'Blame'.
--
-- Intended to be imported with qualification:
--
-- > import Hegel.Report.Trace.Compose qualified as Compose
module Hegel.Report.Trace.Compose
  ( composedDoc,
    headlineDoc,
    footnotesDoc,
    footerDoc,
  )
where

import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Report.Ann (Ann (..))
import Hegel.Report.Discovery (Declarations)
import Hegel.Report.Journal (footnoteDocs)
import Hegel.Report.Note (Note)
import Hegel.Report.Phrase (PhraseTable (..))
import Hegel.Report.Phrase qualified as Phrase
import Hegel.Report.Stateful (failingGroupDoc)
import Hegel.Report.Style (Style (..))
import Hegel.Report.Trace (Step (..), Trace (..))
import Hegel.Report.Trace qualified as Trace
import Hegel.Report.Trace.Blame (Blame (..), Observation (..))
import Hegel.Report.Trace.Spine qualified as Spine
import Prettyprinter (Doc)
import Prettyprinter qualified as PP

-- | The composed report (sans the @failed after …@ line, which the caller
-- prepends): headline, spine, failing-step splice, footnotes, footer.
-- Sections separate with one blank line.
composedDoc :: Style -> Declarations -> Trace -> Blame -> [Note] -> Maybe Text -> Doc Ann
composedDoc style decls trace blame notes databaseKey =
  PP.vsep (PP.punctuate PP.line (catMaybes sections))
  where
    sections =
      [ headlineDoc style trace blame,
        Just (Spine.spineDoc style trace blame),
        failingGroupDoc decls notes,
        footnotesDoc notes,
        footerDoc style.phrases databaseKey
      ]

-- | Word the verdict as a single reflowing headline, e.g.
-- @\"Step 5 (verify): expected Nothing.\"@ The justifications are not worded
-- here — every cited fact is already at its arrowhead in the spine below.
--
-- Leads with the failure reason (or the observed response when there is no
-- message). 'Nothing' when the blame tree has no citations.
headlineDoc :: Style -> Trace -> Blame -> Maybe (Doc Ann)
headlineDoc style trace blame
  | null blame.observed.since = Nothing
  | otherwise = fmap (PP.annotate NoteAnn . reflow) headlineText
  where
    phrases = style.phrases
    reflow :: Text -> Doc Ann
    reflow = PP.fillSep . fmap PP.pretty . T.words

    vstep = blame.observed.step
    vrule = maybe "?" (.rule) (Trace.step trace vstep)
    vref = phrases.stepRef (T.pack (show vstep)) vrule
    -- Single-line only: a multi-line response would break the reflowing line.
    response = Phrase.firstLine <$> (Trace.step trace vstep >>= (.response))
    failMsg = Phrase.firstLine (maybe "" (.message) trace.failure)

    -- 'Nothing' when there is neither reason nor response — the headline would
    -- add nothing the splice does not show.
    headlineText :: Maybe Text
    headlineText = fmap (\r -> T.concat [phrases.failedReason vref r, phrases.terminal]) reason

    reason :: Maybe Text
    reason
      | not (T.null failMsg) = Just failMsg
      | Just r <- response = Just (phrases.returned vrule r)
      | otherwise = Nothing

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
