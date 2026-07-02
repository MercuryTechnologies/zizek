-- | The verdict headline: the blame tree's failing observation and its
-- observed outcome, worded as one line — the report's one-sentence statement
-- of what broke. The per-step justifications are not repeated here; the
-- citation ledger renders each cited fact at its arrowhead.
--
-- Intended to be imported with qualification:
--
-- > import Hegel.Report.Verdict qualified as Verdict
module Hegel.Report.Verdict
  ( verdictDoc,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Report.Ann (Ann (..))
import Hegel.Report.Blame (Blame (..), Observation (..))
import Hegel.Report.Phrase (PhraseTable (..))
import Hegel.Report.Phrase qualified as Phrase
import Hegel.Report.Style (Style (..))
import Hegel.Report.Trace (Step (..), Trace)
import Hegel.Report.Trace qualified as Trace
import Prettyprinter (Doc)
import Prettyprinter qualified as PP

-- * Rendering

-- | Word the verdict as a single reflowing headline. The failing fact is a
-- benign pool access (the value is incidental to the failure), so lead with
-- the failure reason: @\"Step 5 (verify): expected Nothing.\"@
--
-- The justifications are not worded here — every cited fact is already at its
-- arrowhead in the citation ledger just below.
--
-- 'Nothing' when the blame tree has no citations: with nothing to justify,
-- the headline alone adds nothing the ledger's failing row does not already
-- say (the composed report's degradation row).
verdictDoc :: Style -> Trace -> Blame -> Maybe (Doc Ann)
verdictDoc style trace blame
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

    -- Lead with the failure reason. 'Nothing' when there is neither reason nor
    -- response — the headline would add nothing the splice does not show.
    headlineText :: Maybe Text
    headlineText = fmap (\r -> T.concat [phrases.failedReason vref r, phrases.terminal]) reason

    -- The reason: the failure message, else the observed response.
    reason :: Maybe Text
    reason
      | not (T.null failMsg) = Just failMsg
      | Just r <- response = Just (phrases.returned vrule r)
      | otherwise = Nothing
