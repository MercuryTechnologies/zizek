-- | The lead: a one-line history of the failing value in a stateful test.
--
-- When a stateful failure's blame tree has no lifecycle event, the spine's
-- geometry would be redundant; instead it reads:
--
-- > ↳ p₁: open @1 · write @3 · close @4
--
-- Intended to be imported with qualification:
--
-- > import Hegel.Report.Trace.Lead qualified as Lead
module Hegel.Report.Trace.Lead
  ( leadDoc,
  )
where

import Data.IntSet qualified as IntSet
import Data.Text qualified as T
import Hegel.Report.Ann (Ann (..))
import Hegel.Report.Glyph (Cell (..), GlyphTable (..), displayName)
import Hegel.Report.Phrase (PhraseTable (..))
import Hegel.Report.Style (Style (..))
import Hegel.Report.Trace (Lifeline (..), Step (..), Trace)
import Hegel.Report.Trace qualified as Trace
import Hegel.Report.Trace.Blame (Blame (..), Observation (..))
import Prettyprinter (Doc)
import Prettyprinter qualified as PP

-- | The failing value's history as a one-line lead, or 'Nothing' when it
-- passed through no step other than the failing one.
leadDoc :: Style -> Trace -> Blame -> Maybe (Doc Ann)
leadDoc style trace blame
  | null entries = Nothing
  | otherwise = Just (PP.annotate NoteAnn (PP.pretty line))
  where
    glyphs = style.glyphs
    phrases = style.phrases
    -- 'displayName' resolves the lineage root itself (as in Spine/Compose),
    -- so pass the raw subject rather than pre-rooting it.
    name = displayName glyphs trace blame.subject
    failingStep = blame.observed.step

    -- Every step the subject's lineage (across 'Hegel.Pool.transfer') touched,
    -- minus the failing step (already the header above) and the prelude (step
    -- 0 is machine setup, labelled @\<initial\>@ — not a rule the reader can
    -- navigate to); distinct and chronological.
    lives = Trace.chainLifelines trace blame.subject
    stepIxs =
      IntSet.toAscList . IntSet.fromList $
        [ i
        | l <- lives,
          i <- maybe [] pure l.bornAt <> l.touchedAt <> maybe [] pure l.consumedAt,
          i > 0,
          i /= failingStep
        ]
    entries = [(i, s.rule) | i <- stepIxs, Just s <- [Trace.step trace i]]

    line =
      glyphs.cell TrajectoryLead
        <> " "
        <> phrases.trajectory name [(r, T.pack (show i)) | (i, r) <- entries]
