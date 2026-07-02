-- | The trajectory lead: a degraded report's compact, one-line history of the
-- failing value.
--
-- When a stateful failure's blame tree has no lifecycle event (no death or
-- handoff — see 'Hegel.Report.Blame.hasLifecycleEvent'), the ledger's geometry
-- would only draw a flat born+touch biography, so the composed report degrades
-- to the step timeline plus this lead. It reads:
--
-- > ↳ p₁: open @1 · write @3 · close @4
--
-- Spelled in /rule names/ (verbatim, the same token as the timeline's
-- @Step N: rule@ and the user's @'Hegel.Stateful.Rule' \"open\"@) so each
-- @\@N@ cross-references the timeline and the rule name points back to source.
-- Chronological, and excluding the failing step (already the header above it).
--
-- Intended to be imported with qualification:
--
-- > import Hegel.Report.Trajectory qualified as Trajectory
module Hegel.Report.Trajectory
  ( trajectoryDoc,
  )
where

import Data.IntSet qualified as IntSet
import Data.Text qualified as T
import Hegel.Report.Ann (Ann (..))
import Hegel.Report.Blame (Blame (..), Observation (..))
import Hegel.Report.Glyph (Cell (..), GlyphTable (..), displayName)
import Hegel.Report.Phrase (PhraseTable (..))
import Hegel.Report.Style (Style (..))
import Hegel.Report.Trace (Lifeline (..), Step (..), Trace)
import Hegel.Report.Trace qualified as Trace
import Prettyprinter (Doc)
import Prettyprinter qualified as PP

-- | The failing value's trajectory as a one-line lead, or 'Nothing' when it
-- passed through no step other than the failing one.
trajectoryDoc :: Style -> Trace -> Blame -> Maybe (Doc Ann)
trajectoryDoc style trace blame
  | null entries = Nothing
  | otherwise = Just (PP.annotate NoteAnn (PP.pretty line))
  where
    glyphs = style.glyphs
    phrases = style.phrases
    -- 'displayName' resolves the lineage root itself (as in Ledger/Verdict),
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
