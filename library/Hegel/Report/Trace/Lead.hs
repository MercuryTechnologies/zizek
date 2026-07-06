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
    trajectory,
    trajectorySteps,
  )
where

import Data.IntSet qualified as IntSet
import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Internal.Event (Var)
import Hegel.Report.Ann (Ann (..))
import Hegel.Report.Glyph (Cell (..), GlyphTable (..), displayName)
import Hegel.Report.Phrase (PhraseTable (..))
import Hegel.Report.Style (Style (..))
import Hegel.Report.Trace (Lifeline (..), Step (..), Trace)
import Hegel.Report.Trace qualified as Trace
import Hegel.Report.Trace.Blame (Blame (..), Observation (..))
import Prettyprinter (Doc)
import Prettyprinter qualified as PP

-- | Every step a value's lineage (across 'Hegel.Pool.transfer') touched — its
-- 'Born', 'Reused', and 'Consumed' steps — paired with the rule name, distinct
-- and chronological, excluding the prelude (step 0 is machine setup, labelled
-- @\<initial\>@ — not a rule the reader can navigate to).
--
-- The shared core of 'leadDoc' (the degraded whole-report lead) and the spine's
-- elided-lifeline footer.
trajectorySteps :: Trace -> Var -> [(Int, Text)]
trajectorySteps trace v =
  [(i, s.rule) | i <- stepIxs, Just s <- [Trace.step trace i]]
  where
    stepIxs =
      IntSet.toAscList . IntSet.fromList $
        [ i
        | l <- Trace.chainLifelines trace v,
          i <- maybe [] pure l.bornAt <> l.touchedAt <> maybe [] pure l.consumedAt,
          i > 0
        ]

-- | A value's history as a single line — @name: rule \@i · rule \@i@ — or
-- 'Nothing' when its lineage touched no navigable step. Used per elided
-- lifeline in the spine footer.
trajectory :: Style -> Trace -> Var -> Maybe Text
trajectory style trace v = case trajectorySteps trace v of
  [] -> Nothing
  entries ->
    Just (style.phrases.trajectory (displayName style.glyphs trace v) (labelled entries))

-- | Pair each step with its index rendered for the phrase table.
labelled :: [(Int, Text)] -> [(Text, Text)]
labelled entries = [(r, T.pack (show i)) | (i, r) <- entries]

-- | The failing value's history as a one-line lead, or 'Nothing' when it
-- passed through no step other than the failing one.
leadDoc :: Style -> Trace -> Blame -> Maybe (Doc Ann)
leadDoc style trace blame
  | null entries = Nothing
  | otherwise = Just (PP.annotate NoteAnn (PP.pretty line))
  where
    -- 'displayName' resolves the lineage root itself (as in Spine/Compose),
    -- so pass the raw subject rather than pre-rooting it.
    name = displayName style.glyphs trace blame.subject
    -- The failing step is the header above the lead, so drop it here.
    entries = filter ((/= blame.observed.step) . fst) (trajectorySteps trace blame.subject)
    line =
      style.glyphs.cell TrajectoryLead
        <> " "
        <> style.phrases.trajectory name (labelled entries)
