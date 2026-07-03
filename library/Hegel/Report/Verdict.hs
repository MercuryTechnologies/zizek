-- | The verdict paragraph: the blame tree worded as a short prose proof —
-- the failing observation, its justifications as \"since\" clauses, and the
-- observed outcome.
--
-- Intended to be imported with qualification:
--
-- > import Hegel.Report.Verdict qualified as Verdict
module Hegel.Report.Verdict
  ( -- * The plan
    Clause (..),
    plan,

    -- * Rendering
    verdictDoc,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Report.Ann (Ann (..))
import Hegel.Report.Blame (Blame (..), Fact (..), Observation (..))
import Hegel.Report.Glyph (GlyphTable, displayName)
import Hegel.Report.Phrase (PhraseTable (..))
import Hegel.Report.Trace (Step (..), Trace)
import Hegel.Report.Trace qualified as Trace
import Prettyprinter (Doc)
import Prettyprinter qualified as PP

-- * The plan

-- | One clause of the verdict, wordless: which step, which rule, which fact.
-- User text ('Returned', 'FailedWith') is carried verbatim for quoting.
data Clause
  = -- | The violation: what the failing step did.
    Violated {step :: !Int, rule :: !Text, fact :: !Fact}
  | -- | A justification: what a cited step did, earlier.
    Since {step :: !Int, rule :: !Text, fact :: !Fact}
  | -- | The observed outcome, from the failing rule's
    -- 'Hegel.Stateful.respond'.
    Returned {rule :: !Text, response :: !Text}
  | -- | The observed outcome, from the journaled failure message (when the
    -- failing rule declared no response).
    FailedWith {message :: !Text}
  deriving stock (Show, Eq)

-- | Project the blame tree to its verdict plan: the violation, one 'Since'
-- per citation (most recent first, as in the tree), and the outcome.
plan :: Trace -> Blame -> [Clause]
plan trace blame =
  Violated
    { step = blame.observed.step,
      rule = ruleOf blame.observed.step,
      fact = blame.observed.fact
    }
    : [Since {step = o.step, rule = ruleOf o.step, fact = o.fact} | o <- blame.observed.since]
      <> [outcome]
  where
    failing = Trace.step trace blame.observed.step
    ruleOf i = maybe "?" (.rule) (Trace.step trace i)
    outcome = case failing >>= (.response) of
      Just r -> Returned {rule = ruleOf blame.observed.step, response = r}
      Nothing -> FailedWith {message = maybe "" (.message) trace.failure}

-- * Rendering

-- | Word the verdict plan as a reflowing paragraph.
--
-- 'Nothing' when the blame tree has no citations: with nothing to justify,
-- the headline suffices (the composed report's degradation row).
verdictDoc :: PhraseTable -> GlyphTable -> Trace -> Blame -> Maybe (Doc Ann)
verdictDoc phrases glyphs trace blame
  | null blame.observed.since = Nothing
  | otherwise =
      Just
        ( PP.annotate
            NoteAnn
            (PP.fillSep (fmap PP.pretty (T.words (paragraph phrases glyphs trace blame))))
        )

-- | The paragraph as one 'Text' (also what the pins check).
paragraph :: PhraseTable -> GlyphTable -> Trace -> Blame -> Text
paragraph phrases glyphs trace blame =
  mconcat (lead : causes <> outcome <> [phrases.terminal])
  where
    clauses = plan trace blame
    nameOf = displayName glyphs trace
    factName fact = case fact of
      BornAt v -> nameOf v
      TouchedAt v -> nameOf v
      ConsumedAt v -> nameOf v
      HauntedAt v -> nameOf v
    ref i rule = phrases.stepRef (T.pack (show i)) rule
    lead =
      mconcat
        [ phrases.lead (ref st r) (phrases.violates f (factName f))
        | Violated {step = st, rule = r, fact = f} <- clauses
        ]
    causes = case [(st, r, f) | Since {step = st, rule = r, fact = f} <- clauses] of
      [] -> []
      cs ->
        [ phrases.causeIntro
            <> T.intercalate
              phrases.causeSep
              [phrases.caused f (factName f) <> phrases.at <> ref st r | (st, r, f) <- cs]
        ]
    outcome =
      [ phrases.but <> worded
      | c <- clauses,
        worded <- case c of
          Returned {rule, response} -> [phrases.returned rule response]
          FailedWith {message}
            | not (T.null message) -> [phrases.failedWith (firstLine message)]
            | otherwise -> []
          _ -> []
      ]
    firstLine = T.takeWhile (/= '\n')
