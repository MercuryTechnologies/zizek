-- | The blame tree: which steps a failure cites, and why.
--
-- Intended to be imported with qualification:
--
-- > import Hegel.Report.Blame (Blame)
-- > import Hegel.Report.Blame qualified as Blame
module Hegel.Report.Blame
  ( -- * Blame
    Blame (..),
    Observation (..),
    Fact (..),
    Phenomenon (..),

    -- * Analysis
    analyze,

    -- * Projections
    Citation (..),
    citations,
    citationClosure,
  )
where

import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (sortOn)
import Data.List qualified as List
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Hegel.Internal.Event (EventKind (..), Var)
import Hegel.Report.Trace (Lifeline (..), Touch (..), Trace)
import Hegel.Report.Trace qualified as Trace

-- * Blame

-- | Why the trace failed, as a tree of cited observations.
--
-- A 'Blame' is always a /definite/ failure: a counterexample in hand.
-- Verdict strength (definitely\/probably false, the RV-LTL vocabulary)
-- enters this record together with the obligation API — a probably-false
-- verdict will want the open obligation as payload, so its shape is not
-- guessed at here.
data Blame = Blame
  { -- | The trunk value: the failing step's most causally loaded touch; owns
    -- the leftmost lane once the multi-lane ledger exists.
    subject :: !Var,
    -- | The failing observation and, nested beneath it, its justifications.
    observed :: !Observation,
    -- | The matched failure phenomenon, when the fact shape names one.
    diagnosis :: !(Maybe Phenomenon)
  }
  deriving stock (Show)

-- | One fact observed at one step, justified by the observations beneath
-- it.
--
-- The root observation is the violation; its 'since' children are the citations,
-- while deeper nesting is the reserved shape for indirect chains like.
data Observation = Observation
  { step :: !Int,
    fact :: !Fact,
    since :: [Observation]
  }
  deriving stock (Show, Eq)

-- | What a cited step did to a value.
--
-- Rendered as an edge by the ledger and as a deontic\/indicative clause by the
-- verdict paragraph.
data Fact
  = -- | Added it to a pool.
    BornAt !Var
  | -- | Drew it without consuming.
    TouchedAt !Var
  | -- | Consumed it; the value's death.
    ConsumedAt !Var
  | -- | Touched it /after/ its death: the step is haunted by a value
    -- consumed earlier.
    HauntedAt !Var
  deriving stock (Show, Eq)

-- | Named failure phenomena detectable from fact shape alone.
data Phenomenon = UseAfterConsume
  deriving stock (Show, Eq)

-- * Analysis

-- | Analyze a trace's failure into a blame tree.
--
-- 'Nothing' when there is nothing to cite: no journaled failure, or a
-- failing step that touched no pool values.
analyze :: Trace -> Maybe Blame
analyze trace = do
  failure <- trace.failure
  failing <- Trace.step trace failure.step
  primary <- listToMaybe (sortOn (Down . causalWeight) failing.touches)
  let subject = primary.var
      violation = factAt trace failure.step primary
  pure
    Blame
      { subject,
        observed =
          Observation
            { step = failure.step,
              fact = violation,
              since = citationsFor trace failure.step subject
            },
        diagnosis = case violation of
          HauntedAt _ -> Just UseAfterConsume
          _ -> Nothing
      }

-- | Rank a failing step's touches by causal weight:
--
-- * a posthumous touch names the bug outright
-- * a consumption is the step's action
-- * a reuse observes
-- * a birth instantiates
causalWeight :: Touch -> Int
causalWeight t = case t.kind of
  Named _ -> -1 -- never a step touch; unreachable
  Born _ -> 0
  Reused -> 1
  Consumed -> 2

-- | The fact a touch contributes at a step, upgrading a reuse of a dead
-- value to 'HauntedAt'.
factAt :: Trace -> Int -> Touch -> Fact
factAt trace at t = case t.kind of
  Named _ -> TouchedAt t.var -- never a step touch; unreachable
  Born _ -> BornAt t.var
  Consumed -> ConsumedAt t.var
  Reused
    | Just l <- Trace.lifeline trace t.var,
      Just died <- l.consumedAt,
      died < at ->
        HauntedAt t.var
    | otherwise -> TouchedAt t.var

-- | The subject's earlier story, starting from the most recent 'Observation'.
-- The subject's story is its whole lineage chain ('Trace.chain'): a
-- transferred value cites its pre-transfer history too.
citationsFor :: Trace -> Int -> Var -> [Observation]
citationsFor trace failingStep subject =
  [ Observation {step = s, fact = e, since = []}
  | (s, e) <- dedupe (sortOn (Down . fst) (mapMaybe id (birth : deaths <> touches)))
  ]
  where
    chainLives = mapMaybe (Trace.lifeline trace) (Trace.chain trace subject)
    -- Only the chain root has a true birth; a transfer arrival's Born is
    -- represented by the ConsumedAt of its source at the same step.
    birth = do
      l <- Trace.lifeline trace (Trace.root trace subject)
      b <- l.bornAt
      before b (BornAt l.var)
    deaths =
      [ before d (ConsumedAt l.var)
      | l <- chainLives,
        Just d <- [l.consumedAt]
      ]
    touches =
      [ before t (TouchedAt l.var)
      | l <- chainLives,
        t <- l.touchedAt
      ]
    before s e
      | s < failingStep = Just (s, e)
      | otherwise = Nothing
    -- One citation per step: the ledger draws one rail edge per cited row,
    -- so multiple facts at one step (two draws, or a transfer's consume)
    -- keep only the most causally loaded (facts sort Born < Touched <
    -- Consumed < Haunted by construction order).
    dedupe :: [(Int, Fact)] -> [(Int, Fact)]
    dedupe = fmap strongest . groupOn fst
      where
        strongest :: [(Int, Fact)] -> (Int, Fact)
        strongest xs = last (sortOn (factWeight . snd) xs)
        groupOn :: (Eq b) => ((Int, Fact) -> b) -> [(Int, Fact)] -> [[(Int, Fact)]]
        groupOn f = List.groupBy (\a b -> f a == f b)
        factWeight = \case
          BornAt _ -> 0 :: Int
          TouchedAt _ -> 1
          ConsumedAt _ -> 2
          HauntedAt _ -> 3

-- * Projections

-- | One flattened blame edge: @from@ cites @to@ for @fact@.
data Citation = Citation
  { from :: !Int,
    to :: !Int,
    fact :: !Fact
  }
  deriving stock (Show, Eq)

-- | The blame tree's edges, flattened pre-order.
citations :: Blame -> [Citation]
citations b = go b.observed
  where
    go :: Observation -> [Citation]
    go p = [Citation {from = p.step, to = c.step, fact = c.fact} | c <- p.since] <> concatMap go p.since

-- | The revset: every step the blame tree reaches.
--
-- The ledger shows exactly these steps and elides the rest.
citationClosure :: Blame -> IntSet
citationClosure b = go b.observed
  where
    go :: Observation -> IntSet
    go p = IntSet.insert p.step (IntSet.unions (fmap go p.since))
