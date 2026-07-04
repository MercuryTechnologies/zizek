-- | The blame tree: which steps a failure cites, and why.
--
-- Intended to be imported with qualification:
--
-- > import Hegel.Report.Trace.Blame (Blame)
-- > import Hegel.Report.Trace.Blame qualified as Blame
module Hegel.Report.Trace.Blame
  ( -- * Blame
    Blame (..),
    Observation (..),
    Fact (..),

    -- * Analysis
    analyze,
    factVar,

    -- * Projections
    Citation (..),
    citations,
    citationClosure,
    hasLifecycleEvent,
  )
where

import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Hegel.Internal.Event (Operation (..), Var)
import Hegel.Report.Trace (Lifeline (..), Touch (..), Trace)
import Hegel.Report.Trace qualified as Trace

-- * Blame

-- | Why the trace failed, as a tree of cited observations.
--
-- A 'Blame' is always a /definite/ failure: a counterexample in hand.
data Blame = Blame
  { -- | The trunk value: the failing step's highest-priority variable access.
    subject :: !Var,
    -- | The failing observation and, nested beneath it, its justifications.
    observed :: !Observation
  }
  deriving stock (Show)

-- | One fact observed at one step, justified by observations beneath it.
data Observation = Observation
  { step :: !Int,
    fact :: !Fact,
    since :: [Observation]
  }
  deriving stock (Show, Eq)

-- | What a cited step did to a value.
data Fact
  = -- | Added a value to a pool.
    BornAt !Var
  | -- | Drew a value without consuming it.
    TouchedAt !Var
  | -- | Consumed a value.
    ConsumedAt !Var
  | -- | Consumed a value by transferring it to another pool (see
    -- 'Hegel.Pool.transfer').
    TransferredAt !Var
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
  -- Rank by the *fact* each touch contributes, not its raw event kind, so a
  -- consumption (the step's action) outranks an incidental reuse at the same
  -- step.
  (violation, primary) <-
    listToMaybe
      ( sortOn
          (Down . factWeight . fst)
          [(factAt trace failure.step t, t) | t <- failing.touches]
      )
  let subject = primary.var
  pure
    Blame
      { subject,
        observed =
          Observation
            { step = failure.step,
              fact = violation,
              since = citationsFor trace failure.step subject
            }
      }

-- | Display priority, for choosing subjects and deduping citations:
factWeight :: Fact -> Int
factWeight = \case
  BornAt _ -> 0
  TouchedAt _ -> 1
  TransferredAt _ -> 2
  ConsumedAt _ -> 3

-- | The value a fact concerns.
factVar :: Fact -> Var
factVar = \case
  BornAt v -> v
  TouchedAt v -> v
  ConsumedAt v -> v
  TransferredAt v -> v

-- | The fact a touch contributes at a step.
factAt :: Trace -> Int -> Touch -> Fact
factAt trace _ t = case t.kind of
  Named _ -> TouchedAt t.var -- never a step touch; unreachable
  Born _ -> BornAt t.var
  Consumed
    | Trace.continues trace t.var -> TransferredAt t.var
    | otherwise -> ConsumedAt t.var
  Reused -> TouchedAt t.var

-- | The subject's story.
--
-- The story spans the subject's entire whole lineage 'Trace.chain'; that is
-- to say, a value which was transferred cites its pre-transfer history too.
citationsFor :: Trace -> Int -> Var -> [Observation]
citationsFor trace failingStep subject =
  [ Observation {step = s, fact = e, since = []}
  | (s, e) <- Map.toDescList (Map.fromListWith strongest (mapMaybe id (birth : deaths <> touches)))
  ]
  where
    strongest :: Fact -> Fact -> Fact
    strongest a b = if factWeight a >= factWeight b then a else b
    chainLives = Trace.chainLifelines trace subject
    -- Only the chain root has a true birth; a transfer arrival's Born is
    -- represented by the ConsumedAt/TransferredAt of its source at the same
    -- step.
    birth = do
      l <- Trace.lifeline trace (Trace.root trace subject)
      b <- l.bornAt
      before b (BornAt l.var)
    deaths =
      [ before d (if Trace.continues trace l.var then TransferredAt l.var else ConsumedAt l.var)
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

-- | Does the blame tree's /cited history/ hold a lifecycle event?
-- 
-- Only the citations count, not the violation fact: the failing step is always
-- the @✗@ row, so a consume or transfer /at/ the failure draws no death\/handoff
-- geometry.
hasLifecycleEvent :: Blame -> Bool
hasLifecycleEvent b = any go b.observed.since
  where
    go :: Observation -> Bool
    go o = lifecycle o.fact || any go o.since
    lifecycle :: Fact -> Bool
    lifecycle = \case
      ConsumedAt {} -> True
      TransferredAt {} -> True
      _ -> False
