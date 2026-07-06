-- | The blame tree: which steps a failure cites, and why.
--
-- Intended to be imported with qualification:
--
-- > import Hegel.Report.Trace.Blame (Blame)
-- > import Hegel.Report.Trace.Blame qualified as Blame
module Hegel.Report.Trace.Blame
  ( -- * Blame
    Blame (..),
    Claim (..),
    Observation (..),
    Fact (..),

    -- * Analysis
    analyze,
    factVar,
    factWeight,

    -- * Projections
    Citation (..),
    citations,
    citationClosure,
    primary,
  )
where

import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import Hegel.Internal.Event (Operation (..), Var)
import Hegel.Report.Trace (Lifeline (..), Touch (..), Trace)
import Hegel.Report.Trace qualified as Trace

-- * Blame

-- | Why the trace failed: the failing step, and one 'Claim' per distinct
-- lineage root it touched — each the story of a value the failure implicates.
--
-- A 'Blame' is always a /definite/ failure: a counterexample in hand.
--
-- Multi-root by construction: a step like @settle a₂ a₁@ that touches two
-- independent values yields two claims, so both are cited. Claims are ordered
-- by descending fact weight; the 'primary' (head) is the trunk value used
-- where a single name is required (the focused view). K=1 (one root) is the
-- common case and renders exactly as the single-subject design did.
--
-- Note [Known limitation: ambient values]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A long-lived context/config/session value touched at /every/ step cannot
-- shrink away, so structural union blames it on every row and its citation set
-- covers everything — a citation that selects all selects nothing. It is
-- detectable as "non-discriminating" (every fact 'TouchedAt', touched at every
-- step), but suppressing it must NOT key on fact weight: the future causal case
-- (@read h₂ ← close h₁@) blames a merely-/touched/ victim, exactly what a
-- weight filter would drop. Left unhandled until a real case earns the fix.
--
-- Note [Deferred: causal (data-flow) blame]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- This module cites every value the failing step /touches/. It does not cite a
-- value whose earlier operation /caused/ the failure without being touched at
-- the failing step (@read h₂@ returns garbage because @close h₁@ broke a
-- promise). That is a different mechanism: it needs a cross-value dependency
-- edge, which — following the 'Hegel.Pool.transfer' lineage precedent — should
-- be a /declared/ edge (a @dependsOn@-style journaled link), not inferred from
-- the trace. 'Observation' already admits a fact whose var differs from its
-- claim's root, so the rendering vocabulary here extends to it unchanged.
data Blame = Blame
  { -- | The failing step.
    step :: !Int,
    -- | One claim per distinct lineage root touched at the failing step,
    -- ordered by descending fact weight (head is the 'primary').
    subjects :: !(NonEmpty Claim)
  }
  deriving stock (Show)

-- | One implicated value's story: its fact at the failing step, and the
-- observations justifying it (its earlier lineage history).
data Claim = Claim
  { fact :: !Fact,
    since :: [Observation]
  }
  deriving stock (Show)

-- | One fact observed at one step — an entry in a 'Claim'\'s history.
data Observation = Observation
  { step :: !Int,
    fact :: !Fact
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

-- | Analyze a trace's failure into a blame.
--
-- One 'Claim' per distinct lineage root touched at the failing step (each root's
-- strongest fact plus its lineage history), ordered by descending fact weight.
--
-- 'Nothing' when there is nothing to cite: no journaled failure, or a
-- failing step that touched no pool values.
analyze :: Trace -> Maybe Blame
analyze trace = do
  failure <- trace.failure
  failing <- Trace.step trace failure.step
  -- One representative touch per lineage root, ranked by the *fact* it
  -- contributes (not its raw event kind), so a consumption (the step's action)
  -- outranks an incidental reuse at the same step. Distinct roots stay distinct
  -- — a two-operand step yields a claim each.
  claims <-
    NE.nonEmpty
      ( sortOn
          (Down . factWeight . (.fact))
          [ Claim {fact = violation, since = citationsFor trace failure.step rootVar}
          | (rootVar, violation) <- Map.toList (perRoot failing.touches)
          ]
      )
  pure Blame {step = failure.step, subjects = claims}
  where
    -- Collapse the failing step's touches to one strongest fact per lineage
    -- root; the key is the root var so each independent value is kept.
    perRoot touches =
      Map.fromListWith
        strongest
        [ (Trace.root trace t.var, factAt trace 0 t)
        | t <- touches
        ]
    strongest a b = if factWeight a >= factWeight b then a else b

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
  [ Observation {step = s, fact = e}
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

-- | Every claim's edges, flattened: the failing step cites each observation in
-- each claim's history.
citations :: Blame -> [Citation]
citations b =
  [ Citation {from = b.step, to = o.step, fact = o.fact}
  | c <- NE.toList b.subjects,
    o <- c.since
  ]

-- | Every step the blame reaches: the failing step plus every claim's
-- observation history.
citationClosure :: Blame -> IntSet
citationClosure b =
  IntSet.fromList (b.step : [o.step | c <- NE.toList b.subjects, o <- c.since])

-- | The trunk value: the highest-weight claim's value. Used where a single name
-- is required (the focused view, K=1).
primary :: Blame -> Var
primary b = factVar (NE.head b.subjects).fact
