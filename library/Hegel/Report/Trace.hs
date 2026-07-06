-- | A structured description of a stateful counterexample,
-- constructed by zipping a journal's notes with a stateful pool's event stream
-- along their shared clock (see 'Hegel.Internal.Event').
--
-- Intended to be imported with qualification:
--
-- > import Hegel.Report.Trace (Trace)
-- > import Hegel.Report.Trace qualified as Trace
module Hegel.Report.Trace
  ( -- * Trace
    Trace (..),
    Step (..),
    Touch (..),
    Lifeline (..),
    Failure (..),

    -- * Construction
    build,

    -- * Queries
    step,
    lifeline,
    root,
    continues,
    chain,
    chainLifelines,
  )
where

import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Hegel.Internal.Event (Event (..), Operation (..), Var (..))
import Hegel.Internal.Tick (Tick (..))
import Hegel.Report.Note (Note (..), NoteKind (Drawn, Response, StepHeader))
import Hegel.Report.Note qualified as Note

-- * Trace

-- | A stateful counterexample:
--
-- * the steps, in order
-- * the lifeline of every pool value they touched
-- * the failure, when\/if one was journaled
data Trace = Trace
  { steps :: [Step],
    lifelines :: [Lifeline],
    failure :: !(Maybe Failure)
  }
  deriving stock (Show)

-- | One step of a state-machine's run.
data Step = Step
  { -- | Display number from the @\"Step N: \"@ header; 0 for the prelude.
    index :: !Int,
    -- | The fired rule's name; @\"\<initial\>\"@ for the prelude
    -- pseudo-rule.
    rule :: !Text,
    -- | Half-open clock window @[from, to)@; events land in the step whose
    -- window contains their stamp.
    window :: !(Tick, Tick),
    -- | The step's journal subtree in journal order.
    notes :: [Note],
    response :: !(Maybe Text),
    -- | Pool activity within the step's window, in clock-order.
    touches :: [Touch],
    -- | Rendered values of this step's draws that are /not/ bound to a 'Touch'.
    freeDraws :: ![Text],
    -- | Does this step's subtree carry the in-band 'Failure'?
    failed :: !Bool
  }
  deriving stock (Show)

-- | One pool event inside a step.
data Touch = Touch
  { var :: !Var,
    kind :: !Operation
  }
  deriving stock (Show)

-- | A pool value's story across the whole trace, in step indices.
data Lifeline = Lifeline
  { var :: !Var,
    -- | Birth order within the value's pool, 1-based.
    ordinal :: !Int,
    -- | The pool's display label ('Hegel.Pool.named'), when it has one.
    label :: !(Maybe Text),
    -- | The source var this value continues ('Hegel.Pool.transfer'): a
    -- declared identity link. Renderers resolve names and blame chains
    -- through it (see 'root').
    lineage :: !(Maybe Var),
    -- | Step containing the 'Born' event.
    bornAt :: !(Maybe Int),
    -- | Step containing the 'Consumed' event.
    consumedAt :: !(Maybe Int),
    -- | Steps with 'Reused' draws, chronological.
    touchedAt :: [Int]
  }
  deriving stock (Show)

-- | The journaled in-band failure: its step and message. (The failing step's
-- location and diff render from its spliced source, not from here.)
data Failure = Failure
  { step :: !Int,
    message :: !Text
  }
  deriving stock (Show)

-- * Construction

-- | Build the trace from a counterexample's journal and event stream.
build :: [Note] -> [Event] -> Trace
build notes events =
  Trace
    { steps = stepsOf,
      lifelines = lifelinesOf events (locateStep stepsOf),
      failure = failureOf stepsOf
    }
  where
    stepsOf = zipWith toStep segments (drop 1 (fmap windowStart segments) <> [Tick maxBound])
    segments = segment notes
    toStep seg end =
      let body = seg.body
          inWindow e = windowStart seg <= e.clock && e.clock < end
          stepEvents = [e | e <- events, inWindow e, isTouch e.kind]
          -- Note [Draw provenance]
          -- ~~~~~~~~~~~~~~~~~~~~~~~
          -- A value drawn from a pool journals its 'Drawn' note tagged with the
          -- 'Var'(s) it resolved ('Hegel.Property.Internal.forAllWith'). A note
          -- tagged with exactly one 'Var' is a pool draw already represented by
          -- that var's 'Touch'; anything else — a plain non-pool draw (tagged
          -- @[]@) or a composite multi-pool draw — is a free draw, surfaced as a
          -- detail line. The tag is what tells the two apart.
          touchVars = fmap (.var) stepEvents
          boundToTouch = \case [v] -> v `elem` touchVars; _ -> False
       in Step
            { index = segmentIndex seg,
              rule = segmentLabel seg,
              window = (windowStart seg, end),
              notes = body,
              response = listToMaybe [n.text | n <- reverse body, n.kind == Response],
              touches = [Touch {var = e.var, kind = e.kind} | e <- stepEvents],
              freeDraws = [n.text | n <- body, Drawn prov <- [n.kind], not (boundToTouch prov)],
              failed = any isFailure body
            }
    isFailure :: Note -> Bool
    isFailure n = case n.kind of Note.Failure _ -> True; _ -> False
    failureOf :: [Step] -> Maybe Failure
    failureOf steps' =
      listToMaybe
        [ Failure {step = s.index, message = n.text}
        | s <- steps',
          n <- s.notes,
          Note.Failure _ <- [n.kind]
        ]

data Segment = Segment
  { header :: !(Maybe Header),
    body :: [Note]
  }

-- | A parsed 'StepHeader' note.
data Header = Header
  { index :: !Int,
    rule :: !Text,
    start :: !Tick
  }

segmentIndex :: Segment -> Int
segmentIndex seg = maybe 0 (.index) seg.header

segmentLabel :: Segment -> Text
segmentLabel seg = maybe "<initial>" (.rule) seg.header

windowStart :: Segment -> Tick
windowStart seg = maybe (Tick 0) (.start) seg.header

-- | Split the journal at its step headers.
segment :: [Note] -> [Segment]
segment notes = case break isHeader notes of
  (prelude, rest)
    | null rest -> [Segment {header = Nothing, body = prelude}]
    | null prelude -> go rest
    | otherwise -> Segment {header = Nothing, body = prelude} : go rest
  where
    go [] = []
    go (h : rest) =
      let (body, rest') = break isHeader rest
       in Segment {header = (\(i, l) -> Header {index = i, rule = l, start = h.clock}) <$> parseHeader h, body} : go rest'
    isHeader n = n.depth == 0 && maybe False (const True) (parseHeader n)

-- | A 'StepHeader' note's structured index and rule name.
parseHeader :: Note -> Maybe (Int, Text)
parseHeader n
  | n.depth == 0, StepHeader i label <- n.kind = Just (i, label)
  | otherwise = Nothing

-- | Is this event a step activity (as opposed to out-of-band vocabulary
-- like a pool label)?
isTouch :: Operation -> Bool
isTouch = \case
  Born _ -> True
  Reused -> True
  Consumed -> True
  Named _ -> False

-- | Fold the event stream into per-value lifelines, in birth order.
lifelinesOf :: [Event] -> (Tick -> Int) -> [Lifeline]
lifelinesOf events stepAt =
  fmap tidy (reverse (foldl' apply [] events))
  where
    labels :: Map.Map Int Text
    labels = Map.fromList [(e.var.pool, l) | e <- events, Named l <- [e.kind]]
    labelOf :: Var -> Maybe Text
    labelOf v = Map.lookup v.pool labels
    tidy :: Lifeline -> Lifeline
    tidy l = l {touchedAt = reverse l.touchedAt}
    apply :: [Lifeline] -> Event -> [Lifeline]
    apply ls e = case e.kind of
      Named _ -> ls
      Born lineage ->
        Lifeline
          { var = e.var,
            ordinal = 1 + length [() | l <- ls, l.var.pool == e.var.pool],
            label = labelOf e.var,
            lineage,
            bornAt = Just (stepAt e.clock),
            consumedAt = Nothing,
            touchedAt = []
          }
          : ls
      Reused -> adjust \l -> l {touchedAt = stepAt e.clock : l.touchedAt}
      Consumed -> adjust \l -> l {consumedAt = Just (stepAt e.clock)}
      where
        adjust f = case break (\l -> l.var == e.var) ls of
          (before, l : after) -> before <> (f l : after)
          -- Malformed stream (draw of a never-born var): synthesize the
          -- lifeline with no birth rather than dropping the observation.
          (_, []) ->
            f Lifeline {var = e.var, ordinal = 0, label = labelOf e.var, lineage = Nothing, bornAt = Nothing, consumedAt = Nothing, touchedAt = []}
              : ls

-- | Which step's window contains the given 'Tick'.
locateStep :: [Step] -> Tick -> Int
locateStep steps c =
  maybe fallback (.index) (find (\s -> let (from, to) = s.window in from <= c && c < to) steps)
  where
    fallback = maybe 0 (.index) (listToMaybe steps)

-- * Queries

-- | The step with the given 'Step.index'.
step :: Trace -> Int -> Maybe Step
step t i = find (\s -> s.index == i) t.steps

-- | The lifeline of the given value.
lifeline :: Trace -> Var -> Maybe Lifeline
lifeline t v = find (\l -> l.var == v) t.lifelines

-- | The logical value's original identity: follow declared lineage
-- ('Hegel.Pool.transfer') back to the first var.
root :: Trace -> Var -> Var
root t = go []
  where
    -- The visited guard keeps 'build''s totality promise on malformed
    -- streams: a lineage cycle terminates at the first revisit.
    go seen v = case lifeline t v >>= (.lineage) of
      Just parent | parent /= v, parent `notElem` seen -> go (v : seen) parent
      _ -> v

-- | Does this var's consumption have a corresponding descendent?
continues :: Trace -> Var -> Bool
continues t v = any (\l -> l.lineage == Just v) t.lifelines

-- | Every var of the logical value: the lineage chain through @v@.
chain :: Trace -> Var -> [Var]
chain t v = go [] [root t v]
  where
    -- Breadth-first with a visited guard (same totality promise as 'root').
    go acc = \case
      [] -> reverse acc
      (x : queue)
        | x `elem` acc -> go acc queue
        | otherwise ->
            go (x : acc) (queue <> [l.var | l <- t.lifelines, l.lineage == Just x])

-- | The lifelines of every var in @v@'s lineage chain, oldest first.
chainLifelines :: Trace -> Var -> [Lifeline]
chainLifelines t v = mapMaybe (lifeline t) (chain t v)
