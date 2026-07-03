-- | A structured, versioned description of a stateful counterexample,
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
  )
where

import Control.Applicative ((<|>))
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import GHC.Stack (SrcLoc)
import Hegel.Diff (Diff)
import Hegel.Internal.Event (Clock (..), Event (..), EventKind (..), Var (..))
import Hegel.Report.Note (Note (..), NoteKind (Drawn, Response, StepHeader))
import Hegel.Report.Note qualified as Note

-- * Trace

-- | A stateful counterexample:
--
-- * the steps, in order
-- * the lifeline of every pool value they touched
-- * the failure, when\/if one was journaled
data Trace = Trace
  { -- | The "version" that this data type supports; intended to handle
    -- breaking changes in the event that we persist trace artifacts to disk
    -- and need to be mindful about how they are consumed in future versions of
    -- this library.
    version :: !Int,
    steps :: [Step],
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
    window :: !(Clock, Clock),
    -- | The step's journal subtree in journal order.
    notes :: [Note],
    response :: !(Maybe Text),
    -- | Pool activity within the step's window, in clock-order.
    touches :: [Touch],
    -- | Does this step's subtree carry the in-band 'Failure'?
    failed :: !Bool
  }
  deriving stock (Show)

-- | One pool event inside a step, correlated (when possible) with the
-- journaled draw that consumed the engine's reply.
data Touch = Touch
  { var :: !Var,
    kind :: !EventKind,
    -- | The clock-adjacent 'Drawn' note, if one exists.
    --
    -- A pool draw runs inside the generator, strictly before @forAll@ journals,
    -- so the note whose clock immediately follows the event's is the draw's
    -- rendering.
    note :: !(Maybe Note)
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
    touchedAt :: [Int],
    -- | Touches at steps after 'consumedAt', not currently supported by @hegel@.
    posthumous :: [Int]
  }
  deriving stock (Show)

-- | The journaled in-band failure, located to its step.
data Failure = Failure
  { step :: !Int,
    message :: !Text,
    loc :: !(Maybe SrcLoc),
    diff :: !(Maybe Diff)
  }
  deriving stock (Show)

-- * Construction

-- | The current 'Trace.version'.
currentVersion :: Int
currentVersion = 1

-- | Build the trace from a counterexample's journal and event stream.
build :: [Note] -> [Event] -> Trace
build notes events =
  Trace
    { version = currentVersion,
      steps = stepsOf,
      lifelines = lifelinesOf events (locateStep stepsOf),
      failure = failureOf stepsOf
    }
  where
    stepsOf = zipWith toStep segments (drop 1 (fmap windowStart segments) <> [Clock maxBound])
    segments = segment notes
    toStep seg end =
      let body = seg.body
       in Step
            { index = segmentIndex seg,
              rule = segmentLabel seg,
              window = (windowStart seg, end),
              notes = body,
              response = listToMaybe [n.text | n <- reverse body, n.kind == Response],
              touches =
                [ Touch
                    { var = e.var,
                      kind = e.kind,
                      note = correlatedNote body e
                    }
                | e <- events,
                  windowStart seg <= e.clock && e.clock < end,
                  isTouch e.kind
                ],
              failed = any isFailure body
            }
    isFailure :: Note -> Bool
    isFailure n = case n.kind of Note.Failure _ -> True; _ -> False
    failureOf :: [Step] -> Maybe Failure
    failureOf steps' =
      listToMaybe
        [ Failure {step = s.index, message = n.text, loc = n.loc, diff = d}
        | s <- steps',
          n <- s.notes,
          Note.Failure d <- [n.kind]
        ]

data Segment = Segment
  { header :: !(Maybe Header),
    body :: [Note]
  }

-- | A parsed 'StepHeader' note.
data Header = Header
  { index :: !Int,
    rule :: !Text,
    start :: !Clock
  }

segmentIndex :: Segment -> Int
segmentIndex seg = maybe 0 (.index) seg.header

segmentLabel :: Segment -> Text
segmentLabel seg = maybe "<initial>" (.rule) seg.header

windowStart :: Segment -> Clock
windowStart seg = maybe (Clock 0) (.start) seg.header

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
isTouch :: EventKind -> Bool
isTouch = \case
  Born _ -> True
  Reused -> True
  Consumed -> True
  Named _ -> False

-- | The journaled 'Drawn' note for a pool /draw/ event, by clock adjacency.
--
-- Only draws correlate — a 'Born' is a 'Hegel.Pool.add', not a draw, and any
-- note adjacent to it belongs to something else. A transfer's consuming draw
-- is followed first by its lineage 'Born' (same composite operation), so its
-- note sits one clock further along.
correlatedNote :: [Note] -> Event -> Maybe Note
correlatedNote body e = case e.kind of
  Reused -> at (succ e.clock)
  Consumed -> at (succ e.clock) <|> at (succ (succ e.clock))
  _ -> Nothing
  where
    at c = find (\n -> n.kind == Drawn && n.clock == c) body

-- | Fold the event stream into per-value lifelines, in birth order.
--
-- Explicit fold state: lifelines accumulate in reverse birth order (a
-- structural guarantee, not a clever map key), with per-pool counts giving
-- ordinals; touch lists accumulate reversed for cheap appends and are
-- straightened at the end.
lifelinesOf :: [Event] -> (Clock -> Int) -> [Lifeline]
lifelinesOf events stepAt =
  fmap tidy (reverse (foldl' apply [] events))
  where
    labels :: Map.Map Int Text
    labels = Map.fromList [(e.var.pool, l) | e <- events, Named l <- [e.kind]]
    labelOf :: Var -> Maybe Text
    labelOf v = Map.lookup v.pool labels
    tidy :: Lifeline -> Lifeline
    tidy l = l {touchedAt = reverse l.touchedAt, posthumous = reverse l.posthumous}
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
            touchedAt = [],
            posthumous = []
          }
          : ls
      Reused -> adjust \l -> l {touchedAt = stepAt e.clock : l.touchedAt}
      Consumed -> adjust \l -> l {consumedAt = Just (stepAt e.clock)}
      where
        adjust f = case break (\l -> l.var == e.var) ls of
          (before, l : after) -> before <> (f' l : after)
            where
              -- A touch after death is posthumous (impossible via engine
              -- pool draws today; see 'Lifeline.posthumous').
              f' = case (e.kind, l.consumedAt) of
                (Reused, Just _) -> \l' -> l' {posthumous = stepAt e.clock : l'.posthumous}
                _ -> f
          -- Malformed stream (draw of a never-born var): synthesize the
          -- lifeline with no birth rather than dropping the observation.
          (_, []) ->
            f Lifeline {var = e.var, ordinal = 0, label = labelOf e.var, lineage = Nothing, bornAt = Nothing, consumedAt = Nothing, touchedAt = [], posthumous = []}
              : ls

-- | Which step's window contains this clock stamp. Falls back to the
-- earliest step (never a step index absent from the trace): a journal whose
-- first note is a 'StepHeader' has no prelude segment, but events stamped
-- before that header must still land on a step that renders.
locateStep :: [Step] -> Clock -> Int
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
-- ('Hegel.Pool.transfer') back to the first var. Display names resolve
-- here, so a transferred value keeps one name across pools.
root :: Trace -> Var -> Var
root t = go []
  where
    -- The visited guard keeps 'build''s totality promise on malformed
    -- streams: a lineage cycle terminates at the first revisit.
    go seen v = case lifeline t v >>= (.lineage) of
      Just parent | parent /= v, parent `notElem` seen -> go (v : seen) parent
      _ -> v

-- | Does this var's consumption continue into a lineage-linked descendant
-- (i.e. was it a 'Hegel.Pool.transfer', not a death)? The single home of the
-- transfer-vs-death distinction: 'Hegel.Report.Blame' words it,
-- 'Hegel.Report.Ledger' draws it.
continues :: Trace -> Var -> Bool
continues t v = any (\l -> l.lineage == Just v) t.lifelines

-- | Every var of the logical value: the lineage chain through @v@, oldest
-- first (ancestors, @v@, and any descendants declared later).
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
