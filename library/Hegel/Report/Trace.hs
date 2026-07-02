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
  )
where

import Data.List (find)
import Data.Map.Strict (Map)
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
                      note = find (\n -> n.kind == Drawn && n.clock == succ e.clock) body
                    }
                | e <- events,
                  windowStart seg <= e.clock && e.clock < end
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
  { header :: !(Maybe (Int, Text, Clock)),
    body :: [Note]
  }

segmentIndex :: Segment -> Int
segmentIndex seg = maybe 0 (\(i, _, _) -> i) seg.header

segmentLabel :: Segment -> Text
segmentLabel seg = maybe "<initial>" (\(_, l, _) -> l) seg.header

windowStart :: Segment -> Clock
windowStart seg = maybe (Clock 0) (\(_, _, c) -> c) seg.header

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
       in Segment {header = (\(i, l) -> (i, l, h.clock)) <$> parseHeader h, body} : go rest'
    isHeader n = n.depth == 0 && maybe False (const True) (parseHeader n)

-- | A 'StepHeader' note's structured index and rule name.
parseHeader :: Note -> Maybe (Int, Text)
parseHeader n
  | n.depth == 0, StepHeader i label <- n.kind = Just (i, label)
  | otherwise = Nothing

-- | Fold the event stream into per-value lifelines, in birth order.
lifelinesOf :: [Event] -> (Clock -> Int) -> [Lifeline]
lifelinesOf events stepAt = Map.elems (foldl' apply Map.empty events)
  where
    -- Keyed by (birth sequence, var) so Map.elems yields birth order.
    apply :: Map (Int, Var) Lifeline -> Event -> Map (Int, Var) Lifeline
    apply m e = case e.kind of
      Born ->
        let ordinal = 1 + length [() | (_, v) <- Map.keys m, v.pool == e.var.pool]
         in Map.insert
              (Map.size m, e.var)
              Lifeline
                { var = e.var,
                  ordinal,
                  bornAt = Just (stepAt e.clock),
                  consumedAt = Nothing,
                  touchedAt = [],
                  posthumous = []
                }
              m
      Reused -> adjust \l -> l {touchedAt = l.touchedAt <> [stepAt e.clock]}
      Consumed -> adjust \l -> l {consumedAt = Just (stepAt e.clock)}
      where
        adjust f = case find (\(_, v) -> v == e.var) (Map.keys m) of
          Just k -> Map.adjust f' k m
            where
              -- A touch after death is posthumous (impossible via engine
              -- pool draws today; see 'Lifeline.posthumous').
              f' l = case (e.kind, l.consumedAt) of
                (Reused, Just _) -> l {posthumous = l.posthumous <> [stepAt e.clock]}
                _ -> f l
          -- Malformed stream (draw of a never-born var): synthesize the
          -- lifeline with no birth rather than dropping the observation.
          Nothing ->
            Map.insert
              (Map.size m, e.var)
              (f Lifeline {var = e.var, ordinal = 0, bornAt = Nothing, consumedAt = Nothing, touchedAt = [], posthumous = []})
              m

-- | Which step's window contains this clock stamp.
locateStep :: [Step] -> Clock -> Int
locateStep steps c =
  maybe 0 (.index) (find (\s -> let (from, to) = s.window in from <= c && c < to) steps)

-- * Queries

-- | The step with the given 'Step.index'.
step :: Trace -> Int -> Maybe Step
step t i = find (\s -> s.index == i) t.steps

-- | The lifeline of the given value.
lifeline :: Trace -> Var -> Maybe Lifeline
lifeline t v = find (\l -> l.var == v) t.lifelines
