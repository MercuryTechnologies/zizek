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
    Identity (..),
    Failure (..),

    -- * Construction
    build,

    -- * Queries
    step,
    identity,
    root,
  )
where

import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Hegel.Internal.Event (Event (..), Operation (..), Var (..))
import Hegel.Internal.Tick (Tick (..))
import Hegel.Report.Note (Note (..), NoteKind (Drawn, Response, StepHeader))
import Hegel.Report.Note qualified as Note

-- * Trace

-- | A stateful counterexample:
--
-- * the steps, in order
-- * the identity of every pool value they touched
-- * the failure, when\/if one was journaled
data Trace = Trace
  { steps :: [Step],
    identities :: [Identity],
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

-- | A pooled value's identity: its birth order and display label, and the
-- source var it continues across a 'Hegel.Pool.transfer'.
data Identity = Identity
  { var :: !Var,
    -- | Birth order within the value's pool, 1-based.
    ordinal :: !Int,
    -- | The pool's display label ('Hegel.Pool.named'), when it has one.
    label :: !(Maybe Text),
    -- | The source var this value continues ('Hegel.Pool.transfer'): a
    -- declared identity link. 'root' resolves names through it.
    lineage :: !(Maybe Var)
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
      identities = identitiesOf events,
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

-- | Fold the event stream into per-value identities, in birth order,
-- contributed only by 'Born' events. A 'Reused' or 'Consumed' event carries
-- no identity information of its own, so it's captured in 'Step.touches'
-- instead.
identitiesOf :: [Event] -> [Identity]
identitiesOf events = reverse (foldl' apply [] events)
  where
    labels :: Map.Map Int Text
    labels = Map.fromList [(e.var.pool, l) | e <- events, Named l <- [e.kind]]
    labelOf :: Var -> Maybe Text
    labelOf v = Map.lookup v.pool labels
    apply :: [Identity] -> Event -> [Identity]
    apply is e = case e.kind of
      Born lineage ->
        Identity
          { var = e.var,
            ordinal = 1 + length [() | i <- is, i.var.pool == e.var.pool],
            label = labelOf e.var,
            lineage
          }
          : is
      Reused -> is
      Consumed -> is
      Named _ -> is

-- * Queries

-- | The step with the given 'Step.index'.
step :: Trace -> Int -> Maybe Step
step t i = find (\s -> s.index == i) t.steps

-- | The identity of the given value.
identity :: Trace -> Var -> Maybe Identity
identity t v = find (\i -> i.var == v) t.identities

-- | The logical value's original identity: follow declared lineage
-- ('Hegel.Pool.transfer') back to the first var.
root :: Trace -> Var -> Var
root t = go []
  where
    -- The visited guard keeps 'build''s totality promise on malformed
    -- streams: a lineage cycle terminates at the first revisit.
    go seen v = case identity t v >>= (.lineage) of
      Just parent | parent /= v, parent `notElem` seen -> go (v : seen) parent
      _ -> v
