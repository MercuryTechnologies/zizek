-- | Journal entries for failure reports.
module Hegel.Report.Note
  ( Note (..),
    NoteKind (..),
    hasInBandFailure,
    isDrawn,
    isFailureNote,
    isBranchHeader,
    isBranchFailure,
    renderValue,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import GHC.Stack (SrcLoc)
import Hegel.Diff (Diff)
import Hegel.Internal.Event (Var)
import Hegel.Internal.Tick (Tick)
import Text.Show.Pretty qualified as Pretty

-- | The kind of a journaled 'Note'.
data NoteKind
  = -- | A value drawn during the test, tagged with the pool 'Var's the draw
    -- resolved; this list is empty for values drawn outside of a pool.
    Drawn ![Var]
  | -- | Context attached mid-test.
    Annotation
  | -- | A rule's declared result.
    Response
  | -- | A stateful step header, carrying the step number and rule name.
    StepHeader !Int !Text
  | -- | Context rendered after the report body.
    Footnote
  | -- | A caught failure journaled in-band at the point it occurred.
    Failure (Maybe Diff)
  | -- | A concurrent combinator's branch header, carrying its branch index.
    BranchHeader !Int
  | -- | A concurrent branch's own failure, journaled in-band under its
    -- 'BranchHeader'.
    BranchFailure (Maybe Diff)
  | -- | The round, 1-based worker, and concurrency group that fired a
    -- folded concurrent stateful step, carrying all three structurally;
    -- 'Nothing' when the rule belongs to no named group.
    StepOrigin !Int !Int !(Maybe Text)
  | -- | A concurrent stateful round's join point, carrying the step index
    -- this boundary occupies in the log and the round number it closes.
    RoundBoundary !Int !Int
  deriving stock (Show, Eq)

-- | One entry in a failure report's journal: rendered text plus the call
-- site that produced it, when known.
data Note = Note
  { kind :: NoteKind,
    text :: Text,
    loc :: Maybe SrcLoc,
    -- | Nesting level (0 = top level). Draws made inside a stateful step are
    -- journaled one level deeper than the step header itself.
    depth :: !Int,
    -- | Sequence stamp from the clock shared with the pool-event stream, so
    -- that the render boundary can zip streams back into a single ordered
    -- history.
    clock :: !Tick
  }
  deriving stock (Show)

-- | Render a value via its 'Show' instance, pretty-printed multi-line when
-- the output parses as a value AST, the raw 'show' string otherwise.
--
-- The default renderer for @forAll@-style draws.
renderValue :: (Show a) => a -> Text
renderValue a = T.pack (maybe s Pretty.valToStr (Pretty.parseValue s))
  where
    s = show a

-- | Is this a 'Drawn' note, regardless of its draw provenance?
isDrawn :: NoteKind -> Bool
isDrawn = \case
  Drawn _ -> True
  _ -> False

-- | Is this note an in-band 'Failure'?
isFailureNote :: Note -> Bool
isFailureNote n = case n.kind of
  Failure _ -> True
  _ -> False

-- | Is this note a concurrent combinator's 'BranchHeader'?
isBranchHeader :: Note -> Bool
isBranchHeader n = case n.kind of
  BranchHeader _ -> True
  _ -> False

-- | Is this note a concurrent branch's own in-band 'BranchFailure'?
isBranchFailure :: Note -> Bool
isBranchFailure n = case n.kind of
  BranchFailure _ -> True
  _ -> False

-- | Does this journal carry a failure rendered in-band at its own tree
-- position rather than only at the top-level headline?
hasInBandFailure :: [Note] -> Bool
hasInBandFailure = any (\n -> isFailureNote n || isBranchFailure n)
