-- | Journal entries for failure reports. Factored out of "Hegel.Report" so
-- that the rendering modules under @Hegel.Report.*@ can import them without
-- creating a module cycle.
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
  = -- | A value drawn during the test (a @forAll@-style draw), tagged with the
    -- pool 'Var's the draw resolved; this list is empty for values drawn
    -- outside of a pool.
    Drawn ![Var]
  | -- | Context attached mid-test (an @annotate@-style call).
    Annotation
  | -- | A rule's declared result (a 'Hegel.Stateful.respond' call): the
    -- right-hand side of the event log's @call -> response@ column.
    Response
  | -- | A stateful step header, carrying the step number and rule name
    -- structurally.
    --
    -- 'Note.text' still carries the rendered @\"Step N: rulename\"@ string,
    -- which is what the structured renderers display.
    StepHeader !Int !Text
  | -- | Context rendered after the report body (a @footnote@-style call).
    Footnote
  | -- | A caught failure journaled in-band at the point it occurred (used by
    -- stateful tests to attach the failure to its step), carrying the
    -- structured diff when the failure came from @(===)@.
    Failure (Maybe Diff)
  | -- | A concurrent combinator's branch header, carrying the 1-based branch
    -- index structurally. 'Note.text' still carries the rendered
    -- @\"Branch N\"@ string, mirroring 'StepHeader'.
    BranchHeader !Int
  | -- | A concurrent branch's own failure, journaled in-band under its
    -- 'BranchHeader' — the concurrent analogue of 'Failure', carrying the
    -- structured diff when the failure came from @(===)@. Kept distinct from
    -- 'Failure' so a branch failure never trips the stateful (step-structured)
    -- render path.
    BranchFailure (Maybe Diff)
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
    -- | Sequence stamp from the clock shared with the pool-event stream
    -- ("Hegel.Internal.Event"); lets the render boundary zip the two streams
    -- back into one ordered history. @'Hegel.Internal.Tick.Tick' 0@ when no
    -- event stream was recording (including synthetic test journals).
    clock :: !Tick
  }
  deriving stock (Show)

-- | Render a value via its 'Show' instance, pretty-printed multi-line when
-- the output parses as a value AST, the raw 'show' string otherwise. The
-- default renderer for @forAll@-style draws.
renderValue :: (Show a) => a -> Text
renderValue a = T.pack (maybe s Pretty.valToStr (Pretty.parseValue s))
  where
    s = show a

-- | Is this a 'Drawn' note, regardless of its draw provenance?
isDrawn :: NoteKind -> Bool
isDrawn = \case
  Drawn _ -> True
  _ -> False

-- | Is this note an in-band 'Failure'? Only 'Stateful.run' produces one, and
-- this predicate drives the step-structured render path, so a
-- 'BranchFailure' must not satisfy it; 'isBranchFailure' answers that.
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
-- position rather than only at the top-level headline? True for a stateful
-- step's 'Failure' or a concurrent branch's 'BranchFailure'; when true, the
-- renderers drop the redundant top-level headline in favor of the in-band
-- block, per 'Hegel.Report.renderFailure'. Drives headline suppression only;
-- the stateful render-path choice is 'isFailureNote' alone.
hasInBandFailure :: [Note] -> Bool
hasInBandFailure = any (\n -> isFailureNote n || isBranchFailure n)
