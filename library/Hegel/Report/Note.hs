-- | Journal entries for failure reports. Factored out of "Hegel.Report" so
-- that the rendering modules under @Hegel.Report.*@ can import them without
-- creating a module cycle.
module Hegel.Report.Note
  ( Note (..),
    NoteKind (..),
    hasInBandFailure,
    isFailureNote,
    renderValue,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import GHC.Stack (SrcLoc)
import Hegel.Diff (Diff)
import Hegel.Internal.Tick (Tick)
import Text.Show.Pretty qualified as Pretty

-- | The kind of a journaled 'Note'.
data NoteKind
  = -- | A value drawn during the test (a @forAll@-style draw).
    Drawn
  | -- | Context attached mid-test (an @annotate@-style call).
    Annotation
  | -- | A rule's declared result (a 'Hegel.Stateful.respond' call): the
    -- right-hand side of the trace spine's @call -> response@ column.
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

-- | Is this note an in-band 'Failure'?
isFailureNote :: Note -> Bool
isFailureNote n = case n.kind of
  Failure _ -> True
  _ -> False

-- | Does this journal carry an in-band 'Failure' note (a stateful report)?
-- If so, the renderers switch to the in-band layout described on
-- 'Hegel.Report.renderFailure'.
hasInBandFailure :: [Note] -> Bool
hasInBandFailure = any isFailureNote
