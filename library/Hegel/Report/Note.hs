-- | Journal entries for failure reports. Factored out of "Hegel.Report" so
-- that the rendering modules under @Hegel.Report.*@ can import them without
-- creating a module cycle.
module Hegel.Report.Note
  ( Note (..),
    NoteKind (..),
    hasInBandFailure,
  )
where

import Data.Text (Text)
import GHC.Stack (SrcLoc)
import Hegel.Diff (Diff)

-- | The kind of a journaled 'Note'.
data NoteKind
  = -- | A value drawn during the test (a @forAll@-style draw).
    Drawn
  | -- | Context attached mid-test (an @annotate@-style call).
    Annotation
  | -- | Context rendered after the report body (a @footnote@-style call).
    Footnote
  | -- | A caught failure journaled in-band at the point it occurred (used by
    -- stateful tests to attach the failure to its step).
    Failure
  deriving stock (Show, Eq)

-- | One entry in a failure report's journal: rendered text plus the call
-- site that produced it, when known.
data Note = Note
  { kind :: NoteKind,
    text :: Text,
    loc :: Maybe SrcLoc,
    -- | Structured diff, when this note is a 'Failure' from @(===)@.
    diff :: Maybe Diff,
    -- | Nesting level (0 = top level). Draws made inside a stateful step are
    -- journaled one level deeper than the step header itself.
    depth :: !Int
  }
  deriving stock (Show)

-- | Does this journal carry an in-band 'Failure' note (a stateful report)?
-- If so, the renderers switch to the in-band layout described on
-- 'Hegel.Report.renderFailure'.
hasInBandFailure :: [Note] -> Bool
hasInBandFailure = any (\n -> n.kind == Failure)
