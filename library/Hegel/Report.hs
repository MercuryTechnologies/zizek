-- | Result of a property run.
module Hegel.Report
  ( -- * Reports
    Report (..),
    Result (..),
    Abort (..),
    Stats (..),
    aborted,

    -- * Notes
    Note (..),
    NoteKind (..),

    -- * Exceptions
    PropertyFailed (..),
  )
where

import Control.Exception (Exception (displayException), SomeException)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Stack (SrcLoc)

-- | Summary statistics for a property run.
data Stats = Stats
  { -- | Valid test cases executed.
    valid :: !Int,
    -- | How many cases were rejected as invalid (via 'Hegel.Gen.assume',
    -- 'Hegel.Gen.filtered', 'Hegel.Gen.discard', or 'Hegel.Gen.mapMaybe'
    -- exhaustion).
    invalid :: !Int
  }
  deriving stock (Show)

-- | What happened when a property was run, plus run statistics.
--
-- The @cex@ parameter is the type of the reported counterexample: the
-- generator's value type for 'Hegel.runProperty', or @()@ for entry points
-- whose draws are described by 'notes' instead of a single typed value.
data Report cex = Report
  { result :: Result cex,
    -- | Tallies for the run. Zero when the run aborted before any test case
    -- could run.
    stats :: Stats
  }
  deriving stock (Show, Functor)

-- | The verdict of a property run.
data Result cex
  = -- | Every attempted test case passed.
    Ok
  | -- | A counterexample was found.
    Counterexample
      { -- | The typed counterexample.
        value :: cex,
        -- | The failure message (the engine's diagnostic, or its stable
        -- origin string when no diagnostic exists).
        message :: Text,
        -- | Journal entries describing the failing case.
        notes :: [Note],
        -- | Source location of the failing assertion, when known.
        loc :: Maybe SrcLoc
      }
  | -- | No valid examples were generated.
    GaveUp Text
  | -- | The run stopped before reaching a verdict.
    Aborted Abort
  deriving stock (Show, Functor)

-- | Why a run stopped without reaching a verdict.
data Abort
  = -- | An exception other than a property failure escaped the runner.
    Errored SomeException
  | -- | A health check failed before the property ran.
    UnhealthyInput Text
  deriving stock (Show)

-- | A report for a run that stopped before any test case could run.
aborted :: Abort -> Report cex
aborted a = Report {result = Aborted a, stats = Stats {valid = 0, invalid = 0}}

-- | The kind of a journaled 'Note'.
data NoteKind
  = -- | A value drawn during the test (a @forAll@-style draw).
    Drawn
  | -- | Context attached mid-test (an @annotate@-style call).
    Annotation
  | -- | Context rendered after the report body (a @footnote@-style call).
    Footnote
  deriving stock (Show, Eq)

-- | One entry in a failure report's journal: rendered text plus the call
-- site that produced it, when known.
data Note = Note
  { kind :: NoteKind,
    text :: Text,
    loc :: Maybe SrcLoc
  }
  deriving stock (Show)

-- | Counterexample wrapped for throwing from 'Hegel.runProperty_'.
--
-- The counterexample is rendered to 'Text' at throw time: the only thing a
-- catcher could ever do with the erased value is render it, so carrying the
-- rendering keeps the type first-order and lets entry points choose their
-- own renderer (e.g. 'show', or a custom function).
data PropertyFailed = PropertyFailed
  { -- | The rendered counterexample.
    counterexample :: Text,
    -- | The failure message (the engine's diagnostic, or its stable origin
    -- string when no diagnostic exists).
    message :: Text,
    -- | Journal entries describing the failing case.
    notes :: [Note]
  }
  deriving stock (Show)

instance Exception PropertyFailed where
  displayException f =
    "property failed with counterexample "
      <> T.unpack f.counterexample
      <> ": "
      <> T.unpack f.message
      <> concatMap (\n -> "\n  " <> T.unpack n.text) f.notes
