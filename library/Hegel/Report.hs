-- | Result of a property run, and its human-readable rendering.
module Hegel.Report
  ( -- * Reports
    Report (..),
    Result (..),
    Abort (..),
    Stats (..),
    aborted,
    throwOnFailure,

    -- * Notes
    Note (..),
    NoteKind (..),

    -- * Rendering
    renderReport,
    renderFailure,
    renderValue,

    -- * Exceptions
    PropertyFailed (..),
  )
where

import Control.Exception (Exception (displayException), SomeException, throwIO)
import Data.List (mapAccumL, partition)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Stack (SrcLoc (..))
import Prettyprinter (Doc, (<+>))
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PP.Text
import Text.Show.Pretty qualified as Pretty

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
data Report = Report
  { result :: Result,
    -- | Tallies for the run. Zero when the run aborted before any test case
    -- could run.
    stats :: Stats
  }
  deriving stock (Show)

-- | The verdict of a property run.
data Result
  = -- | Every attempted test case passed.
    Ok
  | -- | A counterexample was found, described by its journal: every drawn
    -- value and annotation recorded while re-executing the minimal failing
    -- case.
    Counterexample
      { -- | The failure message: the reproducing assertion's message when
        -- available, otherwise the engine's diagnostic or stable origin
        -- string.
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
  deriving stock (Show)

-- | Why a run stopped without reaching a verdict.
data Abort
  = -- | An exception other than a property failure escaped the runner.
    Errored SomeException
  | -- | A health check failed before the property ran.
    UnhealthyInput Text
  deriving stock (Show)

-- | A report for a run that stopped before any test case could run.
aborted :: Abort -> Report
aborted a = Report {result = Aborted a, stats = Stats {valid = 0, invalid = 0}}

-- | Throw on anything other than 'Ok': 'PropertyFailed' on a counterexample,
-- the original exception on 'Errored', and 'fail' otherwise.
throwOnFailure :: Report -> IO ()
throwOnFailure report = case report.result of
  Ok -> pure ()
  Counterexample {message, notes, loc} -> throwIO PropertyFailed {message, notes, loc}
  GaveUp msg -> fail ("Property rejected all inputs: " <> show msg)
  Aborted (Errored exc) -> throwIO exc
  Aborted (UnhealthyInput msg) -> fail ("Health check failed: " <> show msg)

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

-- * Rendering

-- | Semantic annotations on report fragments. The plain-text renderers strip
-- them; a future ANSI renderer maps them to colours.
data Ann
  = MessageAnn
  | LocAnn
  | DrawnAnn
  | NoteAnn

-- | Render a report for human consumption.
renderReport :: Report -> Text
renderReport = docToText . reportDoc

-- | Render the failure section alone (headline message, location, journal);
-- 'PropertyFailed' and 'renderReport' share this layout.
renderFailure :: Text -> [Note] -> Maybe SrcLoc -> Text
renderFailure message notes loc = docToText (failureDoc message notes loc)

-- | Render a value via its 'Show' instance, pretty-printed multi-line when
-- the output parses as a value AST, the raw 'show' string otherwise. The
-- default renderer for @forAll@-style draws.
renderValue :: (Show a) => a -> Text
renderValue a = T.pack (maybe s Pretty.valToStr (Pretty.parseValue s))
  where
    s = show a

reportDoc :: Report -> Doc Ann
reportDoc report = case report.result of
  Ok -> "OK, passed" <+> statsDoc report.stats
  GaveUp msg -> "gave up after" <+> statsDoc report.stats <> ":" <+> PP.pretty msg
  Aborted (Errored e) -> "aborted:" <+> PP.pretty (displayException e)
  Aborted (UnhealthyInput msg) -> "aborted: health check failed:" <+> PP.pretty msg
  Counterexample {message, notes, loc} ->
    PP.vsep
      [ "failed after" <+> statsDoc report.stats,
        failureDoc message notes loc
      ]

-- | Headline message, then (indented) the source location and the journal in
-- order, footnotes last.
failureDoc :: Text -> [Note] -> Maybe SrcLoc -> Doc Ann
failureDoc message notes loc =
  PP.vsep (PP.annotate MessageAnn (PP.pretty message) : fmap (PP.indent 2) details)
  where
    details = locLine <> snd (mapAccumL noteDoc 1 (inline <> footers))
    locLine = maybe [] (\l -> [PP.annotate LocAnn ("at" <+> locDoc l)]) loc
    (footers, inline) = partition (\n -> n.kind == Footnote) notes
    noteDoc :: Int -> Note -> (Int, Doc Ann)
    noteDoc i n = case n.kind of
      Drawn ->
        ( i + 1,
          PP.annotate DrawnAnn ("forAll" <+> PP.pretty i <+> "=" <+> PP.align (PP.pretty n.text))
        )
      Annotation -> (i, PP.annotate NoteAnn (PP.pretty n.text))
      Footnote -> (i, PP.annotate NoteAnn (PP.pretty n.text))

statsDoc :: Stats -> Doc Ann
statsDoc stats
  | stats.invalid == 0 = PP.pretty stats.valid <+> "tests"
  | otherwise =
      PP.pretty stats.valid <+> "tests" <+> PP.parens (PP.pretty stats.invalid <+> "discarded")

locDoc :: SrcLoc -> Doc Ann
locDoc sl = PP.pretty sl.srcLocFile <> ":" <> PP.pretty sl.srcLocStartLine

docToText :: Doc Ann -> Text
docToText = PP.Text.renderStrict . PP.layoutPretty PP.defaultLayoutOptions

-- * Exceptions

-- | Counterexample wrapped for throwing from 'Hegel.runProperty_' and
-- 'Hegel.Property.check_'. Carries the failure message, its source location,
-- and the journal describing the failing case.
data PropertyFailed = PropertyFailed
  { -- | The failure message.
    message :: Text,
    -- | Journal entries describing the failing case.
    notes :: [Note],
    -- | Source location of the failing assertion, when known.
    loc :: Maybe SrcLoc
  }
  deriving stock (Show)

instance Exception PropertyFailed where
  displayException f =
    T.unpack (renderFailure ("property failed: " <> f.message) f.notes f.loc)
