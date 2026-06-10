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
    renderReportAnsi,
    renderReportRich,
    renderFailure,
    renderFailureAnsi,
    renderValue,

    -- * Exceptions
    PropertyFailed (..),
  )
where

import Control.Exception (Exception (displayException), IOException, SomeException, throwIO)
import Control.Exception qualified as Ex
import Data.List (mapAccumL, partition)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Stack (SrcLoc (..))
import Hegel.Diff (Diff, LineDiff (..))
import Prettyprinter (Doc, (<+>))
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal (AnsiStyle)
import Prettyprinter.Render.Terminal qualified as PP.Terminal
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
        loc :: Maybe SrcLoc,
        -- | Structural or line-level diff, when the failure came from '(===)'.
        diff :: Maybe Diff
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
  Counterexample {message, notes, loc, diff} ->
    throwIO PropertyFailed {message, notes, loc, diff}
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

-- | Semantic annotations on report fragments. The plain-text renderer strips
-- them; the ANSI renderer maps them to colours.
data Ann
  = MessageAnn
  | LocAnn
  | DrawnAnn
  | NoteAnn
  | DiffSame
  | DiffRemoved
  | DiffAdded

-- | Render a report as plain text.
renderReport :: Report -> Text
renderReport = docToText . reportDoc

-- | Render a report with ANSI colour codes (suitable for TTY output).
-- Diff lines are red\/green; the failure message is bold; location is dim.
renderReportAnsi :: Report -> Text
renderReportAnsi = docToAnsi . reportDoc

-- | Render a report as plain text, splicing in the source line above each
-- 'Drawn' note whose location is known. Reads source files at render time;
-- degrades gracefully to 'renderReport' when files are unavailable.
renderReportRich :: Report -> IO Text
renderReportRich report = case report.result of
  Counterexample {message, notes, loc, diff} -> do
    richNotes <- mapM enrichNote notes
    pure $
      docToText $
        PP.vsep
          [ "failed after" <+> statsDoc report.stats,
            failureDoc message richNotes loc diff
          ]
  _ -> pure (renderReport report)
  where
    enrichNote :: Note -> IO Note
    enrichNote n@Note {kind = Drawn, loc = Just sl} =
      readSourceLine sl.srcLocFile sl.srcLocStartLine
        >>= \msrc -> pure case msrc of
          Nothing -> n
          Just src -> n {text = T.pack src <> "\n" <> n.text}
    enrichNote n = pure n

-- | Read a single line (1-based) from a source file, returning 'Nothing' on
-- any 'IOException' or out-of-range line number.
readSourceLine :: FilePath -> Int -> IO (Maybe String)
readSourceLine path lineNo =
  ( do
      ls <- lines <$> readFile path
      let idx = lineNo - 1
      pure
        if idx >= 0 && idx < length ls
          then Just (ls !! idx)
          else Nothing
  )
    `Ex.catch` \(_ :: IOException) -> pure Nothing

-- | Render the failure section alone (headline message, diff, location,
-- journal). 'PropertyFailed' and 'renderReport' share this layout.
renderFailure :: Text -> [Note] -> Maybe SrcLoc -> Maybe Diff -> Text
renderFailure message notes loc diff = docToText (failureDoc message notes loc diff)

-- | 'renderFailure' with ANSI colour codes.
renderFailureAnsi :: Text -> [Note] -> Maybe SrcLoc -> Maybe Diff -> Text
renderFailureAnsi message notes loc diff = docToAnsi (failureDoc message notes loc diff)

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
  Counterexample {message, notes, loc, diff} ->
    PP.vsep
      [ "failed after" <+> statsDoc report.stats,
        failureDoc message notes loc diff
      ]

-- | Headline message, then (indented) the diff (if any), the source
-- location, and the journal in order, footnotes last.
failureDoc :: Text -> [Note] -> Maybe SrcLoc -> Maybe Diff -> Doc Ann
failureDoc message notes loc diff =
  PP.vsep (PP.annotate MessageAnn (PP.pretty message) : fmap (PP.indent 2) details)
  where
    details :: [Doc Ann]
    details = diffLines <> locLine <> snd (mapAccumL noteDoc 1 (inline <> footers))
    diffLines :: [Doc Ann]
    diffLines = maybe [] (\d -> [diffDoc d]) diff
    locLine :: [Doc Ann]
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

diffDoc :: Diff -> Doc Ann
diffDoc = PP.vsep . fmap lineDiffDoc
  where
    lineDiffDoc :: LineDiff -> Doc Ann
    lineDiffDoc (LineSame t) = PP.annotate DiffSame ("  " <> PP.pretty t)
    lineDiffDoc (LineRemoved t) = PP.annotate DiffRemoved ("- " <> PP.pretty t)
    lineDiffDoc (LineAdded t) = PP.annotate DiffAdded ("+ " <> PP.pretty t)

statsDoc :: Stats -> Doc Ann
statsDoc stats
  | stats.invalid == 0 = PP.pretty stats.valid <+> "tests"
  | otherwise =
      PP.pretty stats.valid <+> "tests" <+> PP.parens (PP.pretty stats.invalid <+> "discarded")

locDoc :: SrcLoc -> Doc Ann
locDoc sl = PP.pretty sl.srcLocFile <> ":" <> PP.pretty sl.srcLocStartLine

docToText :: Doc Ann -> Text
docToText = PP.Text.renderStrict . PP.layoutPretty PP.defaultLayoutOptions

docToAnsi :: Doc Ann -> Text
docToAnsi =
  PP.Terminal.renderStrict
    . PP.layoutPretty PP.defaultLayoutOptions
    . PP.reAnnotate annToAnsi

annToAnsi :: Ann -> AnsiStyle
annToAnsi = \case
  MessageAnn -> PP.Terminal.bold
  LocAnn -> PP.Terminal.colorDull PP.Terminal.White
  DrawnAnn -> PP.Terminal.color PP.Terminal.Cyan
  NoteAnn -> mempty
  DiffSame -> mempty
  DiffRemoved -> PP.Terminal.color PP.Terminal.Red
  DiffAdded -> PP.Terminal.color PP.Terminal.Green

-- * Exceptions

-- | Counterexample wrapped for throwing from 'Hegel.runProperty_' and
-- 'Hegel.Property.check_'. Carries the failure message, its source location,
-- the journal describing the failing case, and the diff (if any).
data PropertyFailed = PropertyFailed
  { -- | The failure message.
    message :: Text,
    -- | Journal entries describing the failing case.
    notes :: [Note],
    -- | Source location of the failing assertion, when known.
    loc :: Maybe SrcLoc,
    -- | Structural or line-level diff, when the failure came from '(===)'.
    diff :: Maybe Diff
  }
  deriving stock (Show)

instance Exception PropertyFailed where
  displayException f =
    T.unpack (renderFailure ("property failed: " <> f.message) f.notes f.loc f.diff)
