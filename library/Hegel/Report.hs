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
    renderReportRichAnsi,
    renderFailure,
    renderValue,

    -- * Re-exports from 'Hegel.Report.Ann'
    Ann (..),

    -- * Exceptions
    PropertyFailed (..),
  )
where

import Control.Exception (Exception (displayException), SomeException, throwIO)
import Data.Either (partitionEithers)
import Data.List (mapAccumL, partition)
import Data.Maybe (catMaybes, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Stack (SrcLoc (..))
import Hegel.Diff (Diff)
import Hegel.Report.Ann (Ann (..), docToAnsi, docToText, lineDiffDoc)
import Hegel.Report.Discovery (loadDeclarations)
import Hegel.Report.Source
  ( applyContext,
    defaultContext,
    mergeDeclarations,
    ppDeclaration,
    ppFailedInput,
    ppFailureLocation,
  )
import Hegel.Report.Span (Span (..), spanFromSrcLoc)
import Prettyprinter (Doc, (<+>))
import Prettyprinter qualified as PP
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
    -- | Structured diff, when this note is a 'Failure' from '(===)'.
    diff :: Maybe Diff,
    -- | Nesting level (0 = top level). Draws made inside a stateful step are
    -- journaled one level deeper than the step header itself.
    depth :: !Int
  }
  deriving stock (Show)

-- * Pure rendering (always succeeds, no IO)

-- | Render a report as plain text.
renderReport :: Report -> Text
renderReport = docToText . reportDoc

-- | Render a report with ANSI colour codes (suitable for TTY output).
-- Diff lines are red\/green; the failure message is bold; location is dim.
renderReportAnsi :: Report -> Text
renderReportAnsi = docToAnsi . reportDoc

-- | Render the failure section alone (headline message, diff, location,
-- journal). 'PropertyFailed' and 'renderReport' share this layout.
renderFailure :: Text -> [Note] -> Maybe SrcLoc -> Maybe Diff -> Text
renderFailure message notes loc diff = docToText (failureDoc message notes loc diff)

-- | Render a value via its 'Show' instance, pretty-printed multi-line when
-- the output parses as a value AST, the raw 'show' string otherwise. The
-- default renderer for @forAll@-style draws.
renderValue :: (Show a) => a -> Text
renderValue a = T.pack (maybe s Pretty.valToStr (Pretty.parseValue s))
  where
    s = show a

-- * Source-aware rendering (reads files; degrades gracefully)

-- | Render a report as plain text, splicing drawn values and the failure
-- message inline into a source listing. Reads source files at render time;
-- degrades to 'renderReport' when no source is readable.
renderReportRich :: Report -> IO Text
renderReportRich = renderReportRichWith renderReport docToText

-- | 'renderReportRich' with ANSI colour codes. Degrades to 'renderReportAnsi'
-- when no source is readable.
renderReportRichAnsi :: Report -> IO Text
renderReportRichAnsi = renderReportRichWith renderReportAnsi docToAnsi

-- | Shared implementation of the rich renderers, parameterised over the
-- plain-text fallback and the final document renderer.
renderReportRichWith :: (Report -> Text) -> (Doc Ann -> Text) -> Report -> IO Text
renderReportRichWith plain toText report = case report.result of
  Counterexample {message, notes, loc, diff} -> do
    mdoc <- richDoc message notes loc diff
    pure case mdoc of
      Nothing -> plain report
      Just body -> toText (PP.vsep ["failed after" <+> statsDoc report.stats, body])
  _ -> pure (plain report)

-- | Attempt to build the rich failure doc, falling back to 'Nothing' when no
-- declaration could be read for any location.
richDoc :: Text -> [Note] -> Maybe SrcLoc -> Maybe Diff -> IO (Maybe (Doc Ann))
richDoc message notes loc diff
  -- Journals carrying an in-band 'Failure' note (stateful reports) don't fit
  -- the source-splicing model: their structure is @Step N@ annotations, not
  -- drawn-value declarations. Degrade to the shared structured layout so the
  -- plain and rich renderers agree.
  | any (\n -> n.kind == Failure) notes =
      pure (Just (failureDoc message notes loc diff))
richDoc message notes loc diff = do
  let (footers, inline) = partition (\n -> n.kind == Footnote) notes
      inputs = [(fmap spanFromSrcLoc n.loc, n.text) | n <- inline]
      mFailureSpan = fmap spanFromSrcLoc loc
      spans = catMaybes (fmap fst inputs) <> maybeToList mFailureSpan
  decls <- loadDeclarations (fmap (.spanFile) spans)
  let (args, idecls) =
        partitionEithers (zipWith (ppFailedInput decls) [0 ..] inputs)
      mFailureDecl =
        ppFailureLocation decls (fmap PP.pretty (T.lines message)) diff
          =<< mFailureSpan
      allDecls = mergeDeclarations (maybeToList mFailureDecl <> idecls)
      declDocs = fmap (ppDeclaration . applyContext defaultContext) allDecls
      footerDocs = [PP.annotate NoteAnn (PP.pretty n.text) | n <- footers]
      sections = [PP.vsep ds | ds <- [args, declDocs, footerDocs], not (null ds)]
  -- Degrade to the plain renderer unless at least one declaration rendered;
  -- the @forAll N =@ fallback docs in 'args' only supplement a source
  -- listing, they don't constitute one.
  pure
    if null allDecls
      then Nothing
      else Just (PP.vsep sections)

-- * Internal pure layout

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

-- | Render the failure body.
--
-- Ordinary reports: the headline message, then (indented) the diff (if any),
-- the source location, and the journal in order, footnotes last.
--
-- Reports whose journal contains a 'Failure' note (stateful reports): the
-- failure is rendered __in-band__ at its step and the top-level headline\/
-- diff\/location block is suppressed, since the 'Failure' note already carries
-- them. Each note is indented by its 'Note'@.depth@ so draws nest under the
-- step that made them. The report renderers prepend @"failed after N tests"@
-- as the summary headline; 'renderFailure' does not (yet) supply one in this
-- mode, so 'PropertyFailed'@.displayException@ currently has no headline.
failureDoc :: Text -> [Note] -> Maybe SrcLoc -> Maybe Diff -> Doc Ann
failureDoc message notes loc diff
  | inBand = PP.vsep journalDocs
  | otherwise =
      PP.vsep (PP.annotate MessageAnn (PP.pretty message) : topBlock <> journalDocs)
  where
    inBand :: Bool
    inBand = any (\n -> n.kind == Failure) notes
    topBlock :: [Doc Ann]
    topBlock = fmap (PP.indent 2) (diffLines <> locLine)
    diffLines :: [Doc Ann]
    diffLines = maybe [] (\d -> [diffDoc d]) diff
    locLine :: [Doc Ann]
    locLine = maybe [] (\l -> [PP.annotate LocAnn ("at" <+> locDoc l)]) loc
    (footers, inline) = partition (\n -> n.kind == Footnote) notes
    journalDocs :: [Doc Ann]
    journalDocs = snd (mapAccumL noteDoc 1 (inline <> footers))
    noteDoc :: Int -> Note -> (Int, Doc Ann)
    noteDoc i n =
      let base = (n.depth + 1) * 2
       in case n.kind of
            Drawn ->
              ( i + 1,
                PP.indent base (PP.annotate DrawnAnn ("forAll" <+> PP.pretty i <+> "=" <+> PP.align (PP.pretty n.text)))
              )
            Annotation -> (i, PP.indent base (PP.annotate NoteAnn (PP.pretty n.text)))
            Footnote -> (i, PP.indent base (PP.annotate NoteAnn (PP.pretty n.text)))
            Failure -> (i, failureNoteDoc base n)

-- | Render an in-band 'Failure' note: a marked headline at the note's depth,
-- the structured diff (if any) indented under it, then the source location.
failureNoteDoc :: Int -> Note -> Doc Ann
failureNoteDoc base n =
  PP.vsep $
    PP.indent base (PP.annotate MessageAnn ("✗" <+> PP.pretty n.text))
      : fmap (PP.indent (base + 4)) (maybe [] (\d -> [diffDoc d]) n.diff)
        <> fmap (PP.indent (base + 2)) (maybe [] (\l -> [PP.annotate LocAnn ("at" <+> locDoc l)]) n.loc)

diffDoc :: Diff -> Doc Ann
diffDoc = PP.vsep . fmap lineDiffDoc

statsDoc :: Stats -> Doc Ann
statsDoc stats
  | stats.invalid == 0 = PP.pretty stats.valid <+> "tests"
  | otherwise =
      PP.pretty stats.valid <+> "tests" <+> PP.parens (PP.pretty stats.invalid <+> "discarded")

locDoc :: SrcLoc -> Doc Ann
locDoc sl = PP.pretty sl.srcLocFile <> ":" <> PP.pretty sl.srcLocStartLine

-- * Exceptions

-- | Counterexample wrapped for throwing from 'Hegel.prop' and
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
