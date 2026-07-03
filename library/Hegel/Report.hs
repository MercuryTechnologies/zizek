-- | Result of a property run, and its human-readable rendering.
module Hegel.Report
  ( -- * Reports
    Report (..),
    Result (..),
    Abort (..),
    Stats (..),
    aborted,
    throwOnFailure,

    -- * Notes (re-exported from "Hegel.Report.Note")
    Note (..),
    NoteKind (..),
    isFailureNote,

    -- * Events (re-exported from "Hegel.Internal.Event")
    Event (..),
    EventKind (..),
    Var (..),
    Clock (..),

    -- * Rendering
    renderReport,
    renderReportAnsi,
    renderReportRich,
    renderReportRichAnsi,
    renderReportRichWith,
    renderReportRichAnsiWith,
    renderReportAuto,
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
import Data.List (partition)
import Data.Maybe (catMaybes, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Stack (SrcLoc (..))
import Hegel.Diff (Diff)
import Hegel.Internal.Event (Clock (..), Event (..), EventKind (..), Var (..))
import Hegel.Report.Ann (Ann (..), diffDocs, docToAnsi, docToText)
import Hegel.Report.Blame (Blame)
import Hegel.Report.Blame qualified as Blame
import Hegel.Report.Discovery (Declarations, loadDeclarations)
import Hegel.Report.Glyph qualified as Glyph
import Hegel.Report.Journal (journalDocs, locDoc)
import Hegel.Report.Ledger qualified as Ledger
import Hegel.Report.Note (Note (..), NoteKind (..), hasInBandFailure, isFailureNote)
import Hegel.Report.Phrase (PhraseTable (..))
import Hegel.Report.Source
  ( applyContext,
    defaultContext,
    mergeDeclarations,
    mergeFileDeclarations,
    ppDeclaration,
    ppFailedInput,
    ppFailureLocation,
  )
import Hegel.Report.Span (Span (..), spanFromSrcLoc)
import Hegel.Report.Stateful (failingGroupDoc, isStepJournal, noteFiles, statefulDoc)
import Hegel.Report.Style (Style (..), defaultStyle)
import Hegel.Report.Trace (Trace)
import Hegel.Report.Trace qualified as Trace
import Hegel.Report.Verdict qualified as Verdict
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
    stats :: Stats,
    -- | The example-database key the run persisted under, when persistence
    -- was on — the reproduction surface the failure footer points at.
    databaseKey :: !(Maybe Text)
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
        -- | Pool events recorded alongside the journal (empty when the case
        -- used no pools); shares a clock with 'Note.clock'.
        events :: [Event],
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
  | -- | The engine reported a failure but its reproduction blob did not
    -- re-trigger it on replay (it passed or discarded) — the stored example
    -- may be stale, or the system under test is nondeterministic. A distinct
    -- verdict, never conflated with an error.
    ReplayDiverged Text
  deriving stock (Show)

-- | A report for a run that stopped before any test case could run.
aborted :: Abort -> Report
aborted a = Report {result = Aborted a, stats = Stats {valid = 0, invalid = 0}, databaseKey = Nothing}

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
  Aborted (ReplayDiverged msg) -> fail ("Replay diverged: " <> show msg)

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
renderFailure message notes loc diff = docToText body
  where
    -- In-band journals suppress the headline in 'failureDoc'; the report
    -- renderers substitute @"failed after N tests"@, but 'PropertyFailed''s
    -- 'displayException' has no such summary, so restore the headline here.
    body
      | hasInBandFailure notes = PP.vsep [headlineDoc message, failureDoc message notes loc diff]
      | otherwise = failureDoc message notes loc diff

-- | Render a value via its 'Show' instance, pretty-printed multi-line when
-- the output parses as a value AST, the raw 'show' string otherwise. The
-- default renderer for @forAll@-style draws.
renderValue :: (Show a) => a -> Text
renderValue a = T.pack (maybe s Pretty.valToStr (Pretty.parseValue s))
  where
    s = show a

-- * Source-aware rendering (reads files; degrades gracefully)

-- | Render a report as plain text, splicing drawn values and the failure
-- message inline into a source listing — and, for stateful failures with
-- pool context, composing the citation ledger and verdict paragraph above
-- the failing step's splice (see 'renderReportRichWith' for the ladder).
-- Reads source files at render time; degrades to 'renderReport' when no
-- source is readable.
renderReportRich :: Report -> IO Text
renderReportRich = renderReportRichWith (defaultStyle Glyph.unicode)

-- | 'renderReportRich' with ANSI colour codes. Degrades to 'renderReportAnsi'
-- when no source is readable.
renderReportRichAnsi :: Report -> IO Text
renderReportRichAnsi = renderReportRichAnsiWith (defaultStyle Glyph.unicode)

-- | 'renderReportRich' with an explicit 'Style' (glyph table, phrase table,
-- direction, budgets).
renderReportRichWith :: Style -> Report -> IO Text
renderReportRichWith style = renderRichImpl style renderReport docToText

-- | 'renderReportRichAnsi' with an explicit 'Style'.
renderReportRichAnsiWith :: Style -> Report -> IO Text
renderReportRichAnsiWith style = renderRichImpl style renderReportAnsi docToAnsi

-- | The integrations' one-stop renderer: rich, ANSI per @useColor@, glyphs
-- per the output 'Glyph.Preference' — with the ascii preference's
-- 7-bit-clean guarantee applied to the whole result. Keeps the
-- render-then-clean invariant in one place instead of one per framework.
renderReportAuto :: Bool -> Glyph.Preference -> Report -> IO Text
renderReportAuto useColor pref report =
  Glyph.cleanFor pref
    <$> (if useColor then renderReportRichAnsiWith style else renderReportRichWith style) report
  where
    style = defaultStyle (Glyph.table pref)

-- | Shared implementation of the rich renderers, parameterised over the
-- plain-text fallback and the final document renderer.
renderRichImpl :: Style -> (Report -> Text) -> (Doc Ann -> Text) -> Report -> IO Text
renderRichImpl style plain toText report = do
  mdoc <- richDoc style report
  pure case mdoc of
    Nothing -> plain report
    Just body -> toText (PP.vsep ["failed after" <+> statsDoc report.stats, body])

-- | Attempt to build the rich failure doc, falling back to 'Nothing' when
-- the result is not a counterexample or no declaration could be read for
-- any location.
--
-- Step-structured journals take the degradation ladder, each rung pinned:
--
-- (1) no pool events → today's spliced Timeline, byte-for-byte;
-- (2) events but nothing to cite → the Timeline plus the footer;
-- (3) a blame tree → the composed trace report: verdict paragraph,
--     citation ledger, the failing step's freeze-frame splice, footer
--     (the verdict line itself degrades away when citations are empty).
richDoc :: Style -> Report -> IO (Maybe (Doc Ann))
richDoc style report = case report.result of
  Counterexample {message, notes, events, loc, diff}
    | isStepJournal notes -> do
        decls <- loadDeclarations (noteFiles notes)
        let timeline = statefulDoc decls message notes loc diff
            trace = Trace.build notes events
        pure . Just $ case (events, Blame.analyze trace) of
          ([], _) -> timeline
          (_, Nothing) -> PP.vsep (timeline : maybeToList (footerDoc style.phrases report.databaseKey))
          (_, Just blame) -> composedDoc style decls trace blame notes report.databaseKey
    | otherwise -> plainRichDoc message notes loc diff
  _ -> pure Nothing

-- | The non-stateful rich doc: drawn values and the failure message spliced
-- into a source listing.
plainRichDoc :: Text -> [Note] -> Maybe SrcLoc -> Maybe Diff -> IO (Maybe (Doc Ann))
plainRichDoc message notes loc diff = do
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
      allDecls = mergeFileDeclarations (mergeDeclarations (maybeToList mFailureDecl <> idecls))
      declDocs = fmap (ppDeclaration . applyContext defaultContext) allDecls
      footerDocs = [PP.annotate NoteAnn (PP.pretty n.text) | n <- footers]
      sections = [PP.vsep ds | ds <- [args, declDocs, footerDocs], not (null ds)]
  -- Degrade to the plain renderer unless at least one declaration rendered;
  -- the @Draw N:@ fallback docs in 'args' only supplement a source
  -- listing, they don't constitute one.
  pure
    if null allDecls
      then Nothing
      else Just (PP.vsep sections)

-- | The composed trace report (the R4 stack, sans headline — the caller
-- prepends @failed after …@): phenomenon chip, verdict paragraph, citation
-- ledger, the failing step's freeze-frame splice, footer. Sections separate
-- with one blank line; every layer is a projection of the same trace and
-- blame values.
composedDoc :: Style -> Declarations -> Trace -> Blame -> [Note] -> Maybe Text -> Doc Ann
composedDoc style decls trace blame notes databaseKey =
  PP.vsep (PP.punctuate PP.line (catMaybes sections))
  where
    sections =
      [ chip,
        Verdict.verdictDoc style trace blame,
        Just (Ledger.ledgerDoc style trace blame),
        failingGroupDoc decls notes,
        -- Footnotes keep their contract on the richest rung too: context
        -- rendered after the report body, before the reproduction line.
        footnotesDoc notes,
        footerDoc style.phrases databaseKey
      ]
    chip =
      fmap
        (PP.annotate LocAnn . PP.pretty . style.phrases.phenomenon)
        blame.diagnosis

-- | Footnote notes, rendered after the report body (their documented
-- position, regardless of rung).
footnotesDoc :: [Note] -> Maybe (Doc Ann)
footnotesDoc notes = case [n.text | n <- notes, n.kind == Footnote] of
  [] -> Nothing
  fs -> Just (PP.vsep [PP.indent 2 (PP.annotate NoteAnn (PP.pretty t)) | t <- fs])

-- | The reproduction footer: present only when the run persisted under a
-- database key (pointing anywhere else would be dishonest — replay is
-- automatic on the next run, there is no CLI yet). Words from the phrase
-- table, like everything else.
footerDoc :: PhraseTable -> Maybe Text -> Maybe (Doc Ann)
footerDoc phrases = fmap (PP.annotate LocAnn . PP.pretty . phrases.stored)

-- * Internal pure layout

reportDoc :: Report -> Doc Ann
reportDoc report = case report.result of
  Ok -> "OK, passed" <+> statsDoc report.stats
  GaveUp msg -> "gave up after" <+> statsDoc report.stats <> ":" <+> PP.pretty msg
  Aborted (Errored e) -> "aborted:" <+> PP.pretty (displayException e)
  Aborted (UnhealthyInput msg) -> "aborted: health check failed:" <+> PP.pretty msg
  Aborted (ReplayDiverged msg) -> "aborted: replay diverged:" <+> PP.pretty msg
  Counterexample {message, notes, loc, diff} ->
    PP.vsep
      [ "failed after" <+> statsDoc report.stats,
        failureDoc message notes loc diff
      ]

-- | The headline @message@ line of a failure report.
headlineDoc :: Text -> Doc Ann
headlineDoc = PP.annotate MessageAnn . PP.pretty

-- | Render the failure body.
--
-- Ordinary reports: the headline message, then (indented) the diff (if any),
-- the source location, and the journal in order, footnotes last.
--
-- Reports whose journal contains a 'Failure' note (stateful reports): the
-- failure is rendered __in-band__ at its step and the top-level headline\/
-- diff\/location block is suppressed, since the 'Failure' note already
-- carries them. The report renderers substitute @"failed after N tests"@ as
-- the headline; 'renderFailure' restores the message.
failureDoc :: Text -> [Note] -> Maybe SrcLoc -> Maybe Diff -> Doc Ann
failureDoc message notes loc diff
  | hasInBandFailure notes = PP.vsep (journalDocs notes)
  | otherwise =
      PP.vsep (headlineDoc message : topBlock <> journalDocs notes)
  where
    topBlock :: [Doc Ann]
    topBlock = fmap (PP.indent 2) (diffLines <> locLine)
    diffLines :: [Doc Ann]
    diffLines = maybe [] (\d -> [diffDoc d]) diff
    locLine :: [Doc Ann]
    locLine = maybe [] (\l -> [PP.annotate LocAnn ("at" <+> locDoc l)]) loc

diffDoc :: Diff -> Doc Ann
diffDoc = PP.vsep . diffDocs

statsDoc :: Stats -> Doc Ann
statsDoc stats
  | stats.invalid == 0 = PP.pretty stats.valid <+> "tests"
  | otherwise =
      PP.pretty stats.valid <+> "tests" <+> PP.parens (PP.pretty stats.invalid <+> "discarded")

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
