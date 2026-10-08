-- | Result of a property run, and its human-readable rendering.
module Hegel.Report
  ( -- * Reports
    Report (..),
    Result (..),
    FailureOutcome (..),
    FailureEvidence (..),
    FailureEvidenceStatus (..),
    ReplayReason (..),
    Abort (..),
    ReplayDivergence (..),
    replayDivergenceReason,
    Stats (..),
    Reproduction (..),
    aborted,
    throwOnFailure,

    -- * Notes (re-exported from "Hegel.Report.Note")
    Note (..),
    NoteKind (..),
    isDrawn,
    isFailureNote,
    isBranchHeader,
    isBranchFailure,

    -- * Events (re-exported from "Hegel.Internal.Event")
    Event (..),
    Operation (..),
    Var (..),

    -- * Tick (re-exported from "Hegel.Internal.Tick")
    Tick (..),

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
import Data.Foldable (toList)
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (catMaybes, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Stack (SrcLoc (..))
import Hegel.Diff (Diff)
import Hegel.Internal.Event (Event (..), Operation (..), Var (..))
import Hegel.Internal.Tick (Tick (..))
import Hegel.Replay (ReplayToken, encodeReplayToken)
import Hegel.Report.Ann (Ann (..), docToAnsi, docToText)
import Hegel.Report.Concurrent (concurrentGroupsDoc)
import Hegel.Report.Discovery (Declarations, loadDeclarations)
import Hegel.Report.Journal (footnoteDocs, headlineBlock, journalDocs)
import Hegel.Report.Layout qualified as Layout
import Hegel.Report.Note (Note (..), NoteKind (..), hasInBandFailure, isBranchFailure, isBranchHeader, isDrawn, isFailureNote, renderValue)
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
import Hegel.Report.Stateful (JournalShape (..), classifyJournal, failingGroupDoc, noteFiles)
import Hegel.Report.Style (PhraseTable, Style (..), defaultStyle)
import Hegel.Report.Style qualified as Style
import Hegel.Report.Trace (Trace)
import Hegel.Report.Trace qualified as Trace
import Prettyprinter (Doc, (<+>))
import Prettyprinter qualified as PP

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
    -- | Where a failure from this run can be found again.
    reproduction :: !Reproduction
  }
  deriving stock (Show)

-- | Where a failure from this run can be found again.
data Reproduction
  = -- | Filed in the example database under this key, which the
    -- 'Hegel.Phase.Reuse' phase replays on the next run.
    Stored !Text
  | -- | Nothing was filed.
    Unstored
  | -- | The engine produced no reproduce blob for the first failure, as for
    -- a nondeterministic failure it could not confirm.
    Unreproducible
  deriving stock (Show, Eq)

-- | The verdict of a property run.
data Result
  = -- | Every attempted test case passed.
    Ok
  | -- | Every distinct failure the engine reported, in engine order.
    Failures !(NonEmpty FailureOutcome)
  | -- | No valid examples were generated.
    GaveUp Text
  | -- | The run stopped before reaching a verdict.
    Aborted Abort
  deriving stock (Show)

-- | An explicit replay that did not reproduce its token's failure.
newtype ReplayDivergence = ReplayDivergence
  { replayReason :: ReplayReason
  }
  deriving stock (Show, Eq)

-- | Why an explicit replay did not reproduce its token's failure.
data ReplayReason
  = -- | Every replay the engine attempted within its budget passed.
    DidNotReproduce
  | -- | The replay failed, but at this origin rather than the token's.
    ChangedOrigin !Text
  | -- | The engine rejected the token's blob without running it.
    InvalidReplayBlob !Text
  | -- | The token came from a different @libhegel@ version than this run's.
    IncompatibleVersions {tokenVersion :: !Text, engineVersion :: !Text}
  deriving stock (Show, Eq)

-- | What is known about one reported failure.
data FailureEvidence = FailureEvidence
  { message :: !Text,
    notes :: ![Note],
    events :: ![Event],
    loc :: !(Maybe SrcLoc),
    diff :: !(Maybe Diff)
  }
  deriving stock (Show)

-- | The evidence a report carries for one engine failure.
data FailureEvidenceStatus
  = -- | The diagnostic captured from a failing case with this failure's origin.
    Captured !FailureEvidence
  | -- | No failing case with this origin was seen, so only the origin is known.
    Uncaptured
  | -- | An explicit replay did not reproduce the token's failure.
    Diverged !ReplayDivergence
  deriving stock (Show)

-- | One engine failure: its identity, its evidence, and how to replay it.
data FailureOutcome = FailureOutcome
  { failureOrigin :: !Text,
    -- | A token for 'Hegel.Runner.replay', present whenever the engine
    -- produced a reproduce blob.
    failureReplayToken :: !(Maybe ReplayToken),
    -- | How reliably a failure found under nondeterministic handling
    -- reproduced, quoting the engine's replay evidence.
    failureCaveat :: !(Maybe Text),
    failureEvidence :: !FailureEvidenceStatus
  }
  deriving stock (Show)

replayDivergenceReason :: ReplayDivergence -> Text
replayDivergenceReason = renderReplayReason . (.replayReason)

renderReplayReason :: ReplayReason -> Text
renderReplayReason = \case
  DidNotReproduce -> "every replay passed, so the failure may have been fixed or may not have recurred"
  ChangedOrigin origin -> "replay failed at a different origin: " <> origin
  InvalidReplayBlob detail -> "the replay token was rejected by the engine: " <> detail
  IncompatibleVersions {tokenVersion, engineVersion} ->
    "token was produced by libhegel " <> tokenVersion <> ", but this run uses " <> engineVersion

-- | Why a run stopped without reaching a verdict.
data Abort
  = -- | An exception other than a property failure escaped the runner.
    Errored SomeException
  | -- | A health check failed before the property ran.
    UnhealthyInput Text
  deriving stock (Show)

-- | A report for a run that stopped before any test case could run.
aborted :: Abort -> Report
aborted a = Report {result = Aborted a, stats = Stats {valid = 0, invalid = 0}, reproduction = Unstored}

-- | Throw on anything other than 'Ok': 'PropertyFailed' on a counterexample,
-- the original exception on 'Errored', and 'fail' otherwise.
throwOnFailure :: Report -> IO ()
throwOnFailure report = case report.result of
  Ok -> pure ()
  Failures {} -> throwIO (PropertyFailed report)
  GaveUp msg -> fail ("Property rejected all inputs: " <> show msg)
  Aborted (Errored exc) -> throwIO exc
  Aborted (UnhealthyInput msg) -> fail ("Health check failed: " <> show msg)

-- * Pure rendering (always succeeds, no IO)

-- | Render a report as plain text.
renderReport :: Report -> Text
renderReport = docToText . reportDoc

-- | Render a report with ANSI color codes (suitable for TTY output).
-- Diff lines are red\/green; the failure message is bold; location is dim.
renderReportAnsi :: Report -> Text
renderReportAnsi = docToAnsi . reportDoc

-- | Render the failure section alone
renderFailure :: Text -> [Note] -> Maybe SrcLoc -> Maybe Diff -> Reproduction -> Text
renderFailure message notes loc diff reproduction = docToText (withFooter Style.english reproduction body)
  where
    -- In-band journals retain their own failure headline.
    body
      | hasInBandFailure notes = PP.vsep [headlineDoc message, failureDoc message notes loc diff]
      | otherwise = failureDoc message notes loc diff

-- * Source-aware rendering (reads files; falls back to plain when none is readable)

-- | Render a report as plain text, splicing drawn values and the failure
-- message inline into a source listing — and, for stateful failures with
-- pool context, composing the chronological event log above the failing
-- step's splice (see 'renderReportRichWith' for the form selection).
-- Reads source files at render time; degrades to 'renderReport' when no
-- source is readable.
renderReportRich :: Report -> IO Text
renderReportRich = renderReportRichWith (defaultStyle Style.unicode)

-- | 'renderReportRich' with ANSI color codes. Degrades to 'renderReportAnsi'
-- when no source is readable.
renderReportRichAnsi :: Report -> IO Text
renderReportRichAnsi = renderReportRichAnsiWith (defaultStyle Style.unicode)

-- | 'renderReportRich' with an explicit 'Style' (glyph table, phrase table,
-- budgets).
renderReportRichWith :: Style -> Report -> IO Text
renderReportRichWith style = renderRichImpl style renderReport docToText

-- | 'renderReportRichAnsi' with an explicit 'Style'.
renderReportRichAnsiWith :: Style -> Report -> IO Text
renderReportRichAnsiWith style = renderRichImpl style renderReportAnsi docToAnsi

-- | The renderer the framework integrations call: rich, ANSI per @useColor@,
-- glyphs per the output 'Style.Preference', with the ascii preference's
-- 7-bit-clean guarantee applied to the whole result. Keeps the
-- render-then-clean invariant in one place instead of one per framework.
renderReportAuto :: Bool -> Style.Preference -> Report -> IO Text
renderReportAuto useColor pref report =
  Style.cleanFor pref
    <$> (if useColor then renderReportRichAnsiWith style else renderReportRichWith style) report
  where
    style = defaultStyle (Style.table pref)

-- | Shared implementation of the rich renderers, parameterised over the
-- plain-text fallback and the final document renderer.
renderRichImpl :: Style -> (Report -> Text) -> (Doc Ann -> Text) -> Report -> IO Text
renderRichImpl style plain toText report = do
  mdoc <- richDoc style report
  pure case mdoc of
    Nothing -> plain report
    Just body ->
      toText (withFooter style.phrases report.reproduction (PP.vsep [failureSummary report, body]))

-- | Render every failure outcome with source-aware evidence where available.
richDoc :: Style -> Report -> IO (Maybe (Doc Ann))
richDoc style report = case report.result of
  Failures outcomes -> do
    let total = length (toList outcomes)
    rendered <- traverse (\(index, outcome) -> failureRichOutcomeDoc style total index outcome) (zip [1 :: Int ..] (toList outcomes))
    pure (Just (PP.vsep rendered))
  _ -> pure Nothing

-- | Assemble a stateful failure report from its sections, rendered in order
-- and separated by blank lines.
composed ::
  Style ->
  Declarations ->
  Trace ->
  [Note] ->
  Text ->
  Maybe SrcLoc ->
  Maybe Diff ->
  Doc Ann
composed style decls trace notes message loc diff =
  PP.vsep (PP.punctuate PP.line (catMaybes sections))
  where
    sections =
      [ preludeDoc notes message loc diff,
        Just $ Layout.logDoc style trace,
        failingGroupDoc decls notes,
        footnotesDoc notes
      ]

-- | Assemble a concurrent-combinator failure report.
composedConcurrent :: PhraseTable -> Declarations -> [Note] -> Text -> Maybe SrcLoc -> Maybe Diff -> Maybe (Doc Ann)
composedConcurrent phrases decls notes message loc diff = do
  body <- concurrentGroupsDoc phrases decls notes
  pure (PP.vsep (PP.punctuate PP.line (catMaybes [preludeDoc notes message loc diff, Just body, footnotesDoc notes])))

-- | The optional headline\/diff\/location block leading a composed report.
-- Dropped when the journal already carries its own in-band failure, a
-- stateful step's 'Failure' or a concurrent branch's 'BranchFailure', since
-- that note's own headline would otherwise be repeated.
--
-- Kept when no note anchors the reason, as when an exception escapes
-- mid-loop, every branch of a concurrent combinator succeeds and a later
-- assertion fails the case, or @machine.initial@ fails at depth 0.
preludeDoc :: [Note] -> Text -> Maybe SrcLoc -> Maybe Diff -> Maybe (Doc Ann)
preludeDoc notes message loc diff
  | hasInBandFailure notes = Nothing
  | otherwise = Just $ PP.vsep $ headlineBlock message diff loc

-- | Footnote notes, rendered after the report body (their documented
-- position, regardless of form).
footnotesDoc :: [Note] -> Maybe (Doc Ann)
footnotesDoc notes = case footnoteDocs notes of
  [] -> Nothing
  ds -> Just (PP.vsep ds)

-- | The reproduction footer.
footerDoc :: Style.PhraseTable -> Reproduction -> Maybe (Doc Ann)
footerDoc phrases = \case
  Stored key -> Just (footerLine (phrases.stored key))
  Unstored -> Nothing
  Unreproducible -> Just (footerLine phrases.unreproducible)
  where
    footerLine :: Text -> Doc Ann
    footerLine = PP.annotate LocAnn . PP.pretty

-- | Append the reproduction footer to a rendered failure body.
withFooter :: Style.PhraseTable -> Reproduction -> Doc Ann -> Doc Ann
withFooter phrases reproduction body = case footerDoc phrases reproduction of
  Nothing -> body
  Just f -> PP.vsep [body <> PP.line, f]

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
      -- 'declDocs' is punctuated on its own: two listings from different
      -- files get a blank line between their boxes, matching the gap
      -- 'composed' puts between sections. 'args' and 'footerDocs' stay
      -- tightly stacked, one line per entry.
      sections =
        [PP.vsep ds | ds <- [args], not (null ds)]
          <> [PP.vsep (PP.punctuate PP.line declDocs) | not (null declDocs)]
          <> [PP.vsep ds | ds <- [footerDocs], not (null ds)]
  -- Degrade to the plain renderer unless at least one declaration rendered;
  -- the @Draw N:@ fallback docs in 'args' only supplement a source
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
  Failures outcomes ->
    withFooter
      Style.english
      report.reproduction
      (PP.vsep (failureSummary report : rendered))
    where
      rendered = zipWith (renderOutcome (length (toList outcomes))) [1 :: Int ..] (toList outcomes)

failureSummary :: Report -> Doc Ann
failureSummary report = case report.result of
  Failures (outcome :| []) -> singletonSummary outcome.failureEvidence <+> "after" <+> statsDoc report.stats
  Failures outcomes ->
    "failed with"
      <+> PP.pretty (length outcomes)
      <+> "distinct failures"
      <> qualifiers outcomes
      <+> "after"
      <+> statsDoc report.stats
  _ -> "failed after" <+> statsDoc report.stats
  where
    -- Only failures without captured evidence are called out.
    qualifiers :: NonEmpty FailureOutcome -> Doc Ann
    qualifiers outcomes = case unusual outcomes of
      [] -> mempty
      xs -> PP.space <> PP.parens (PP.hsep (PP.punctuate "," xs))
    unusual :: NonEmpty FailureOutcome -> [Doc Ann]
    unusual outcomes =
      [ PP.pretty n <+> label
      | (n, label) <-
          [ (length [() | Uncaptured <- statuses outcomes], "uncaptured"),
            (length [() | Diverged _ <- statuses outcomes], "replay diverged")
          ],
        n > 0
      ]
    statuses :: NonEmpty FailureOutcome -> [FailureEvidenceStatus]
    statuses = fmap (.failureEvidence) . toList

singletonSummary :: FailureEvidenceStatus -> Doc Ann
singletonSummary = \case
  Diverged _ -> "replay diverged"
  _ -> "failed"

-- | The headline @message@ line of a failure report.
headlineDoc :: Text -> Doc Ann
headlineDoc = PP.annotate MessageAnn . PP.pretty

-- | Render the failure body.
failureDoc :: Text -> [Note] -> Maybe SrcLoc -> Maybe Diff -> Doc Ann
failureDoc message notes loc diff
  | hasInBandFailure notes = PP.vsep (journalDocs notes)
  | otherwise = PP.vsep (headlineBlock message diff loc <> journalDocs notes)

-- | Render pool evidence together with the failure journal.
failureEventsDoc :: Text -> [Note] -> [Event] -> Maybe SrcLoc -> Maybe Diff -> Doc Ann
failureEventsDoc message notes events loc diff
  | null events = failureDoc message notes loc diff
  | otherwise = case classifyJournal notes of
      StatefulShape -> composed (defaultStyle Style.unicode) mempty (Trace.build notes events) notes message loc diff
      ConcurrentShape -> failureDoc message notes loc diff
      PlainShape -> failureDoc message notes loc diff

renderFailureEvidence :: FailureEvidence -> Doc Ann
renderFailureEvidence evidence =
  failureEventsDoc evidence.message evidence.notes evidence.events evidence.loc evidence.diff

renderOutcome :: Int -> Int -> FailureOutcome -> Doc Ann
renderOutcome total i outcome =
  outcomeFrame total i outcome (renderEvidence outcome)

-- | Surround an outcome's body with its heading, when the report has several
-- failures, and its caveat and replay token.
outcomeFrame :: Int -> Int -> FailureOutcome -> Doc Ann -> Doc Ann
outcomeFrame total i outcome body =
  PP.vsep $
    [outcomeHeading i outcome | total > 1]
      <> [body]
      <> [caveatDoc caveat | Just caveat <- [outcome.failureCaveat]]
      <> [replayTokenDoc token | Just token <- [outcome.failureReplayToken]]

outcomeHeading :: Int -> FailureOutcome -> Doc Ann
outcomeHeading index outcome =
  PP.annotate MessageAnn (PP.pretty ("failure " <> T.pack (show index) <> status))
  where
    status = case outcome.failureEvidence of
      Captured _ -> ""
      Uncaptured -> " (uncaptured)"
      Diverged _ -> " (replay diverged)"

-- | The plain rendering of an outcome's evidence.
renderEvidence :: FailureOutcome -> Doc Ann
renderEvidence outcome = case outcome.failureEvidence of
  Captured evidence -> renderFailureEvidence evidence
  Uncaptured -> uncapturedDoc outcome.failureOrigin
  Diverged divergence -> PP.pretty (replayDivergenceReason divergence)

-- | A failure known only by its origin.
uncapturedDoc :: Text -> Doc Ann
uncapturedDoc origin =
  PP.vsep
    [ headlineDoc origin,
      PP.annotate NoteAnn "no failing case with this origin was captured"
    ]

caveatDoc :: Text -> Doc Ann
caveatDoc caveat = PP.annotate NoteAnn (PP.pretty ("note: " <> caveat))

replayTokenDoc :: ReplayToken -> Doc Ann
replayTokenDoc token =
  PP.vsep
    [ PP.annotate LocAnn (PP.pretty ("replay token: " <> encodeReplayToken token)),
      PP.annotate LocAnn "decode with Hegel.decodeReplayToken, then run Hegel.replay settings token property"
    ]

failureRichDoc :: Style -> FailureEvidence -> IO (Doc Ann)
failureRichDoc style evidence = do
  body <- case classifyJournal evidence.notes of
    StatefulShape -> do
      decls <- loadDeclarations (noteFiles evidence.notes)
      let trace = Trace.build evidence.notes evidence.events
      pure (composed style decls trace evidence.notes evidence.message evidence.loc evidence.diff)
    ConcurrentShape -> do
      decls <- loadDeclarations (noteFiles evidence.notes)
      pure (maybe (failureDoc evidence.message evidence.notes evidence.loc evidence.diff) id (composedConcurrent style.phrases decls evidence.notes evidence.message evidence.loc evidence.diff))
    PlainShape -> do
      mdoc <- plainRichDoc evidence.message evidence.notes evidence.loc evidence.diff
      pure (maybe (failureDoc evidence.message evidence.notes evidence.loc evidence.diff) id mdoc)
  pure body

failureRichOutcomeDoc :: Style -> Int -> Int -> FailureOutcome -> IO (Doc Ann)
failureRichOutcomeDoc style total index outcome = do
  body <- case outcome.failureEvidence of
    Captured evidence -> failureRichDoc style evidence
    _ -> pure (renderEvidence outcome)
  pure (outcomeFrame total index outcome body)

statsDoc :: Stats -> Doc Ann
statsDoc stats
  | stats.invalid == 0 = PP.pretty stats.valid <+> "tests"
  | otherwise =
      PP.pretty stats.valid <+> "tests" <+> PP.parens (PP.pretty stats.invalid <+> "discarded")

-- * Exceptions

-- | The complete failure report thrown by 'Hegel.prop' and 'Hegel.Property.check_'.
newtype PropertyFailed = PropertyFailed
  { report :: Report
  }
  deriving stock (Show)

instance Exception PropertyFailed where
  displayException = T.unpack . renderReport . (.report)
