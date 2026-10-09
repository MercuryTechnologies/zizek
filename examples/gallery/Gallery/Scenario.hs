-- | What every gallery scenario provides, and helpers for stating what its
-- report must contain.
module Gallery.Scenario
  ( -- * Scenarios
    Scenario (..),
    gallerySeed,
    seeded,

    -- * Expectations
    ensure,
    ensureEqual,
    captured,
    LogRow (..),
    logRows,
    stepRules,
  )
where

import Data.Foldable (toList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Hegel.Property (Property)
import Hegel.Report (FailureEvidence (..), FailureEvidenceStatus (..), FailureOutcome (..), Report (..), Result (..))
import Hegel.Report.Layout qualified as Layout
import Hegel.Report.Style (Cell (..), defaultStyle)
import Hegel.Report.Style qualified as Style
import Hegel.Report.Trace qualified as Trace
import Hegel.Seed (Seed (..))
import Hegel.Settings (Settings (..), defaultSettings)

-- | One deliberately failing property and the report shape it pins.
data Scenario = Scenario
  { -- | A short identifier for @--check@ and @--sweep@ output.
    name :: Text,
    -- | The banner printed above the rendered report.
    title :: Text,
    settings :: Settings,
    property :: Property (),
    -- | Whether to print a seven-bit ascii rendering after the unicode one.
    ascii :: Bool,
    -- | How many runs @--check@ may take to see the expected report, more
    -- than one only for a scenario whose failure depends on thread timing.
    attempts :: Int,
    -- | Every way the report departs from the shape this scenario pins,
    -- empty when it matches.
    expect :: Report -> IO [Text]
  }

-- | The seed every deterministic scenario runs under, so each run renders the
-- same report.
gallerySeed :: Word64
gallerySeed = 20261008

-- | 'defaultSettings' pinned to 'gallerySeed'.
seeded :: Settings
seeded = defaultSettings {seed = Just (SeedFixed gallerySeed)}

-- | A mismatch when the condition fails.
ensure :: Bool -> Text -> [Text]
ensure ok msg = [msg | not ok]

-- | A mismatch naming both values when they differ.
ensureEqual :: (Eq a, Show a) => Text -> a -> a -> [Text]
ensureEqual what expected actual =
  ensure (expected == actual) (what <> ": expected " <> T.pack (show expected) <> ", got " <> T.pack (show actual))

-- | The captured evidence of every failure in the report, or a mismatch when
-- the run did not fail or a failure arrived without evidence.
captured :: Report -> Either Text [FailureEvidence]
captured report = case report.result of
  Failures outcomes -> traverse evidenceOf (toList outcomes)
  other -> Left ("expected a failure, got " <> T.pack (show other))
  where
    evidenceOf :: FailureOutcome -> Either Text FailureEvidence
    evidenceOf o = case o.failureEvidence of
      Captured e -> Right e
      _ -> Left ("failure at " <> o.failureOrigin <> " carries no captured evidence")

-- | One row of a stateful failure's event log, reduced to what a scenario can
-- pin without depending on glyph choice.
data LogRow = LogRow
  { failed :: Bool,
    elided :: Bool,
    call :: Text,
    origin :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | The event log the rich renderer draws for this failure, detail rows
-- omitted.
logRows :: FailureEvidence -> [LogRow]
logRows e =
  [ LogRow
      { failed = r.gutter == NodeFail,
        elided = r.kind == Layout.ElisionRow,
        call = r.call,
        origin = r.origin
      }
  | r <- Layout.layoutRows (defaultStyle Style.unicode) (Trace.build e.notes e.events),
    r.kind /= Layout.DetailRow
  ]

-- | The rule each row ran, or @"elided"@ for an elision row: the shape of a
-- log with its drawn values stripped.
stepRules :: [LogRow] -> [Text]
stepRules = map \r -> if r.elided then "elided" else T.takeWhile (/= ' ') r.call
