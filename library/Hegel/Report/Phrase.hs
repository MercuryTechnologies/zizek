-- | The fixed wording for every sentence that trace renderers emit.
--
-- The spine's margin\/elision\/footer text composes its sentences exclusively
-- from these fields plus /quoted/ user data.
module Hegel.Report.Phrase
  ( PhraseTable (..),
    english,
    firstLine,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Report.Trace.Blame (Fact (..))

-- | Fact-clause fields take the /resolved display name/ of the fact's value.
data PhraseTable = PhraseTable
  { -- | A fact as an earlier observation: @\"h₁ consumed\"@ (bare participle,
    -- matching 'origin', so it stays terse and pluralizes).
    observed :: Fact -> Text -> Text,
    -- | The origin line for a value born in machine setup:
    -- @\"v₁ initialized\"@ (a bare participle, so it pluralizes:
    -- @\"v₁, v₂ initialized\"@).
    origin :: Text -> Text,
    -- | The trajectory lead's body: @\"v₁: open \@1 · use \@2\"@.
    trajectory :: Text -> [(Text, Text)] -> Text,
    -- | An elision row's label: @\"2 steps, none touch h₁\"@.
    elidedSteps :: Int -> Maybe Text -> Text,
    -- | The elided-lifelines footer: @\"1 lifeline elided (h₂ · 1 step)\"@.
    elidedLifelines :: Int -> [Text] -> Maybe Int -> Text,
    -- | The citation list, given pre-formatted step tokens:
    -- @\"cites setup, \@1, \@4\"@.
    cites :: [Text] -> Text,
    -- | The reproduction footer, given the database key:
    -- @\"stored: k — replays automatically next run\"@.
    stored :: Text -> Text
  }

-- | The default phrase table (English wording).
english :: PhraseTable
english =
  PhraseTable
    { observed = \fact name -> case fact of
        BornAt _ -> name <> " created"
        TouchedAt _ -> name <> " accessed"
        ConsumedAt _ -> name <> " consumed"
        TransferredAt _ -> name <> " transferred",
      origin = \name -> name <> " initialized",
      trajectory = \name steps -> name <> ": " <> T.intercalate " · " [rule <> " @" <> n | (rule, n) <- steps],
      elidedSteps = \n mSubject ->
        counted n "step" <> maybe "" (", none touch " <>) mSubject,
      elidedLifelines = \n names mSteps ->
        counted n "lifeline"
          <> " elided ("
          <> T.unwords names
          <> maybe "" (\k -> " · " <> counted k "step") mSteps
          <> ")",
      cites = \steps -> "cites " <> T.intercalate ", " steps,
      stored = \key -> "stored: " <> key <> " — replays automatically next run"
    }
  where
    counted :: Int -> Text -> Text
    counted n noun =
      T.pack (show n) <> " " <> noun <> (if n == 1 then "" else "s")

-- | Quote only the first physical line of user text where a single line is
-- structurally required.
firstLine :: Text -> Text
firstLine = T.takeWhile (/= '\n')
