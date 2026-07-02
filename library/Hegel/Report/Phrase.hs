-- | The fixed wording for every sentence that trace renderers emit.
--
-- The headline and the spine's arrowhead\/elision\/footer text compose
-- their sentences exclusively from these fields plus /quoted/ user data.
module Hegel.Report.Phrase
  ( PhraseTable (..),
    english,
    firstLine,
  )
where

import Data.Char qualified as Char
import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Report.Trace.Blame (Fact (..))

-- | Fact-clause fields take the /resolved display name/ of the fact's value.
data PhraseTable = PhraseTable
  { -- | A fact as an earlier observation: @\"h₁ was consumed\"@.
    observed :: Fact -> Text -> Text,
    -- | A step reference with its rule: @\"step 4 (close)\"@.
    stepRef :: Text -> Text -> Text,
    -- | The outcome from a declared response: @\"read_closed returned Just …\"@.
    returned :: Text -> Text -> Text,
    -- | The headline for a failing fact: @\"Step 5 (verify): expected Nothing\"@.
    failedReason :: Text -> Text -> Text,
    -- | The trajectory lead's body: @\"v₁: open \@1 · use \@2\"@.
    trajectory :: Text -> [(Text, Text)] -> Text,
    terminal :: Text,
    -- | An elision row's label: @\"2 steps, none touch h₁\"@.
    elidedSteps :: Int -> Maybe Text -> Text,
    -- | The elided-lifelines footer: @\"1 lifeline elided (h₂ · 1 step)\"@.
    elidedLifelines :: Int -> [Text] -> Maybe Int -> Text,
    -- | The numeric citation fallback: @\"cites 5, 4, 1\"@.
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
        BornAt _ -> name <> " was created"
        TouchedAt _ -> name <> " was accessed"
        ConsumedAt _ -> name <> " was consumed"
        TransferredAt _ -> name <> " was transferred",
      stepRef = \n rule -> "step " <> n <> " (" <> rule <> ")",
      returned = \rule response -> rule <> " returned " <> response,
      failedReason = \ref reason -> capitalize ref <> ": " <> reason,
      trajectory = \name steps -> name <> ": " <> T.intercalate " · " [rule <> " @" <> n | (rule, n) <- steps],
      terminal = ".",
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
    capitalize t = case T.uncons t of
      Just (c, rest) -> T.cons (Char.toUpper c) rest
      Nothing -> t

-- | Quote only the first physical line of user text where a single line is
-- structurally required.
firstLine :: Text -> Text
firstLine = T.takeWhile (/= '\n')
