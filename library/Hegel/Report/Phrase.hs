-- | The phrase table: every word the trace renderers can emit.
--
-- The prose twin of "Hegel.Report.Glyph" (tenet 3: content is data, words
-- are tables applied last). Both the verdict paragraph and the ledger's
-- arrowhead\/elision\/footer text compose their sentences exclusively from
-- these fields plus /quoted/ user data (value names, responses, failure
-- messages — never inflected), so the renderers' words agree by
-- construction and the whole linguistic surface is auditable by reading
-- 'english'. Each field produces a complete clause, keeping word-order
-- decisions inside the field where another locale can survive them.
module Hegel.Report.Phrase
  ( PhraseTable (..),
    english,
  )
where

import Data.Char qualified as Char
import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Report.Blame (Fact (..))

-- | Fact-clause fields take the /resolved display name/ of the fact's value.
data PhraseTable = PhraseTable
  { -- | The violation's fact, as the failing step's action:
    -- @\"touched h₁ after its death\"@.
    violates :: Fact -> Text -> Text,
    -- | A fact as an earlier observation — the verdict's \"since\" clauses
    -- /and/ the ledger's arrowhead annotations: @\"h₁ was consumed\"@.
    caused :: Fact -> Text -> Text,
    -- | A step reference with its rule: @\"step 4 (close)\"@.
    stepRef :: Text -> Text -> Text,
    -- | The violation lead-in: step reference + violation clause →
    -- @\"Step 5 (read_closed) touched h₁ after its death\"@.
    lead :: Text -> Text -> Text,
    -- | Joins the lead to its justifications.
    causeIntro :: Text,
    -- | Between justifications.
    causeSep :: Text,
    -- | Joins a justification clause to its step reference.
    at :: Text,
    -- | Joins the story to the observed outcome.
    but :: Text,
    -- | The outcome from a declared response: @\"read_closed returned Just …\"@.
    returned :: Text -> Text -> Text,
    -- | The outcome from the failure message.
    failedWith :: Text -> Text,
    terminal :: Text,
    -- | An elision row's label: count + (when the elided steps never touch
    -- the subject) the subject's name — @\"2 steps, none touch h₁\"@.
    elidedSteps :: Int -> Maybe Text -> Text,
    -- | The elided-lifelines footer (sans sigil): count, names, and the
    -- number of steps that never rendered —
    -- @\"1 lifeline elided (h₂ · 1 step)\"@.
    elidedLifelines :: Int -> [Text] -> Maybe Int -> Text,
    -- | The numeric citation fallback (sans sigil): @\"cites 5, 4, 1\"@.
    cites :: [Text] -> Text
  }

english :: PhraseTable
english =
  PhraseTable
    { violates = \fact name -> case fact of
        BornAt _ -> "created " <> name
        TouchedAt _ -> "touched " <> name
        ConsumedAt _ -> "consumed " <> name
        HauntedAt _ -> "touched " <> name <> " after its death",
      caused = \fact name -> case fact of
        BornAt _ -> name <> " was created"
        TouchedAt _ -> name <> " was touched"
        ConsumedAt _ -> name <> " was consumed"
        HauntedAt _ -> name <> " was touched after its death",
      stepRef = \n rule -> "step " <> n <> " (" <> rule <> ")",
      lead = \ref clause -> capitalize ref <> " " <> clause,
      causeIntro = ": ",
      causeSep = ", ",
      at = " at ",
      but = " — but ",
      returned = \rule response -> rule <> " returned " <> response,
      failedWith = \message -> "it failed: " <> message,
      terminal = ".",
      elidedSteps = \n mSubject ->
        counted n "step" <> maybe "" (", none touch " <>) mSubject,
      elidedLifelines = \n names mSteps ->
        counted n "lifeline"
          <> " elided ("
          <> T.unwords names
          <> maybe "" (\k -> " · " <> counted k "step") mSteps
          <> ")",
      cites = \steps -> "cites " <> T.intercalate ", " steps
    }
  where
    counted :: Int -> Text -> Text
    counted n noun =
      T.pack (show n) <> " " <> noun <> (if n == 1 then "" else "s")
    capitalize t = case T.uncons t of
      Just (c, rest) -> T.cons (Char.toUpper c) rest
      Nothing -> t
