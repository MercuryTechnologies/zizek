-- | Ten concurrent branches, two of which fail on their own.
--
-- A storefront renders each price for its customer's locale, and the checkout
-- parses the rendered text back into cents. The parser treats every comma as a
-- thousands separator, which holds for most locales but not for the ones that
-- write a decimal comma. There, one cent renders as @"0,01"@ and parses back as
-- a whole euro: the parse succeeds and is a hundred times too large.
--
-- Each locale runs in its own branch on its own stream of choices. The eight
-- passing branches collapse into a summary line, and the two failing ones
-- splice into the one declaration they share, each line labeled with its
-- branch.
module Gallery.Locales (scenario) where

import Data.Function ((&))
import Data.List (intercalate)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as T.Read
import Gallery.Scenario
import Hegel.Gen qualified as Gen
import Hegel.Property (Property, annotate, forAllWithLabel, (===))
import Hegel.Property.Branch qualified as Branch
import Hegel.Report (FailureEvidence (..), Report, isBranchFailure, renderReportRich, renderValue)

scenario :: Scenario
scenario =
  Scenario
    { name = "locales",
      title = "branches: a decimal comma read as a thousands separator",
      settings = seeded,
      property = prices,
      ascii = False,
      attempts = 1,
      expect = check
    }

data Locale = Locale
  { tag :: Text,
    decimal :: Char,
    grouping :: Char
  }

locales :: [Locale]
locales =
  [ Locale "en-US" '.' ',',
    Locale "en-GB" '.' ',',
    Locale "ja-JP" '.' ',',
    Locale "zh-CN" '.' ',',
    Locale "ko-KR" '.' ',',
    Locale "hi-IN" '.' ',',
    Locale "en-AU" '.' ',',
    Locale "es-MX" '.' ',',
    Locale "de-DE" ',' '.',
    Locale "pt-BR" ',' '.'
  ]

-- | Render an amount of cents with the locale's separators, e.g. @"1,234.50"@.
render :: Locale -> Int -> Text
render loc cents = T.pack (grouped <> [loc.decimal] <> pad (cents `mod` 100))
  where
    grouped = intercalate [loc.grouping] (reverse (map reverse (chunks (reverse (show (cents `div` 100))))))
    chunks :: String -> [String]
    chunks [] = []
    chunks s = take 3 s : chunks (drop 3 s)
    pad :: Int -> String
    pad n = (if n < 10 then "0" else "") <> show n

-- | Parse a rendered price back into cents.
--
-- BUG: every comma is dropped as a thousands separator.
parse :: Text -> Maybe Int
parse t = case T.splitOn "." (T.filter (/= ',') t) of
  [units] -> (* 100) <$> int units
  [units, cents] | T.length cents == 2 -> (\u c -> u * 100 + c) <$> int units <*> int cents
  _ -> Nothing
  where
    int :: Text -> Maybe Int
    int s = case T.Read.decimal s of
      Right (n, "") -> Just n
      _ -> Nothing

prices :: Property ()
prices =
  Branch.forConcurrently_ locales \loc -> do
    annotate loc.tag
    cents <- forAllWithLabel "cents" (Gen.int & Gen.min 1 & Gen.max 1_000_000 & Gen.build)
    let shown = render loc cents
    annotate ("rendered as " <> renderValue shown)
    parse shown === Just cents

check :: Report -> IO [Text]
check report = case captured report of
  Right [evidence] -> do
    rich <- renderReportRich report
    let failing = length (filter isBranchFailure evidence.notes)
    pure $
      ensureEqual "failing branches" 2 failing
        <> ensure ("8 branches passed" `T.isInfixOf` rich) "expected the passing branches to collapse into a summary"
  Right evidence -> pure ["expected one failure, got " <> renderValue (length evidence)]
  Left mismatch -> pure [mismatch]
