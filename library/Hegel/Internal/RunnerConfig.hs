-- | Runner overrides and identity-targeted execution.
module Hegel.Internal.RunnerConfig where

import Control.Applicative ((<|>))
import Control.Exception (displayException)
import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hegel.Database (Database (..))
import Hegel.Property.Internal (Property)
import Hegel.Replay (ReplayToken, decodeReplayToken)
import Hegel.Report (Report)
import Hegel.Runner qualified as Runner
import Hegel.Seed (Seed (..))
import Hegel.Settings (Settings (..))
import Hegel.Settings qualified as Settings
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- | Values supplied by one configuration source: settings overrides plus a
-- request to replay one identity.
data Overrides = Overrides
  { settings :: Settings,
    replay :: Maybe (Text, ReplayToken)
  }
  deriving stock (Show)

-- | Keeps everything the right-hand side supplies.
instance Semigroup Overrides where
  low <> high = Overrides (low.settings <> high.settings) (high.replay <|> low.replay)

instance Monoid Overrides where
  mempty = Overrides mempty Nothing

-- | Names accepted by both runner integrations.
settingNames :: [String]
settingNames = ["test-cases", "seed", "database", "replay", "replay-key"]

-- | Parse one source, requiring replay identity and token together.
--
-- Seeds and databases use the vocabulary of the engine's own @HEGEL_SEED@ and
-- @HEGEL_DATABASE@ variables.
parseOverrides :: (String -> Maybe String) -> Either String Overrides
parseOverrides get = do
  testCases <- optional "test-cases" (settingInteger "test-cases" (\n -> Settings.defaultSettings {Settings.testCases = Just n}))
  seed <- optional "seed" parseSeed
  database <- optional "database" parseDatabase
  replay <- case (get "replay", get "replay-key") of
    (Nothing, Nothing) -> Right Nothing
    (Just token, Just key) | not (null key) ->
      case decodeReplayToken (T.pack token) of
        Left err -> Left ("hegel-replay: " <> show err)
        Right decoded -> Right (Just (T.pack key, decoded))
    _ -> Left "hegel-replay and hegel-replay-key: supply a token and nonempty exact identity together in one source"
  pure (Overrides Settings.defaultSettings {testCases, seed, database} replay)
  where
    optional :: String -> (String -> Either String a) -> Either String (Maybe a)
    optional name parser = traverse parser (get name)

-- | Parse an Int setting and apply its shared numeric contract.
settingInteger :: String -> (Int -> Settings) -> String -> Either String Int
settingInteger name configure input = case readMaybe input :: Maybe Integer of
  Just value
    | let digits = case input of '-' : rest -> rest; _ -> input,
      not (null digits),
      all (\c -> isDigit c && c <= '9') digits,
      value >= toInteger (minBound :: Int),
      value <= toInteger (maxBound :: Int) ->
        let n = fromInteger value
         in either (Left . (("hegel-" <> name <> ": ") <>) . displayException) (const (Right n)) (Settings.validate (configure n))
  _ -> Left ("hegel-" <> name <> ": expected a decimal Int")

-- | Parse @none@ for a fresh seed, or a fixed seed's decimal digits.
parseSeed :: String -> Either String Seed
parseSeed "none" = Right SeedFresh
parseSeed input = case readMaybe input :: Maybe Integer of
  Just value
    | not (null input),
      all (\c -> isDigit c && c <= '9') input,
      value <= toInteger (maxBound :: Word64) ->
        Right (SeedFixed (fromInteger value))
  _ -> Left ("hegel-seed: expected none or an integer in [0, " <> show (toInteger (maxBound :: Word64)) <> "]")

-- | Parse @disabled@ for no persistence, or the root directory of a store.
parseDatabase :: String -> Either String Database
parseDatabase "disabled" = Right DatabaseDisabled
parseDatabase "" = Left "hegel-database: expected disabled or a nonempty directory path"
parseDatabase path = Right (DatabaseDirectory path)

-- | Read the replay request from @HEGEL_REPLAY@ and @HEGEL_REPLAY_KEY@ at
-- example execution time.
--
-- The engine reads its own @HEGEL_*@ settings variables when it resolves a
-- run's profile, so they are not read here.
readOverrides :: IO (Either String Overrides)
readOverrides = do
  replay <- lookupEnv "HEGEL_REPLAY"
  replayKey <- lookupEnv "HEGEL_REPLAY_KEY"
  pure (parseOverrides (`lookup` [(name, value) | (name, Just value) <- [("replay", replay), ("replay-key", replayKey)]]))

-- | Apply a source's overrides over the settings given in code.
resolve :: (HasCallStack) => Settings -> Overrides -> Either String Settings
resolve settings overrides =
  let resolved = settings <> overrides.settings
   in either (Left . displayException) (const (Right resolved)) (withFrozenCallStack (Settings.validate resolved))

-- | Replay the exact matching identity once, or run ordinary exploration.
execute :: (HasCallStack) => (Int -> IO ()) -> Settings -> Overrides -> Property () -> IO Report
execute progress settings overrides body = case overrides.replay of
  Just (key, token) | settings.databaseKey == Just key -> Runner.replay settings token body
  _ -> Runner.checkWithProgress progress settings body

-- | Runner-specific instructions accompanying the report's replay tokens.
replayInstructions :: Bool -> Settings -> Overrides -> Text
replayInstructions native settings overrides = case settings.databaseKey of
  Nothing -> ""
  Just key ->
    let selected = case overrides.replay of
          Just (requested, _) | requested == key -> "Replay selected for identity: " <> key <> "\n"
          _ -> ""
        command =
          if native
            then "--hegel-replay-key " <> quote key <> " --hegel-replay TOKEN"
            else "HEGEL_REPLAY_KEY=" <> quote key <> " HEGEL_REPLAY=TOKEN"
     in "\n" <> selected <> "Hegel identity: " <> key <> "\nReplay with " <> command <> " and filter the runner to this test.\n"
  where
    quote value = "'" <> T.replace "'" "'\\''" value <> "'"
