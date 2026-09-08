-- | Runner overrides and identity-targeted execution.
module Hegel.Internal.RunnerConfig where

import Control.Applicative ((<|>))
import Control.Exception (displayException)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hegel.Database (Database (..))
import Hegel.Property.Internal (Property)
import Hegel.Replay (ReplayToken, decodeReplayToken)
import Hegel.Report (Report)
import Hegel.Runner qualified as Runner
import Hegel.Settings (Settings (..))
import Hegel.Settings qualified as Settings
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- | Optional values supplied by one configuration source.
data Overrides = Overrides
  { testCases :: Maybe Int,
    statefulSteps :: Maybe Int,
    seed :: Maybe Word64,
    database :: Maybe Database,
    replay :: Maybe (Text, ReplayToken)
  }
  deriving stock (Show)

-- | Preserve every lower-priority setting.
emptyOverrides :: Overrides
emptyOverrides = Overrides Nothing Nothing Nothing Nothing Nothing

-- | Names accepted by both runner integrations.
settingNames :: [String]
settingNames = ["test-cases", "stateful-steps", "seed", "database", "replay", "replay-key"]

-- | Parse one source, requiring replay identity and token together.
parseOverrides :: (String -> Maybe String) -> Either String Overrides
parseOverrides get = do
  cases <- optional "test-cases" (settingInteger "test-cases" (\n -> Settings.defaultSettings {Settings.testCases = n}))
  steps <- optional "stateful-steps" (settingInteger "stateful-steps" (\n -> Settings.defaultSettings {statefulStepCount = n}))
  seed <- optional "seed" (natural "seed" 0)
  database <- optional "database" parseDatabase
  replay <- case (get "replay", get "replay-key") of
    (Nothing, Nothing) -> Right Nothing
    (Just token, Just key) | not (null key) ->
      case decodeReplayToken (T.pack token) of
        Left err -> Left ("hegel-replay: " <> show err)
        Right decoded -> Right (Just (T.pack key, decoded))
    _ -> Left "hegel-replay and hegel-replay-key: supply a token and nonempty exact identity together in one source"
  pure (Overrides cases steps seed database replay)
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

-- | Parse decimal digits within the supplied lower bound and target type maximum.
natural :: forall a. (Integral a, Bounded a) => String -> Integer -> String -> Either String a
natural name lowerBound input = case readMaybe input :: Maybe Integer of
  Just value
    | not (null input),
      all (\c -> isDigit c && c <= '9') input,
      value >= lowerBound,
      value <= toInteger (maxBound :: a) ->
        Right (fromInteger value)
  _ -> Left ("hegel-" <> name <> ": expected an integer in [" <> show lowerBound <> ", " <> show (toInteger (maxBound :: a)) <> "]")

-- | Parse a disabled, default, or directory-backed store.
parseDatabase :: String -> Either String Database
parseDatabase "off" = Right DatabaseDisabled
parseDatabase "default" = Right DatabaseDefault
parseDatabase value
  | Just path <- T.stripPrefix "directory:" (T.pack value), not (T.null path) = Right (DatabaseDirectory (T.unpack path))
  | otherwise = Left "hegel-database: expected off, default, or directory:PATH with a nonempty path"

-- | Read shared environment overrides at example execution time.
readOverrides :: IO (Either String Overrides)
readOverrides = do
  values <- traverse (lookupEnv . environmentName) settingNames
  pure (parseOverrides (\name -> lookup name (zip settingNames values) >>= id))
  where
    environmentName :: String -> String
    environmentName name = "HEGEL_" <> fmap convert name
    convert '-' = '_'
    convert c = toEnum (fromEnum c - fromEnum 'a' + fromEnum 'A')

-- | Higher-priority values replace supplied lower-priority values.
overlay :: Overrides -> Overrides -> Overrides
overlay low high =
  Overrides
    (high.testCases <|> low.testCases)
    (high.statefulSteps <|> low.statefulSteps)
    (high.seed <|> low.seed)
    (high.database <|> low.database)
    (high.replay <|> low.replay)

-- | Resolve settings and reject persistence without an identity.
resolve :: (HasCallStack) => Settings -> Overrides -> Either String Settings
resolve settings overrides =
  let resolved =
        settings
          { testCases = fromMaybe settings.testCases overrides.testCases,
            statefulStepCount = fromMaybe settings.statefulStepCount overrides.statefulSteps,
            seed = overrides.seed <|> settings.seed,
            database = fromMaybe settings.database overrides.database
          }
   in either (Left . displayException) Right (withFrozenCallStack (Settings.validate resolved)) >> case resolved.database of
        DatabaseDisabled -> Right resolved
        _
          | Just key <- resolved.databaseKey, not (T.null key) -> Right resolved
          | otherwise -> Left "hegel-database: persistence requires a nonempty explicit databaseKey or a named Hspec helper"

-- | Replay the exact matching identity once, or run ordinary exploration.
execute :: (Int -> IO ()) -> Settings -> Overrides -> Property () -> IO Report
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
