module Main (main) where

import ConformanceUtils (decodeArgs, nonBasic, runConformancePropertyExpectFailures, runConformancePropertyPaired)
import Data.Aeson (ToJSON (..), withObject, (.:?))
import Data.Aeson qualified as Aeson
import Data.Function ((&))
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Hegel.Gen qualified as Gen
import System.Environment (lookupEnv)

data Params = Params
  { minSize :: !Int,
    maxSize :: !(Maybe Int),
    unique :: !Bool,
    minValue :: !(Maybe Int64),
    maxValue :: !(Maybe Int64),
    mode :: !(Maybe Text)
  }

instance Aeson.FromJSON Params where
  parseJSON = withObject "Params" \o ->
    Params
      <$> (fromMaybe 0 <$> o .:? "min_size")
      <*> o .:? "max_size"
      <*> (fromMaybe False <$> o .:? "unique")
      <*> o .:? "min_value"
      <*> o .:? "max_value"
      <*> o .:? "mode"

newtype Metrics = Metrics {elements :: [Int64]}
  deriving stock (Generic)

instance ToJSON Metrics where
  toJSON m = Aeson.object ["elements" Aeson..= m.elements]

main :: IO ()
main = do
  p <- decodeArgs @Params
  let elemGen =
        Gen.int64
          & maybe id Gen.min p.minValue
          & maybe id Gen.max p.maxValue
          & Gen.build
      g =
        Gen.list (nonBasic (maybe "non_basic" T.unpack p.mode) elemGen)
          & Gen.minSize p.minSize
          & maybe id Gen.maxSize p.maxSize
          & (if p.unique then Gen.unique (==) else id)
          & Gen.build
  testMode <- lookupEnv "HEGEL_PROTOCOL_TEST_MODE"
  case testMode of
    Just _ -> runConformancePropertyExpectFailures g (\_ -> pure ())
    Nothing -> runConformancePropertyPaired g (Just . Metrics)
