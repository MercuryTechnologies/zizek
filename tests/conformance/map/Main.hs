module Main (main) where

import ConformanceUtils (decodeArgs, nonBasic, runConformancePropertyExpectFailures, runConformancePropertyPaired)
import Data.Aeson (ToJSON (..), withObject, (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Function ((&))
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Gen qualified as Gen
import System.Environment (lookupEnv)
import Prelude hiding (map)

data Params = Params
  { minSize :: !Int,
    maxSize :: !Int,
    keyType :: !Text,
    minKey :: !(Maybe Int64),
    maxKey :: !(Maybe Int64),
    minValue :: !(Maybe Int64),
    maxValue :: !(Maybe Int64),
    mode :: !(Maybe Text)
  }

instance Aeson.FromJSON Params where
  parseJSON = withObject "Params" \o ->
    Params
      <$> (fromMaybe 0 <$> o .:? "min_size")
      <*> o .: "max_size"
      <*> (fromMaybe "integer" <$> o .:? "key_type")
      <*> o .:? "min_key"
      <*> o .:? "max_key"
      <*> o .:? "min_value"
      <*> o .:? "max_value"
      <*> o .:? "mode"

data Metrics = Metrics
  { size :: !Int,
    minValue :: !(Maybe Int64),
    maxValue :: !(Maybe Int64),
    minKey :: !(Maybe Int64),
    maxKey :: !(Maybe Int64)
  }

instance ToJSON Metrics where
  toJSON m =
    Aeson.object
      [ "size" Aeson..= m.size,
        "min_value" Aeson..= m.minValue,
        "max_value" Aeson..= m.maxValue,
        "min_key" Aeson..= m.minKey,
        "max_key" Aeson..= m.maxKey
      ]

main :: IO ()
main = do
  p <- decodeArgs @Params
  let modeStr = maybe "non_basic" T.unpack p.mode
      valGen =
        Gen.int64
          & maybe id Gen.min p.minValue
          & maybe id Gen.max p.maxValue
          & Gen.build
  testMode <- lookupEnv "HEGEL_PROTOCOL_TEST_MODE"
  case p.keyType of
    "string" -> do
      let keyGen = nonBasic modeStr (Gen.text & Gen.build)
          g =
            Gen.map keyGen (nonBasic modeStr valGen)
              & Gen.minSize p.minSize
              & Gen.maxSize p.maxSize
              & Gen.build
      case testMode of
        Just _ -> runConformancePropertyExpectFailures g (\_ -> pure ())
        Nothing -> runConformancePropertyPaired g (Just . textMetrics)
    _ -> do
      let keyGen =
            Gen.int64
              & maybe id Gen.min p.minKey
              & maybe id Gen.max p.maxKey
              & Gen.build
          g =
            Gen.map (nonBasic modeStr keyGen) (nonBasic modeStr valGen)
              & Gen.minSize p.minSize
              & Gen.maxSize p.maxSize
              & Gen.build
      case testMode of
        Just _ -> runConformancePropertyExpectFailures g (\_ -> pure ())
        Nothing -> runConformancePropertyPaired g (Just . intMetrics)

intMetrics :: Map Int64 Int64 -> Metrics
intMetrics m
  | Map.null m = Metrics {size = 0, minValue = Nothing, maxValue = Nothing, minKey = Nothing, maxKey = Nothing}
  | otherwise =
      Metrics
        { size = Map.size m,
          minValue = Just (minimum (Map.elems m)),
          maxValue = Just (maximum (Map.elems m)),
          minKey = Just (minimum (Map.keys m)),
          maxKey = Just (maximum (Map.keys m))
        }

textMetrics :: Map Text Int64 -> Metrics
textMetrics m
  | Map.null m = Metrics {size = 0, minValue = Nothing, maxValue = Nothing, minKey = Nothing, maxKey = Nothing}
  | otherwise =
      Metrics
        { size = Map.size m,
          minValue = Just (minimum (Map.elems m)),
          maxValue = Just (maximum (Map.elems m)),
          minKey = Nothing,
          maxKey = Nothing
        }
