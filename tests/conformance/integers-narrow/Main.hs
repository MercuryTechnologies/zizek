module Main (main) where

import ConformanceUtils (aesonOpts, decodeArgs, runConformanceProperty, writeMetrics)
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Function ((&))
import Data.Int (Int32)
import GHC.Generics (Generic)
import Hegel.Gen qualified as Gen

data Params = Params {minValue :: Maybe Int32, maxValue :: Maybe Int32}
  deriving stock (Generic)

newtype Metrics = Metrics {value :: Int32}
  deriving stock (Generic)

instance FromJSON Params where
  parseJSON = genericParseJSON aesonOpts

instance ToJSON Metrics where
  toJSON = genericToJSON aesonOpts

main :: IO ()
main = do
  params <- decodeArgs @Params
  let g =
        Gen.int32
          & maybe id Gen.min params.minValue
          & maybe id Gen.max params.maxValue
          & Gen.build
  runConformanceProperty g (writeMetrics . Metrics)
