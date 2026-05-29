module Main (main) where

import ConformanceUtils (aesonOpts, decodeArgs, runConformanceProperty, writeMetrics)
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Int (Int64)
import GHC.Generics (Generic)
import Hegel.Gen qualified as Gen
import System.Exit (die)

newtype Params = Params {options :: [Int64]}
  deriving stock (Generic)

newtype Metrics = Metrics {value :: Int64}
  deriving stock (Generic)

instance FromJSON Params where
  parseJSON = genericParseJSON aesonOpts

instance ToJSON Metrics where
  toJSON = genericToJSON aesonOpts

main :: IO ()
main = do
  params <- decodeArgs @Params
  case params.options of
    [] -> die "test-sampled-from: options must be non-empty"
    _ -> pure ()
  runConformanceProperty (Gen.element params.options) (writeMetrics . Metrics)
