module Main (main) where

import ConformanceUtils (aesonOpts, decodeArgs, runConformanceProperty, writeMetrics)
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Int (Int64)
import Data.List.NonEmpty qualified as NE
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
  opts <- case NE.nonEmpty params.options of
    Just xs -> pure xs
    Nothing -> die "test-sampled-from: options must be non-empty"
  runConformanceProperty (Gen.element opts) (writeMetrics . Metrics)
