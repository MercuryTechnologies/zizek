module Main (main) where

import ConformanceUtils (decodeArgs, runConformanceProperty, writeMetrics)
import Data.Aeson (ToJSON (..), Value)
import Data.Function ((&))
import GHC.Generics (Generic)
import Hegel.Gen qualified as Gen

newtype Metrics = Metrics {value :: Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON)

main :: IO ()
main = do
  _ <- decodeArgs @Value
  runConformanceProperty (Gen.bool & Gen.build) (writeMetrics . Metrics)
