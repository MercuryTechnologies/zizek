module Main (main) where

import Common (runConformanceProperty, writeMetrics)
import Data.Aeson (ToJSON (..))
import Data.Function ((&))
import GHC.Generics (Generic)
import Hegel.Gen qualified as Gen
import System.Environment (getArgs)
import System.Exit (die)

newtype Metrics = Metrics {value :: Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [_] -> pure ()
    _ -> die "Usage: test-booleans '<json_params>'"
  runConformanceProperty (Gen.bool & Gen.build) (writeMetrics . Metrics)
