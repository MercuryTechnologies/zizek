module Main (main) where

import Common (runConformanceProperty, writeMetrics)
import Data.Aeson (ToJSON (..))
import GHC.Generics (Generic)
import Hegel.Generators.Bool qualified as Bool
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
  runConformanceProperty Bool.gen (writeMetrics . Metrics)
