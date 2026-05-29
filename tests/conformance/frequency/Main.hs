module Main (main) where

import ConformanceUtils (aesonOpts, decodeArgs, runConformancePropertyPaired)
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Function ((&))
import Data.Int (Int64)
import GHC.Generics (Generic)
import Hegel (Gen)
import Hegel.Gen qualified as Gen
import System.Exit (die)

data Branch = Branch {weight :: Int, minValue :: Int64, maxValue :: Int64}
  deriving stock (Generic)

newtype Params = Params {branches :: [Branch]}
  deriving stock (Generic)

data Metrics = Metrics {value :: Int64, branch :: Int}
  deriving stock (Generic)

instance FromJSON Branch where
  parseJSON = genericParseJSON aesonOpts

instance FromJSON Params where
  parseJSON = genericParseJSON aesonOpts

instance ToJSON Metrics where
  toJSON = genericToJSON aesonOpts

branchGen :: Int -> Branch -> Gen Metrics
branchGen idx b = do
  v <- Gen.int64 & Gen.min b.minValue & Gen.max b.maxValue & Gen.build
  pure (Metrics {value = v, branch = idx})

main :: IO ()
main = do
  params <- decodeArgs @Params
  case params.branches of
    [] -> die "test-frequency: branches must be non-empty"
    _ -> pure ()
  let pairs = zipWith (\i b -> (b.weight, branchGen i b)) [0 ..] params.branches
      g = Gen.frequency pairs
  runConformancePropertyPaired g Just
