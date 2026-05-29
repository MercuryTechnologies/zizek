module Main (main) where

import ConformanceUtils (aesonOpts, decodeArgs, runConformancePropertyPaired)
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Hegel (Gen)
import Hegel.Gen qualified as Gen
import System.Exit (die)

data Range = Range {minValue :: Int, maxValue :: Int}
  deriving stock (Generic)

data Params = Params {ranges :: [Range], mode :: Text}
  deriving stock (Generic)

newtype Metrics = Metrics {value :: Int}
  deriving stock (Generic)

instance FromJSON Range where
  parseJSON = genericParseJSON aesonOpts

instance FromJSON Params where
  parseJSON = genericParseJSON aesonOpts

instance ToJSON Metrics where
  toJSON = genericToJSON aesonOpts

branch :: Range -> Gen Int
branch r = Gen.int & Gen.min r.minValue & Gen.max r.maxValue & Gen.build

main :: IO ()
main = do
  params <- decodeArgs @Params
  case params.ranges of
    [] -> die "test-one-of: ranges must be non-empty"
    _ -> pure ()
  transform <- case params.mode of
    "basic" -> pure id
    "map_negate" -> pure (fmap negate)
    "filter_even" -> pure (Gen.filtered even)
    other -> die ("test-one-of: unknown mode: " <> T.unpack other)
  let g = Gen.oneOf (fmap (transform . branch) params.ranges)
  runConformancePropertyPaired g (Just . Metrics)
