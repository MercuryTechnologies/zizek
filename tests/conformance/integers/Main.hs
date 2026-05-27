module Main (main) where

import ConformanceUtils (aesonOpts, decodeArgs, runConformanceProperty, writeMetrics)
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Function ((&))
import Data.Int (Int64)
import GHC.Generics (Generic)
import Hegel.Gen qualified as Gen

-- TODO: add a test-integers-narrow binary using Int32 to cover CBOR
-- uint8/16/32/64 tag boundaries (matching the Rust i32 binary).
data Params = Params {minValue :: Maybe Int64, maxValue :: Maybe Int64}
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
  let g =
        Gen.int64
          & maybe id Gen.min params.minValue
          & maybe id Gen.max params.maxValue
          & Gen.build
  runConformanceProperty g (writeMetrics . Metrics)
