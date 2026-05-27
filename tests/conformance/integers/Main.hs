module Main (main) where

import Common (camelToSnake, runConformanceProperty, writeMetrics)
import Data.Aeson (FromJSON (..), Options (..), ToJSON (..), defaultOptions, eitherDecodeStrict', genericParseJSON, genericToJSON)
import Data.ByteString.Char8 qualified as BS
import Data.Function ((&))
import Data.Int (Int64)
import GHC.Generics (Generic)
import Hegel.Gen qualified as Gen
import System.Environment (getArgs)
import System.Exit (die)

-- TODO: add a test-integers-narrow binary using Int32 to cover CBOR
-- uint8/16/32/64 tag boundaries (matching the Rust i32 binary).
data Params = Params {minValue :: Maybe Int64, maxValue :: Maybe Int64}
  deriving stock (Generic)

newtype Metrics = Metrics {value :: Int64}
  deriving stock (Generic)

aesonOpts :: Options
aesonOpts = defaultOptions {fieldLabelModifier = camelToSnake}

instance FromJSON Params where
  parseJSON = genericParseJSON aesonOpts

instance ToJSON Metrics where
  toJSON = genericToJSON aesonOpts

main :: IO ()
main = do
  args <- getArgs
  j <- case args of
    [j] -> pure j
    _ -> die "Usage: test-integers '<json_params>'"
  params <- either die pure (eitherDecodeStrict' @Params (BS.pack j))
  let g =
        Gen.integer @Int64
          & maybe id Gen.min params.minValue
          & maybe id Gen.max params.maxValue
          & Gen.build
  runConformanceProperty g (writeMetrics . Metrics)
