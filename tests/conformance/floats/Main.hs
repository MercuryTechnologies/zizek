module Main (main) where

import Common (camelToSnake, runConformanceProperty, writeMetrics)
import Data.Aeson (FromJSON (..), Options (..), defaultOptions, eitherDecodeStrict', genericParseJSON, object, (.=))
import Data.ByteString.Char8 qualified as BS
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Hegel.Gen qualified as Gen
import Hegel.Gen.Float (FloatOptions (..))
import System.Environment (getArgs)
import System.Exit (die)

data Params = Params
  { minValue :: Maybe Double,
    maxValue :: Maybe Double,
    excludeMin :: Bool,
    excludeMax :: Bool,
    allowNan :: Maybe Bool,
    allowInfinity :: Maybe Bool
  }
  deriving stock (Generic)

aesonOpts :: Options
aesonOpts = defaultOptions {fieldLabelModifier = camelToSnake}

instance FromJSON Params where
  parseJSON = genericParseJSON aesonOpts

main :: IO ()
main = do
  args <- getArgs
  j <- case args of
    [j] -> pure j
    _ -> die "Usage: test-floats '<json_params>'"
  params <- either die pure (eitherDecodeStrict' @Params (BS.pack j))
  let g =
        Gen.doubleWith
          FloatOptions
            { minValue = params.minValue,
              maxValue = params.maxValue,
              excludeMin = params.excludeMin,
              excludeMax = params.excludeMax,
              allowNan = fromMaybe True params.allowNan,
              allowInfinity = fromMaybe True params.allowInfinity
            }
  runConformanceProperty g \v ->
    writeMetrics $
      object
        [ "value" .= (if isNaN v || isInfinite v then 0.0 else v :: Double),
          "is_nan" .= isNaN v,
          "is_infinite" .= isInfinite v
        ]
