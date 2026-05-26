module Main (main) where

import Common (camelToSnake, runConformanceProperty, writeMetrics)
import Data.Aeson (FromJSON (..), Options (..), defaultOptions, eitherDecodeStrict', genericParseJSON, object, (.=))
import Data.ByteString.Char8 qualified as BS
import GHC.Generics (Generic)
import Hegel.Generators.Float qualified as Float
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
        Float.genDouble
          . maybe id Float.withAllowInfinity params.allowInfinity
          . maybe id Float.withAllowNan params.allowNan
          . Float.withExcludeMax params.excludeMax
          . Float.withExcludeMin params.excludeMin
          . maybe id Float.withMaxValue params.maxValue
          . maybe id Float.withMinValue params.minValue
          $ Float.floats
  runConformanceProperty g \v ->
    writeMetrics $
      object
        [ "value" .= (if isNaN v || isInfinite v then 0.0 else v :: Double),
          "is_nan" .= isNaN v,
          "is_infinite" .= isInfinite v
        ]
