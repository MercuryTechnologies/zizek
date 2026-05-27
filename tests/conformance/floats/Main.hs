module Main (main) where

import Common (camelToSnake, runConformanceProperty, writeMetrics)
import Data.Aeson (FromJSON (..), Options (..), defaultOptions, eitherDecodeStrict', genericParseJSON, object, (.=))
import Data.ByteString.Char8 qualified as BS
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Hegel.Gen qualified as Gen
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
        Gen.double
          & maybe id Gen.min params.minValue
          & maybe id Gen.max params.maxValue
          & (if params.excludeMin then Gen.exclusiveMin else id)
          & (if params.excludeMax then Gen.exclusiveMax else id)
          & (if fromMaybe True params.allowNan then id else Gen.disallowNan)
          & (if fromMaybe True params.allowInfinity then id else Gen.disallowInfinity)
          & Gen.build
  runConformanceProperty g \v ->
    writeMetrics $
      object
        [ "value" .= (if isNaN v || isInfinite v then 0.0 else v :: Double),
          "is_nan" .= isNaN v,
          "is_infinite" .= isInfinite v
        ]
