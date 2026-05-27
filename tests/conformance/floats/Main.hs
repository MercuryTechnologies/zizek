module Main (main) where

import ConformanceUtils (aesonOpts, decodeArgs, runConformanceProperty, writeMetrics)
import Data.Aeson (FromJSON (..), genericParseJSON, object, (.=))
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Hegel.Gen qualified as Gen

data Params = Params
  { minValue :: Maybe Double,
    maxValue :: Maybe Double,
    excludeMin :: Bool,
    excludeMax :: Bool,
    allowNan :: Maybe Bool,
    allowInfinity :: Maybe Bool
  }
  deriving stock (Generic)

instance FromJSON Params where
  parseJSON = genericParseJSON aesonOpts

main :: IO ()
main = do
  params <- decodeArgs @Params
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
