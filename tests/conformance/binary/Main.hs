module Main (main) where

import ConformanceUtils (aesonOpts, decodeArgs, runConformanceProperty, writeMetrics)
import Data.Aeson (FromJSON (..), genericParseJSON, object, (.=))
import Data.ByteString qualified as BS
import Data.Function ((&))
import GHC.Generics (Generic)
import Hegel.Gen qualified as Gen

data Params = Params {minSize :: Int, maxSize :: Maybe Int}
  deriving stock (Generic)

instance FromJSON Params where
  parseJSON = genericParseJSON aesonOpts

main :: IO ()
main = do
  params <- decodeArgs @Params
  let g =
        Gen.binary
          & Gen.minSize params.minSize
          & maybe id Gen.maxSize params.maxSize
          & Gen.build
  runConformanceProperty g \bs -> writeMetrics (object ["length" .= BS.length bs])
