module Main (main) where

import Common (camelToSnake, runConformanceProperty, writeMetrics)
import Data.Aeson (FromJSON (..), Options (..), defaultOptions, eitherDecodeStrict', genericParseJSON, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import GHC.Generics (Generic)
import Hegel.Gen qualified as Gen
import Hegel.Gen.Binary (BinaryOptions (..))
import System.Environment (getArgs)
import System.Exit (die)

data Params = Params {minSize :: Int, maxSize :: Maybe Int}
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
    _ -> die "Usage: test-binary '<json_params>'"
  params <- either die pure (eitherDecodeStrict' @Params (BS8.pack j))
  let g = Gen.binaryWith BinaryOptions {minSize = params.minSize, maxSize = params.maxSize}
  runConformanceProperty g \bs -> writeMetrics (object ["length" .= BS.length bs])
