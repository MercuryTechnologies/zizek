module Main (main) where

import CBOR.Value (Value)
import ConformanceUtils (decodeArgs, runConformancePropertyPaired)
import Data.Aeson (ToJSON (..), withObject, (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Hegel.Gen.Internal (basic)
import Hegel.Internal.Foreign.CBOR (ParseError, hegelText)
import Hegel.Internal.Foreign.Schema (CharacterFields (..), TextSchema (..))

data Params = Params
  { pMinSize :: Int,
    pMaxSize :: Maybe Int,
    pCodec :: Maybe Text,
    pMinCodepoint :: Maybe Int,
    pMaxCodepoint :: Maybe Int,
    pCategories :: Maybe [Text],
    pExcludeCategories :: Maybe [Text],
    pIncludeCharacters :: Maybe Text,
    pExcludeCharacters :: Maybe Text
  }

instance Aeson.FromJSON Params where
  parseJSON = withObject "Params" \o ->
    Params
      <$> o .: "min_size"
      <*> o .:? "max_size"
      <*> o .:? "codec"
      <*> o .:? "min_codepoint"
      <*> o .:? "max_codepoint"
      <*> o .:? "categories"
      <*> o .:? "exclude_categories"
      <*> o .:? "include_characters"
      <*> o .:? "exclude_characters"

newtype Metrics = Metrics {codepoints :: [Int]}
  deriving stock (Generic)

instance ToJSON Metrics where
  toJSON m = Aeson.object ["codepoints" Aeson..= m.codepoints]

main :: IO ()
main = do
  p <- decodeArgs @Params
  let cf =
        CharacterFields
          { codec = p.pCodec,
            minCodepoint = p.pMinCodepoint,
            maxCodepoint = p.pMaxCodepoint,
            categories = p.pCategories,
            excludeCategories = p.pExcludeCategories,
            includeCharacters = p.pIncludeCharacters,
            excludeCharacters = p.pExcludeCharacters
          }
      gen =
        basic
          TextSchema {minSize = p.pMinSize, maxSize = p.pMaxSize, charFields = cf}
          parseText
  runConformancePropertyPaired gen (Just . toMetrics)
  where
    parseText :: Value -> Either ParseError Text
    parseText = hegelText

    toMetrics :: Text -> Metrics
    toMetrics t = Metrics {codepoints = fromEnum <$> T.unpack t}
