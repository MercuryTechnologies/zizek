module Main (main) where

import ConformanceUtils (decodeArgs, runConformancePropertyPaired)
import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Function ((&))
import Data.Text (Text)
import GHC.Generics (Generic)
import Hegel.Gen qualified as Gen

data Params = Params
  { regexPattern :: Text,
    fullmatch :: Bool
  }

instance FromJSON Params where
  parseJSON = withObject "Params" \o ->
    Params
      <$> o .: "pattern"
      <*> (maybe False id <$> o .:? "fullmatch")

newtype Metrics = Metrics {value :: Text}
  deriving stock (Generic)

instance ToJSON Metrics where
  toJSON m = Aeson.object ["value" Aeson..= m.value]

main :: IO ()
main = do
  p <- decodeArgs @Params
  let g =
        Gen.regex p.regexPattern
          & (if p.fullmatch then Gen.fullMatch else id)
          & Gen.build
  runConformancePropertyPaired g (Just . Metrics)
