module Main (main) where

import ConformanceUtils (decodeArgs, runConformancePropertyPaired)
import Data.Aeson (FromJSON (..))
import Data.Aeson qualified as Aeson
import Data.Function ((&))
import GHC.Generics (Generic)
import Hegel.Gen qualified as Gen

data Params = Params {}
  deriving stock (Generic)

instance FromJSON Params where
  parseJSON _ = pure Params {}

main :: IO ()
main = do
  _ <- decodeArgs @Params
  let gen = Gen.char & Gen.build
  runConformancePropertyPaired gen (Just . (Aeson.Number . fromIntegral . fromEnum))
