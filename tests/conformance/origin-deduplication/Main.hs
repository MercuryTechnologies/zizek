module Main (main) where

import ConformanceUtils (decodeArgs, runConformancePropertyExpectFailures, writeEmptyMetrics)
import Data.Aeson qualified as Aeson
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Assertion qualified as Assertion
import Hegel.Gen qualified as Gen
import System.Exit (die)

-- A single buggy function with a single bug site. Both call paths funnel into
-- it so a correct innermost-frame origin dedups them to one failure.
buggy :: Int -> IO ()
buggy x = Assertion.assert (x <= 10) "x exceeds threshold"

callPathA :: Int -> IO ()
callPathA = buggy

callPathB :: Int -> IO ()
callPathB = buggy

newtype Params = Params {mode :: Text}
  deriving stock (Show)

instance Aeson.FromJSON Params where
  parseJSON = Aeson.withObject "Params" $ \o -> Params <$> o Aeson..: "mode"

main :: IO ()
main = do
  params <- decodeArgs @Params
  let gen = Gen.integer @Int & Gen.min 0 & Gen.max 100 & Gen.build
      body x = do
        -- The harness pairs client metrics 1:1 with server metrics, so we
        -- emit a sentinel line per test case to keep the lengths aligned.
        writeEmptyMetrics
        case params.mode of
          "value_in_error_message" ->
            Assertion.assert
              (x <= 10)
              (T.pack ("Generated value " <> show x <> " exceeded threshold 10"))
          "multiple_call_sites" ->
            if even x then callPathA x else callPathB x
          other -> die ("unknown mode: " <> T.unpack other)
  runConformancePropertyExpectFailures gen body
