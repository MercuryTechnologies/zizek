-- | Simplified stubs and fakes driven by 'Hegel.Supply', to judge whether
-- drawing generated values from plain 'IO' is viable.
--
-- Set @SUPPLY_SPIKE_SHOW=1@ to print each demo's rendered failure report.
module Main (main) where

import Clock qualified
import Faults qualified
import Http qualified
import System.Environment (setEnv)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)

main :: IO ()
main = do
  setEnv "HEGEL_DATABASE" "disabled"
  http <- testSpec "HTTP handler" Http.spec
  clock <- testSpec "fake clock" Clock.spec
  faults <- testSpec "injected faults" Faults.spec
  defaultMain (testGroup "zizek:supply-spike" [http, clock, faults])
