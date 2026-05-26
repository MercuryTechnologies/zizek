module Main (main) where

import BasicProperties qualified
import Control.Exception (bracket_)
import GeneratorSchemas qualified
import Hegel (closeSession, globalSession)
import SessionRecovery qualified
import StandardGenerators qualified
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)

main :: IO ()
main = do
  basics <- testSpec "basic properties" BasicProperties.spec
  schemas <- testSpec "generator schemas" GeneratorSchemas.spec
  standards <- testSpec "standard generators" StandardGenerators.spec
  recovery <- testSpec "session recovery" SessionRecovery.spec
  let tree = testGroup "zizek:unit" [basics, schemas, standards, recovery]
  bracket_ (pure ()) (closeSession globalSession) (defaultMain tree)
