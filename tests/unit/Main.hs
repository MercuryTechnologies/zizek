module Main (main) where

import BasicProperties qualified
import Control.Exception (bracket_)
import GeneratorSchemas qualified
import Hegel (closeSession)
import SessionRecovery qualified
import Test.Tasty (defaultMain, localOption, testGroup)
import Test.Tasty.Hspec (testSpec)
import Test.Tasty.Runners (NumThreads (..))

main :: IO ()
main = do
  basics <- testSpec "basic properties" BasicProperties.spec
  schemas <- testSpec "generator schemas" GeneratorSchemas.spec
  recovery <- testSpec "session recovery" SessionRecovery.spec
  let tree =
        localOption (NumThreads 1) $
          testGroup "zizek:unit" [basics, schemas, recovery]
  bracket_ (pure ()) closeSession (defaultMain tree)
