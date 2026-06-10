module Main (main) where

import BasicProperties qualified
import DatabaseReplay qualified
import GeneratorSchemas qualified
import Integrations qualified
import PropertyChecks qualified
import ReportRendering qualified
import StandardGenerators qualified
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)
import TestBackends (backends)
import TestRunner (Checker, Runner)

main :: IO ()
main = do
  rendering <- testSpec "report rendering" ReportRendering.spec
  integrations <- testSpec "framework integrations" Integrations.spec
  trees <- traverse (\(name, runner, checker) -> buildBackendTree name runner checker) backends
  defaultMain
    (testGroup "zizek:unit" (rendering : integrations : Integrations.tastyTree : trees))

buildBackendTree :: String -> Runner -> Checker -> IO TestTree
buildBackendTree name runner checker = do
  basics <- testSpec "basic properties" (BasicProperties.spec runner)
  schemas <- testSpec "generator schemas" (GeneratorSchemas.spec runner)
  standards <- testSpec "standard generators" (StandardGenerators.spec runner)
  properties <- testSpec "property monad" (PropertyChecks.spec checker)
  replay <- testSpec "database replay" (DatabaseReplay.spec name checker)
  pure $ testGroup name [basics, schemas, standards, properties, replay]
