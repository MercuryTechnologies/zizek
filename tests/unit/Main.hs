module Main (main) where

import BasicProperties qualified
import GeneratorSchemas qualified
import PipelinedRequests qualified
import PropertyChecks qualified
import ReportRendering qualified
import SessionRecovery qualified
import StandardGenerators qualified
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)
import TestBackends (backends)
import TestRunner (Checker, Runner)
import UnsupportedCapabilities qualified

main :: IO ()
main = do
  rendering <- testSpec "report rendering" ReportRendering.spec
  trees <- traverse (\(name, runner, checker) -> buildBackendTree name runner checker) backends
  defaultMain (testGroup "zizek:unit" (rendering : trees))

buildBackendTree :: String -> Runner -> Checker -> IO TestTree
buildBackendTree name runner checker = do
  basics <- testSpec "basic properties" (BasicProperties.spec runner)
  schemas <- testSpec "generator schemas" (GeneratorSchemas.spec runner)
  standards <- testSpec "standard generators" (StandardGenerators.spec runner)
  properties <- testSpec "property monad" (PropertyChecks.spec checker)

  serverOnly <-
    if name == "server"
      then do
        recovery <- testSpec "session recovery" SessionRecovery.spec
        pipelined <- testSpec "pipelined requests" PipelinedRequests.spec
        unsupported <- testSpec "unsupported capabilities" UnsupportedCapabilities.spec
        pure [recovery, pipelined, unsupported]
      else pure []

  pure $ testGroup name ([basics, schemas, standards, properties] <> serverOnly)
