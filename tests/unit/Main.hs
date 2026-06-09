module Main (main) where

import BasicProperties qualified
import GeneratorSchemas qualified
import PipelinedRequests qualified
import SessionRecovery qualified
import StandardGenerators qualified
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)
import TestBackends (backends)
import TestRunner (Runner)
import UnsupportedCapabilities qualified

main :: IO ()
main = do
  trees <- traverse (uncurry buildBackendTree) backends
  defaultMain (testGroup "zizek:unit" trees)

buildBackendTree :: String -> Runner -> IO TestTree
buildBackendTree name runner = do
  basics <- testSpec "basic properties" (BasicProperties.spec runner)
  schemas <- testSpec "generator schemas" (GeneratorSchemas.spec runner)
  standards <- testSpec "standard generators" (StandardGenerators.spec runner)

  serverOnly <-
    if name == "server"
      then do
        recovery <- testSpec "session recovery" SessionRecovery.spec
        pipelined <- testSpec "pipelined requests" PipelinedRequests.spec
        unsupported <- testSpec "unsupported capabilities" UnsupportedCapabilities.spec
        pure [recovery, pipelined, unsupported]
      else pure []

  pure $ testGroup name ([basics, schemas, standards] <> serverOnly)
