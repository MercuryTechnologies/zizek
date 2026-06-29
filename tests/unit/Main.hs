module Main (main) where

import BasicProperties qualified
import DatabaseReplay qualified
import GeneratorSchemas qualified
import Integrations qualified
import KeyedProperties qualified
import PropertyChecks qualified
import ReportRendering qualified
import SourceRendering qualified
import StandardGenerators qualified
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)

main :: IO ()
main = do
  rendering <- testSpec "report rendering" ReportRendering.spec
  sourceRendering <- testSpec "source rendering" SourceRendering.spec
  integrations <- testSpec "framework integrations" Integrations.spec
  basics <- testSpec "basic properties" BasicProperties.spec
  schemas <- testSpec "generator schemas" GeneratorSchemas.spec
  standards <- testSpec "standard generators" StandardGenerators.spec
  properties <- testSpec "property monad" PropertyChecks.spec
  replay <- testSpec "database replay" DatabaseReplay.spec
  keyed <- testSpec "keyed properties" KeyedProperties.spec
  defaultMain
    ( testGroup
        "zizek:unit"
        [ rendering,
          sourceRendering,
          integrations,
          Integrations.tastyTree,
          basics,
          schemas,
          standards,
          properties,
          replay,
          keyed,
          KeyedProperties.tastyTree
        ]
    )
