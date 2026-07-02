module Main (main) where

import BasicProperties qualified
import ControlSignals qualified
import DatabaseReplay qualified
import GeneratorSchemas qualified
import Integrations qualified
import KeyedProperties qualified
import PoolEvents qualified
import PropertyChecks qualified
import ReportRendering qualified
import SourceRendering qualified
import SpineRendering qualified
import StandardGenerators qualified
import Stateful qualified
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)
import TraceModel qualified

main :: IO ()
main = do
  controlSignals <- testSpec "control signals" ControlSignals.spec
  rendering <- testSpec "report rendering" ReportRendering.spec
  sourceRendering <- testSpec "source rendering" SourceRendering.spec
  integrations <- testSpec "framework integrations" Integrations.spec
  basics <- testSpec "basic properties" BasicProperties.spec
  schemas <- testSpec "generator schemas" GeneratorSchemas.spec
  standards <- testSpec "standard generators" StandardGenerators.spec
  properties <- testSpec "property monad" PropertyChecks.spec
  replay <- testSpec "database replay" DatabaseReplay.spec
  keyed <- testSpec "keyed properties" KeyedProperties.spec
  stateful <- testSpec "stateful testing" Stateful.spec
  poolEvents <- testSpec "pool events" PoolEvents.spec
  traceModel <- testSpec "trace model" TraceModel.spec
  ledger <- testSpec "spine rendering" SpineRendering.spec
  defaultMain
    ( testGroup
        "zizek:unit"
        [ controlSignals,
          rendering,
          sourceRendering,
          integrations,
          Integrations.tastyTree,
          basics,
          schemas,
          standards,
          properties,
          replay,
          keyed,
          KeyedProperties.tastyTree,
          stateful,
          poolEvents,
          traceModel,
          ledger
        ]
    )
