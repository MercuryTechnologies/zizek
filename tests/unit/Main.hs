module Main (main) where

import Alphabet qualified
import BasicProperties qualified
import BranchProperties qualified
import ConcurrentStateful qualified
import ControlSignals qualified
import DatabaseReplay qualified
import DiffRendering qualified
import Finalizers qualified
import ForkProperties qualified
import GenValidation qualified
import GrammarGenerators qualified
import Integrations qualified
import KeyedProperties qualified
import LogRendering qualified
import PoolEvents qualified
import PoolFilters qualified
import PropertyChecks qualified
import RecursiveGenerators qualified
import ReportRendering qualified
import Resources qualified
import RunnerConfiguration qualified
import Sampling qualified
import SequenceGenerators qualified
import SourceRendering qualified
import StandardGenerators qualified
import Stateful qualified
import StatefulRoundInternal qualified
import System.Environment (setEnv)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)
import TestCaseClone qualified
import TickRecording qualified
import TraceModel qualified

main :: IO ()
main = do
  -- Tests that persist failures override the database with a temporary
  -- directory, so every other keyed run stays out of the working directory.
  setEnv "HEGEL_DATABASE" "disabled"
  alphabet <- testSpec "Hegel.Alphabet" Alphabet.spec
  controlSignals <- testSpec "control signals" ControlSignals.spec
  rendering <- testSpec "report rendering" ReportRendering.spec
  sourceRendering <- testSpec "source rendering" SourceRendering.spec
  integrations <- testSpec "framework integrations" Integrations.spec
  basics <- testSpec "basic properties" BasicProperties.spec
  standards <- testSpec "standard generators" StandardGenerators.spec
  genValidation <- testSpec "generator validation" GenValidation.spec
  grammarGenerators <- testSpec "grammar generators" GrammarGenerators.spec
  properties <- testSpec "property monad" PropertyChecks.spec
  replay <- testSpec "database replay" DatabaseReplay.spec
  keyed <- testSpec "keyed properties" KeyedProperties.spec
  stateful <- testSpec "stateful testing" Stateful.spec
  statefulRoundInternal <- testSpec "stateful round (internal)" StatefulRoundInternal.spec
  concurrentStateful <- testSpec "concurrent stateful testing" ConcurrentStateful.spec
  finalizers <- testSpec "finalizers" Finalizers.spec
  resources <- testSpec "resources" Resources.spec
  poolEvents <- testSpec "pool events" PoolEvents.spec
  poolFilters <- testSpec "pool filters" PoolFilters.spec
  traceModel <- testSpec "trace model" TraceModel.spec
  ledger <- testSpec "event-log rendering" LogRendering.spec
  testCaseClone <- testSpec "TestCase cloning" TestCaseClone.spec
  tickRecording <- testSpec "tick recording" TickRecording.spec
  branchProperties <- testSpec "branch combinators" BranchProperties.spec
  forkProperties <- testSpec "fork combinators" ForkProperties.spec
  sampling <- testSpec "sampling" Sampling.spec
  recursiveGenerators <- testSpec "recursive generators" RecursiveGenerators.spec
  sequenceGenerators <- testSpec "sequence generators" SequenceGenerators.spec
  diffRendering <- testSpec "diff rendering" DiffRendering.spec
  runnerConfiguration <- testSpec "runner configuration" RunnerConfiguration.spec
  defaultMain
    ( testGroup
        "zizek:unit"
        [ runnerConfiguration,
          alphabet,
          controlSignals,
          rendering,
          sourceRendering,
          integrations,
          Integrations.tastyTree,
          basics,
          standards,
          genValidation,
          grammarGenerators,
          properties,
          replay,
          keyed,
          KeyedProperties.tastyTree,
          stateful,
          statefulRoundInternal,
          concurrentStateful,
          finalizers,
          resources,
          poolEvents,
          poolFilters,
          traceModel,
          ledger,
          testCaseClone,
          tickRecording,
          branchProperties,
          forkProperties,
          sampling,
          recursiveGenerators,
          sequenceGenerators,
          diffRendering
        ]
    )
