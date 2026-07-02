-- | Profiling scenarios: deterministic, named workloads that isolate the
-- Haskell-side hot paths. See @notes/04-profiling-harness.md@ for the full
-- scenario table and the guide to interpreting the captured profiles.
--
-- Run via @just profile-run \<scenario\>@ (smoke test on the dev build),
-- @just profile-space \<scenario\>@ (.prof\/heap\/eventlog capture on the
-- profiling build), or @just profile-time@ (hyperfine wall-clock comparison
-- on the release build).
--
-- Every scenario is a 'check' with a fixed seed and a fixed test-case count,
-- so consecutive runs do identical work (hyperfine-comparable). Reports are
-- summarized in one line and never rendered — rendering would pollute the
-- profile of a failing scenario. A completed run always exits 0; this is a
-- harness, not a test. Only usage errors exit nonzero.
module Main (main) where

import Control.Exception (displayException)
import Control.Monad (replicateM_, void)
import Data.Function ((&))
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Word (Word64)
import Hegel (Gen)
import Hegel.Assertion (assert)
import Hegel.Gen qualified as Gen
import Hegel.HealthCheck (HealthCheck (..))
import Hegel.Phase (Phase (..))
import Hegel.Property (Property, forAll, forAllSilent)
import Hegel.Report (Abort (..), Report (..), Result (..), Stats (..))
import Hegel.Runner (check)
import Hegel.Settings (Settings (..), defaultSettings)
import Hegel.Stateful qualified as Stateful
import Stress qualified
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)
import Warehouse qualified

main :: IO ()
main =
  getArgs >>= \case
    ["--list"] -> mapM_ (putStrLn . describeScenario) scenarios
    name : rest
      | Just scenario <- List.find ((== name) . (.name)) scenarios ->
          either usageError (runScenario scenario) (parseOpts rest)
      | otherwise -> usageError ("unknown scenario: " <> name)
    [] -> usageError "expected a scenario name"

-- * CLI

data Opts = Opts
  { cases :: Maybe Int,
    seed :: Word64,
    shrink :: Bool
  }

-- | Fixed by default so consecutive runs (and hyperfine iterations) do
-- identical work; override with @--seed@ to sample a different trajectory.
defaultSeed :: Word64
defaultSeed = 2026

parseOpts :: [String] -> Either String Opts
parseOpts = go Opts {cases = Nothing, seed = defaultSeed, shrink = True}
  where
    go opts = \case
      [] -> Right opts
      "--no-shrink" : rest -> go opts {shrink = False} rest
      -- Full construction, not a record update: `seed` also lives on
      -- 'Settings', and DuplicateRecordFields updates are ambiguous.
      "--seed" : n : rest
        | Just s <- readMaybe n ->
            go Opts {cases = opts.cases, seed = s, shrink = opts.shrink} rest
      arg : rest
        | Just n <- readMaybe arg -> go opts {cases = Just n} rest
      arg : _ -> Left ("unrecognized argument: " <> arg)

usageError :: String -> IO a
usageError err = do
  prog <- getProgName
  hPutStrLn stderr ("error: " <> err)
  hPutStrLn stderr ""
  hPutStrLn stderr ("usage: " <> prog <> " <scenario> [cases] [--no-shrink] [--seed N]")
  hPutStrLn stderr ("       " <> prog <> " --list")
  hPutStrLn stderr ""
  hPutStrLn stderr "scenarios:"
  mapM_ (hPutStrLn stderr . ("  " <>) . describeScenario) scenarios
  exitFailure

-- * Driving

runScenario :: Scenario -> Opts -> IO ()
runScenario scenario opts = do
  let settings =
        defaultSettings
          { testCases = fromMaybe scenario.defaultCases opts.cases,
            seed = Just opts.seed,
            phases =
              if opts.shrink
                then defaultSettings.phases
                else List.filter (/= Shrink) defaultSettings.phases,
            -- Profiling workloads are deliberately extreme; the health
            -- checks would reject exactly the pathological cases (e.g.
            -- gen-hoard's 10k draws per case) we are here to measure.
            suppressHealthCheck =
              [FilterTooMuch, TooSlow, TestCasesTooLarge, LargeInitialTestCase]
          }
  report <- check settings scenario.property
  T.putStrLn (summary scenario settings report)

summary :: Scenario -> Settings -> Report -> Text
summary scenario settings report =
  T.unwords
    [ T.pack scenario.name <> ":",
      "cases=" <> tshow settings.testCases,
      "valid=" <> tshow report.stats.valid,
      "invalid=" <> tshow report.stats.invalid,
      "result=" <> resultTag report.result
    ]

-- | Constructor name, plus the reason for runs that stopped early — an
-- opaque @Aborted@ would otherwise hide exactly the misconfiguration (e.g. a
-- tripped health check) a new scenario needs to hear about.
resultTag :: Result -> Text
resultTag = \case
  Ok -> "Ok"
  Counterexample {} -> "Counterexample"
  GaveUp why -> "GaveUp (" <> why <> ")"
  Aborted (UnhealthyInput why) -> "Aborted (UnhealthyInput: " <> why <> ")"
  Aborted (Errored e) -> "Aborted (Errored: " <> T.pack (displayException e) <> ")"

tshow :: (Show a) => a -> Text
tshow = T.pack . show

-- * Scenarios

data Scenario = Scenario
  { name :: String,
    defaultCases :: Int,
    blurb :: String,
    property :: Property ()
  }

describeScenario :: Scenario -> String
describeScenario s =
  s.name
    <> List.replicate (Prelude.max 1 (17 - List.length s.name)) ' '
    <> s.blurb
    <> " (default cases: "
    <> show s.defaultCases
    <> ")"

scenarios :: [Scenario]
scenarios =
  [ Scenario "overhead" 10000 "one full-range int draw per case; per-case round-trip floor" overheadProperty,
    Scenario "scalars" 1000 "100 small int draws per case; per-draw round-trip cost" scalarsProperty,
    Scenario "collections" 500 "one list-of-text + one map draw per case; per-byte CBOR cost" collectionsProperty,
    Scenario "stateful-simple" 2000 "passing one-rule counter machine; per-step overhead" (Stateful.run counterMachine),
    Scenario "warehouse" 1000 "passing warehouse machine; realistic mixed stateful workload" (Stateful.run (Warehouse.machine Warehouse.Fixed)),
    Scenario "shrink" 100 "buggy warehouse machine; find + shrink + replay (pair with --no-shrink)" (Stateful.run (Warehouse.machine Warehouse.Buggy)),
    Scenario "warehouse-heavy" 300 "24-SKU warehouse w/ audit-log thunk chains + fat annotations" (Stateful.run Stress.heavyMachine),
    Scenario "gen-churn" 2000 "fresh dependent generator per draw; pre-encoding worst case" Stress.churnProperty,
    Scenario "gen-hoard" 20 "10k generators alive as a CAF; cached-encoding retention" Stress.hoardProperty
  ]

smallInt :: Gen Int
smallInt = Gen.int & Gen.min 0 & Gen.max 1000 & Gen.build

-- | Per-case floor: 'Hegel.Runner.check''s drive loop, @hegel_next_test_case@,
-- and @markComplete@, over the smallest workload that still varies per case.
-- (A zero-draw body won't do: the engine deduplicates identical choice
-- sequences, so a draw-free property runs exactly one valid case.) The draw
-- is full-range so 10k cases don't exhaust the value space either.
overheadProperty :: Property ()
overheadProperty = void (forAllSilent (Gen.int & Gen.build))

-- | Per-draw round-trip cost with minimal payloads: schema CBOR encode,
-- @hegel_generate@, result decode, and the marshalling copies in
-- 'Hegel.Internal.FFI.generate'. 'forAllSilent' keeps journaling out of the
-- measurement.
scalarsProperty :: Property ()
scalarsProperty = replicateM_ 100 (forAllSilent smallInt)

-- | One CBOR-heavy composite draw per case: per-byte encode\/decode cost and
-- the 'Hegel.Collection' span machinery, in contrast to the per-call cost
-- 'scalarsProperty' isolates.
collectionsProperty :: Property ()
collectionsProperty = do
  void (forAllSilent (Gen.list shortText & Gen.minSize 10 & Gen.maxSize 50 & Gen.build))
  void (forAllSilent (Gen.map smallInt shortText & Gen.minSize 5 & Gen.maxSize 25 & Gen.build))
  where
    shortText = Gen.text & Gen.maxSize 100 & Gen.build

-- | One rule drawing one small int plus one always-true invariant: the
-- profile isolates the per-step machinery (@stateMachineNextRule@, step
-- journaling, invariant dispatch) rather than user work.
counterMachine :: Stateful.Machine Int IO
counterMachine =
  Stateful.Machine
    { initial = pure 0,
      rules =
        [ Stateful.Rule "add" \n -> do
            d <- forAll smallInt
            pure (n + d)
        ],
      invariants =
        [ Stateful.Invariant "non_negative" \n ->
            assert (0 <= n) "counter never goes negative"
        ]
    }
