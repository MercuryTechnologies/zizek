-- | A guided tour of the failure report's progression: how the same wired
-- renderer ('renderReportRichAnsi') grows the report as the property gains
-- structure. Six stages:
--
--   1. a plain property — drawn values spliced into source, structural diff
--   2. a plain property using a pool — the report deliberately does /not/
--      change (events are recorded, but a lifeline needs steps to have a
--      story); the stage prints the recorded event count to prove it
--   3. a machine without pools — the step spine, draws nested under their
--      steps, the failing invariant spliced
--   4. a machine with one pool — the composed report appears: the verdict
--      paragraph and the citation ledger around the failing value
--   5. a machine with two pools — the ledger elides what the failure never
--      touches: an elision row and the elided-lifelines footer
--   6. a pipeline transferring jobs across three pools — the works: one
--      lifeline across pending→running→done ('Pool.transfer' lineage), a
--      full three-edge rail, elision, footer, a verdict quoting the rule's
--      'Stateful.respond', and the stored-example reproduction line
--
-- Run with @just tour@ from the repo root (source splicing
-- resolves @srcLocFile@ relative to the working directory).
--
-- Every stage's bug is shaped so its /minimal/ counterexample retains the
-- demonstrated feature — shrinking deletes anything that isn't load-bearing.
--
-- Always exits 0; this is an eyeballing harness, not an assertion.
module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Hegel.Assertion (assert)
import Hegel.Database (Database (..))
import Hegel.Gen qualified as Gen
import Hegel.Pool (Pool)
import Hegel.Pool qualified as Pool
import Hegel.Property (Property, forAll, (===))
import Hegel.Property.Internal (Env (..), askEnv)
import Hegel.Report (Report (..), Result (..), renderReportRichAnsi)
import Hegel.Runner (check)
import Hegel.Settings (Settings (..), defaultSettings)
import Hegel.Stateful qualified as Stateful
import UnliftIO.IORef (IORef, modifyIORef', newIORef, readIORef)
import UnliftIO.Temporary (withSystemTempDirectory)

main :: IO ()
main = do
  stage
    "1: a plain property — the rich splice"
    ["Drawn values splice into their source; the (===) failure carries a diff."]
    =<< check defaultSettings plainProperty

  report2 <- check defaultSettings pooledProperty
  stage
    "2: a plain property using a pool — the report doesn't change"
    [ "Pool events are recorded, but a lifeline needs steps to have a story:",
      "everything here happens in one breath, so there is nothing to cite.",
      eventCount report2
    ]
    report2

  stage
    "3: a machine without pools — the step spine"
    ["Steps give the run a timeline; the failing invariant splices in place."]
    =<< check defaultSettings (Stateful.run counterMachine)

  stage
    "4: a machine with one pool — the composed report"
    [ "A pool value drawn across steps has a story; the verdict words it and",
      "the ledger draws it: born at register, touched at the first use."
    ]
    =<< check defaultSettings (Stateful.run registryMachine)

  stage
    "5: a machine with two pools — elision"
    [ "The ledger shows only what the failure cites; the other pool's value",
      "and the step that made it are elided, explicitly."
    ]
    =<< check defaultSettings (Stateful.run twoPoolMachine)

  withSystemTempDirectory "report-tour" \dbDir -> do
    stage
      "6: a pipeline with transfers — the works"
      [ "One job crosses three pools (pending → running → done) as a single",
        "lifeline: Pool.transfer declares the identity link, and the report",
        "words each hop as a transfer — ◌ (death) is reserved for consumption",
        "without a continuation. Full rail, elision, footer, a quoted respond,",
        "and the stored-example line."
      ]
      =<< check
        defaultSettings
          { database = DatabaseDirectory dbDir,
            databaseKey = Just "report-tour/pipeline"
          }
        (Stateful.run pipelineMachine)

-- | Banner, narration, then the report through the wired renderer.
stage :: Text -> [Text] -> Report -> IO ()
stage title narration report = do
  T.putStrLn ("\n━━━━━ stage " <> title <> " ━━━━━")
  mapM_ (T.putStrLn . dim) narration
  T.putStrLn ""
  T.putStrLn =<< renderReportRichAnsi report
  where
    dim :: Text -> Text
    dim t = "\x1b[0;37m" <> t <> "\x1b[0m"

eventCount :: Report -> Text
eventCount report = case report.result of
  Counterexample {events} ->
    "(this counterexample recorded "
      <> T.pack (show (length events))
      <> " pool events — present in the data, absent from the report)"
  _ -> "(no counterexample?)"

-- * Stage 1: a plain property

plainProperty :: Property ()
plainProperty = do
  a <- forAll (Gen.int & Gen.min 0 & Gen.max 9 & Gen.build)
  b <- forAll (Gen.int & Gen.min 0 & Gen.max 9 & Gen.build)
  -- A deliberately false claim: subtraction commutes.
  a - b === b - a

-- * Stage 2: a plain property using a pool

pooledProperty :: Property ()
pooledProperty = do
  env <- askEnv
  keys <- liftIO (Pool.named "k" env.testCase)
  n <- forAll (Gen.int & Gen.min 0 & Gen.max 9 & Gen.build)
  liftIO (Pool.add keys n)
  k <- forAll (Pool.valuesReusable keys)
  -- Fails for any nonzero key, so shrinking still has work to do.
  k === 0

-- * Stage 3: a machine without pools

counterMachine :: Stateful.Machine Int IO
counterMachine =
  Stateful.Machine
    { initial = pure 0,
      rules =
        [ Stateful.Rule "increment" \n -> do
            delta <- forAll (Gen.int & Gen.min 1 & Gen.max 3 & Gen.build)
            pure (n + delta)
        ],
      invariants =
        [ Stateful.Invariant "stays_small" \n ->
            assert (n < 5) "counter stays below 5"
        ]
    }

-- * Stage 4: a machine with one pool

data Registry = Registry
  { tokens :: Pool Int,
    used :: Bool
  }

registryMachine :: Stateful.Machine Registry IO
registryMachine =
  Stateful.Machine
    { initial = do
        env <- askEnv
        tokens <- liftIO (Pool.named "k" env.testCase)
        pure Registry {tokens, used = False},
      rules =
        [ Stateful.Rule "register" \m -> do
            n <- forAll (Gen.int & Gen.min 0 & Gen.max 9 & Gen.build)
            liftIO (Pool.add m.tokens n)
            pure m,
          Stateful.Rule "use" \m -> do
            _ <- forAll (Pool.valuesReusable m.tokens)
            Stateful.respond "ok"
            -- BUG: claims single-use tokens, but the pool happily hands the
            -- same token out twice.
            assert (not m.used) "a token is only ever used once"
            pure m {used = True}
        ],
      invariants = []
    }

-- * Stage 5: a machine with two pools

data TwoPools = TwoPools
  { as :: Pool Int,
    bs :: Pool Int,
    hasB :: Bool
  }

twoPoolMachine :: Stateful.Machine TwoPools IO
twoPoolMachine =
  Stateful.Machine
    { initial = do
        env <- askEnv
        as <- liftIO (Pool.named "a" env.testCase)
        bs <- liftIO (Pool.named "b" env.testCase)
        pure TwoPools {as, bs, hasB = False},
      rules =
        [ Stateful.Rule "make_a" \m -> do
            n <- forAll (Gen.int & Gen.min 0 & Gen.max 9 & Gen.build)
            liftIO (Pool.add m.as n)
            pure m,
          Stateful.Rule "make_b" \m -> do
            n <- forAll (Gen.int & Gen.min 0 & Gen.max 9 & Gen.build)
            liftIO (Pool.add m.bs n)
            pure m {hasB = True},
          Stateful.Rule "poke_a" \m -> do
            _ <- forAll (Pool.valuesReusable m.as)
            Stateful.respond "ok"
            -- BUG: pokes are only legal before any b exists.
            assert (not m.hasB) "a-values are only poked before any b exists"
            pure m
        ],
      invariants = []
    }

-- * Stage 6: a pipeline with transfers

data Pipeline = Pipeline
  { pending :: Pool Int,
    running :: Pool Int,
    done :: Pool Int,
    nextJob :: IORef Int,
    -- | SUT: grows on every submit (the \"epoch\").
    submits :: IORef Int,
    -- | SUT: the epoch at each job's start.
    startedAt :: IORef (Map Int Int),
    -- | SUT: each job's stored payload.
    actual :: IORef (Map Int Text),
    -- | Model: the payload each job should still have.
    expected :: Map Int Text
  }

pipelineMachine :: Stateful.Machine Pipeline IO
pipelineMachine =
  Stateful.Machine
    { initial = do
        env <- askEnv
        pending <- liftIO (Pool.named "p" env.testCase)
        running <- liftIO (Pool.named "r" env.testCase)
        done <- liftIO (Pool.named "d" env.testCase)
        nextJob <- newIORef 0
        submits <- newIORef 0
        startedAt <- newIORef Map.empty
        actual <- newIORef Map.empty
        pure Pipeline {pending, running, done, nextJob, submits, startedAt, actual, expected = Map.empty},
      rules =
        [ Stateful.Rule "submit" \m -> do
            payload <- forAll (Gen.text & Gen.minSize 1 & Gen.maxSize 4 & Gen.build)
            j <- liftIO do
              j <- readIORef m.nextJob
              modifyIORef' m.nextJob (+ 1)
              modifyIORef' m.submits (+ 1)
              modifyIORef' m.actual (Map.insert j payload)
              pure j
            liftIO (Pool.add m.pending j)
            Stateful.respond "ok"
            pure m {expected = Map.insert j payload m.expected},
          Stateful.Rule "start" \m -> do
            j <- forAll (Pool.transfer m.pending m.running)
            liftIO do
              e <- readIORef m.submits
              modifyIORef' m.startedAt (Map.insert j e)
            Stateful.respond "ok"
            pure m,
          Stateful.Rule "finish" \m -> do
            j <- forAll (Pool.transfer m.running m.done)
            liftIO do
              e <- readIORef m.submits
              se <- Map.lookup j <$> readIORef m.startedAt
              -- BUG: loses the payload when the job table grew (another
              -- submit) between this job's start and its finish.
              case se of
                Just started | e > started -> modifyIORef' m.actual (Map.insert j "")
                _ -> pure ()
            Stateful.respond "ok"
            pure m,
          Stateful.Rule "verify" \m -> do
            j <- forAll (Pool.valuesReusable m.done)
            a <- liftIO (Map.lookup j <$> readIORef m.actual)
            Stateful.respondShow a
            a === Map.lookup j m.expected
            pure m
        ],
      invariants = []
    }
