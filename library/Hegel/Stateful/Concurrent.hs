-- | Concurrent stateful testing: several workers apply rules to one shared
-- model at once, each on its own worker.
--
-- Define a 'Machine' and run it with 'run', choosing how many workers the
-- engine may use via 'fixed', 'upTo', or 'between'.
--
-- Every rule declares which other rules it may run alongside through its
-- 'Rule.group': two rules in the same group may execute concurrently, rules
-- in different groups never do, and an ungrouped rule belongs to a group shared
-- only by other ungrouped rules.
--
-- Build a 'Rule' with 'rule', or 'grouped' to place it in a named concurrency
-- group.
--
-- A rule abandoned mid-'apply' by a rejected assumption or a failed draw can
-- leave a lock it took on the model held forever, so perform every draw a
-- rule needs before taking any lock, rather than interleaving the two.
module Hegel.Stateful.Concurrent
  ( -- * Specification
    Rule (..),
    rule,
    grouped,
    Invariant (..),
    Machine (..),

    -- * Concurrency groups
    anonymousGroup,
    internGroups,

    -- * Concurrency
    Concurrency,
    fixed,
    upTo,
    between,

    -- * Execution
    run,
  )
where

import Control.Exception (mask_)
import Control.Exception qualified as E
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Hegel.Internal.Control (MalformedTest (..))
import Hegel.Internal.DataSource (freeStateMachine, newConcurrentStateMachine, stateMachineNextGroup)
import Hegel.Internal.StatefulRound (RoundVerdict (..), Worker (..), runRound)
import Hegel.Internal.TestCase (withClones)
import Hegel.Property.Internal
  ( Env (..),
    Journal (Silent),
    PropertyT,
    Scope (CaseSetup, InStep),
    askEnv,
    checkCloneDepth,
    closeOpenForks,
    newOpenForks,
    registerFinalizer,
    runPropertyT,
    withBaseRunInIO,
    withScope,
  )
import Hegel.Stateful (Invariant (..))
import UnliftIO (MonadUnliftIO, throwIO, withRunInIO)

-- * Specification

-- | A rule applied to the shared model during a concurrent stateful test.
data Rule s m = Rule
  { name :: !Text,
    group :: !(Maybe Text),
    apply :: s -> PropertyT m ()
  }

-- | Construct an ungrouped 'Rule': one that runs alongside every other
-- ungrouped rule, but never alongside a grouped one.
--
-- Use 'grouped' instead to place a rule in a named concurrency group.
rule :: Text -> (s -> PropertyT m ()) -> Rule s m
rule name apply = Rule {name, group = Nothing, apply}

-- | Construct a 'Rule' in the given concurrency group: it may run
-- concurrently with any other rule sharing that group, and never with a
-- rule in a different one.
grouped :: Text -> Text -> (s -> PropertyT m ()) -> Rule s m
grouped name group apply = Rule {name, group = Just group, apply}

-- | A complete concurrent stateful test specification.
data Machine s m = Machine
  { -- | Construct the model shared by every worker.
    initial :: PropertyT m s,
    rules :: [Rule s m],
    invariants :: [Invariant s m]
  }

-- * Concurrency groups

-- | The group every ungrouped 'Rule' belongs to.
anonymousGroup :: Text
anonymousGroup = "<anonymous>"

-- | The distinct group labels of a rule list, in first-appearance order,
-- and the dense identifier parallel to each input label that
-- @libhegel@'s @rule_groups@ array wants.
--
-- 'Nothing' resolves to 'anonymousGroup'.
internGroups :: [Maybe Text] -> ([Text], [Int64])
internGroups labels = (reverse namesRev, reverse idsRev)
  where
    resolved = map (maybe anonymousGroup id) labels
    (namesRev, idsRev, _seen) = foldl' step ([], [], Map.empty) resolved
    step :: ([Text], [Int64], Map.Map Text Int64) -> Text -> ([Text], [Int64], Map.Map Text Int64)
    step (names, ids, seen) name = case Map.lookup name seen of
      Just gid -> (names, gid : ids, seen)
      Nothing ->
        let gid = fromIntegral (Map.size seen)
         in (name : names, gid : ids, Map.insert name gid seen)

-- * Concurrency

-- | The concurrency bounds a 'run' asks the engine for: the number of
-- workers to run each case, drawn from @[minWorkers, maxWorkers]@ and
-- weighted toward the maximum.
--
-- The engine decides the level; the caller runs exactly as many workers as it
-- draws.
--
-- Construct one with 'fixed', 'upTo', or 'between'.
data Concurrency = Concurrency
  { minWorkers :: Int,
    maxWorkers :: Int
  }
  deriving stock (Eq, Show)

-- | Run exactly @n@ workers every case. Draws no entropy to fix the level,
-- and @n == 1@ keeps the whole run deterministic.
fixed :: Int -> Concurrency
fixed n = Concurrency {minWorkers = n, maxWorkers = n}

-- | Run between 1 and @n@ workers, the level drawn per case.
upTo :: Int -> Concurrency
upTo = between 1

-- | Run between @lo@ and @hi@ workers, the level drawn per case.
between :: Int -> Int -> Concurrency
between lo hi = Concurrency {minWorkers = lo, maxWorkers = hi}

-- * Execution

-- | Run a concurrent stateful test.
run :: forall s m. (MonadUnliftIO m) => Concurrency -> Machine s m -> PropertyT m ()
run bounds machine = do
  when (null machine.rules) $
    throwIO (MalformedTest "Hegel.Stateful.Concurrent.run: a Machine must have at least one rule")
  when (bounds.minWorkers < 1 || bounds.maxWorkers < bounds.minWorkers) $
    throwIO (MalformedTest "Hegel.Stateful.Concurrent.run: concurrency bounds must satisfy 1 <= min <= max")

  env <- askEnv
  liftIO (checkCloneDepth env)

  let tc = env.testCase
      (_groupNames, groupIds) = internGroups (map (.group) machine.rules)
      indexedRules = zip [0 ..] machine.rules

      -- Every invariant's draws (and any failure) run on the root test
      -- case, ambient journal untouched: unlike a worker's dispatch, only
      -- this one root handle ever touches it, so nothing here needs the
      -- concurrency safety a worker's env forces.
      checkInvariants :: s -> PropertyT m ()
      checkInvariants s =
        forM_ machine.invariants \invariant -> withScope InStep (invariant.check s)

  -- Acquire the state-machine handle and register its release atomically
  -- under 'mask_', the same fix 'Hegel.Property.Internal.resource' applies.
  (sm, concurrency) <-
    withRunInIO \runInIO ->
      mask_ do
        acquired <-
          newConcurrentStateMachine
            tc
            (map (.name) machine.rules)
            groupIds
            (map (.name) machine.invariants)
            (fromIntegral bounds.minWorkers)
            (fromIntegral bounds.maxWorkers)
        runInIO (registerFinalizer (freeStateMachine tc (fst acquired)))
        pure acquired

  s0 <- withScope CaseSetup machine.initial
  checkInvariants s0

  withBaseRunInIO \runBase ->
    withClones concurrency tc \clones -> do
      let mkWorker clone =
            let workerEnv =
                  env
                    { testCase = clone,
                      journal = Silent,
                      cloneDepth = env.cloneDepth + 1
                    }
                dispatch ruleIndex = do
                  let matchedRule = case lookup ruleIndex indexedRules of
                        Just r -> r
                        -- @libhegel@ guarantees indices in @[0, num_rules)@,
                        -- so this is unreachable unless the engine itself is
                        -- misbehaving.
                        Nothing ->
                          error
                            ( "Hegel.Stateful.Concurrent.run: libhegel returned rule index "
                                <> show ruleIndex
                                <> " for a machine with "
                                <> show (length machine.rules)
                                <> " rules. This should be impossible; please report it as a libhegel bug."
                            )
                  workerForks <- newOpenForks
                  let dispatchEnv = workerEnv {openForks = workerForks}
                  runBase (runPropertyT dispatchEnv (withScope InStep (matchedRule.apply s0)))
                  closeOpenForks workerForks
             in Worker {testCase = clone, dispatch, onRejected = pure ()}

          workers = map mkWorker clones

          roundLoop = do
            mGroupId <- stateMachineNextGroup tc sm
            case mGroupId of
              -- HEGEL_STATE_MACHINE_DONE: the whole machine is done stepping.
              Nothing -> pure ()
              Just _groupId -> do
                -- The dropped-panics list can never render today: it is
                -- non-empty only when some other worker's overrun, invalid
                -- conclusion, or control error outranked a panic, which
                -- needs at least two workers, and every worker's journal is
                -- 'Silent' whenever there is more than one. Surfacing it is
                -- deferred to the reporter follow-up that gives this module
                -- a journal to surface it into.
                (verdict, _dropped) <- runRound sm workers
                case verdict of
                  ContinueRound -> do
                    runBase (runPropertyT env (checkInvariants s0))
                    roundLoop
                  -- Base 'Control.Exception.throwIO', not the 'UnliftIO'
                  -- import above: 'e' may carry 'TestStopped'\/'AssumeRejected',
                  -- async-classified so a user's catch-all cannot swallow
                  -- them, and 'UnliftIO.throwIO' would wrap an async
                  -- exception so 'Hegel.Runner.runTestCase's 'catchControl'
                  -- no longer recognizes it.
                  Conclude e -> E.throwIO e

      roundLoop
{-# INLINEABLE run #-}
{-# SPECIALIZE run :: Concurrency -> Machine s IO -> PropertyT IO () #-}
