-- | Stateful (model-based) testing with engine-owned swarm selection.
--
-- Define a 'Machine' — initial state, rules, invariants — and run it with
-- 'run'. The engine picks which rules to execute each step and restricts the
-- active subset per test case (swarm testing), then shrinks the rule sequence
-- to a minimal counterexample automatically.
--
-- = Usage
--
-- @
-- import Hegel.Stateful qualified as Stateful
--
-- data Counter = Counter Int
--
-- increment :: Stateful.Rule Counter IO
-- increment = Stateful.Rule "increment" \\(Counter n) -> pure (Counter (n + 1))
--
-- neverAboveTen :: Stateful.Invariant Counter IO
-- neverAboveTen = Stateful.Invariant "never_above_ten" \\(Counter n) ->
--   assert (n <= 10) "counter stays small"
--
-- counterMachine :: Stateful.Machine Counter IO
-- counterMachine = Stateful.Machine
--   { initial    = pure (Counter 0)
--   , rules      = [increment]
--   , invariants = [neverAboveTen]
--   }
--
-- test_counter :: IO ()
-- test_counter = check_ defaultSettings (Stateful.run counterMachine)
-- @
--
-- = Notes
--
-- * The entire 'Machine' body re-runs on every shrink attempt and once more
--   to reconstruct the failure report. Effects against a real system under
--   test must tolerate repetition; reset\/setup belongs in 'initial'.
--
-- * Preconditions are expressed with 'assume'\/'discard' at the head of a
--   rule's 'apply'. A rejected precondition skips the step (the attempt still
--   counts toward the livelock guard) but does not discard the whole sequence.
--
-- * @StateT s (PropertyT m)@ rules adapt with
--   @\\s -> execStateT myStateRule s :: s -> PropertyT m s@.
--
-- * Report indentation (Rust's @child(2)@) is cosmetic and out of scope for
--   v1; notes appear flat in the counterexample report.
module Hegel.Stateful
  ( -- * Specification
    Rule (..),
    Invariant (..),
    Machine (..),

    -- * Execution
    run,
  )
where

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Gen qualified as Gen
import Hegel.Internal.Control (ControlSignal (..), MalformedTest (..), catchControl)
import Hegel.Internal.DataSource (newStateMachine, stateMachineNextRule)
import Hegel.Property.Internal
  ( Env (..),
    PropertyT,
    askEnv,
    forAllSilent,
    note,
  )
import Hegel.Report (NoteKind (Annotation))
import UnliftIO (MonadUnliftIO, throwIO, withRunInIO)

-- | A rule applied to the model during a stateful test.
--
-- The 'apply' function may draw values, assert things, and interact with a
-- real system under test via 'liftIO'. Return the updated model state.
-- Use 'assume'\/'discard' at the head of 'apply' to express preconditions.
data Rule s m = Rule
  { name :: !Text,
    apply :: s -> PropertyT m s
  }

-- | An invariant checked after every successful rule application and after
-- the initial state is constructed.
--
-- May draw values but must not modify the model.
data Invariant s m = Invariant
  { name :: !Text,
    check :: s -> PropertyT m ()
  }

-- | Journal a step annotation without a source location.
--
-- These entries are journal structure emitted by the runner itself; a
-- call-stack loc would point inside this module, which the rich renderer
-- would then try to splice as if it were the user's test source.
stepNote :: (MonadIO m) => Text -> PropertyT m ()
stepNote = note Annotation Nothing

-- | A complete stateful test specification.
data Machine s m = Machine
  { -- | Construct the initial model state. May draw values.
    initial :: PropertyT m s,
    rules :: [Rule s m],
    invariants :: [Invariant s m]
  }

-- | Run a stateful test.
--
-- Registers the given 'Machine' with @libhegel@, constructs the initial state,
-- checks all invariants, then drives a rule loop until the step cap or choice
-- budget is exhausted, checking invariants after every successful step.
--
-- The step cap is drawn unconditionally, including on replay: the draw is part
-- of the choice sequence, so omitting it would misalign every later draw and
-- the counterexample would not reproduce.
run :: forall s m. (MonadUnliftIO m) => Machine s m -> PropertyT m ()
run machine = do
  when (null machine.rules) $
    throwIO (MalformedTest "Hegel.Stateful.run: a Machine must have at least one rule")

  env <- askEnv
  let tc = env.testCase
      checkInvariants s = forM_ machine.invariants \invariant -> invariant.check s

  machineId <- liftIO (newStateMachine tc (map (.name) machine.rules) (map (.name) machine.invariants))

  s0 <- machine.initial
  stepNote "Initial invariant check."
  checkInvariants s0

  stepCap <- min 50 <$> forAllSilent (Gen.int & Gen.min 1 & Gen.build)

  -- Ported from stateful.rs:263-293; the 1-based display step is @attempts + 1@,
  -- and @succeeded@ counts only steps that ran to completion.
  let loop :: s -> Int -> Int -> PropertyT m ()
      loop s succeeded attempts
        | not (continueLoop succeeded attempts stepCap) = pure ()
        | otherwise = do
            -- STOP_TEST from next_rule propagates to the runner; we don't catch it.
            ruleIndex <- liftIO (stateMachineNextRule tc machineId)
            let rule = case lookup ruleIndex (zip [0 ..] machine.rules) of
                  Just r -> r
                  -- @libhegel@ guarantees indices in @[0, num_rules)@, so this
                  -- is unreachable unless the engine itself is misbehaving.
                  Nothing ->
                    error
                      ( "Hegel.Stateful.run: libhegel returned rule index "
                          <> show ruleIndex
                          <> " for a machine with "
                          <> show (length machine.rules)
                          <> " rules. This should be impossible; please report it as a libhegel bug."
                      )
            stepNote ("Step " <> T.pack (show (attempts + 1)) <> ": " <> rule.name)
            -- Only control signals are caught here; a real failure propagates
            -- out to the runner as the counterexample.
            verdict <-
              withRunInIO \runInIO ->
                (Right <$> runInIO (rule.apply s))
                  `catchControl` (pure . Left)
            case verdict of
              Right s' -> do
                checkInvariants s'
                loop s' (succeeded + 1) (attempts + 1)
              Left Stop -> pure ()
              Left Assume -> do
                stepNote "Rule stopped early due to violated assumption."
                loop s succeeded (attempts + 1)

  loop s0 0 0
  where
    -- Stop at the step cap, or when the attempt budget is spent.
    continueLoop :: Int -> Int -> Int -> Bool
    continueLoop succeeded attempts stepCap =
      succeeded < stepCap && attempts < attemptBudget
      where
        attemptBudget
          | succeeded == 0 = max (10 * stepCap) 1000
          | otherwise = 10 * stepCap
