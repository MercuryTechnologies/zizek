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
-- test_counter = check_ def (Stateful.run counterMachine)
-- @
--
-- = Notes
--
-- * The entire 'Machine' body re-runs on every shrink attempt and once more
--   to reconstruct the failure report. Effects against a real system under
--   test must tolerate repetition; reset\/setup belongs in 'initial'.
--
-- * Preconditions are expressed with 'assume'\/'discard' at the head of a
--   rule's 'apply'; a rejected precondition skips the step without discarding
--   the entire sequence, and the skipped step does not count toward
--   'Hegel.Settings.statefulStepCount'.
--
-- * @StateT s (PropertyT m)@ rules adapt with
--   @\\s -> execStateT myStateRule s :: s -> PropertyT m s@.
--
-- * The counterexample report nests each step's draws under its @Step N@
--   header (Rust's @child(2)@) and renders the failure in-band at the step
--   that produced it.
module Hegel.Stateful
  ( -- * Specification
    Rule (..),
    Invariant (..),
    Machine (..),

    -- * Execution
    run,

    -- * Reporting
    respond,
    respondShow,
  )
where

import Control.Exception (mask_)
import Control.Exception qualified
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)
import Hegel.Assertion (callSite)
import Hegel.Internal.Control (MalformedTest (..), onFailure)
import Hegel.Internal.DataSource
  ( Label (LabelStatefulRule),
    freeStateMachine,
    newStateMachine,
    startSpan,
    stateMachineNextGroup,
    stopSpan,
  )
import Hegel.Internal.StatefulRound (RoundVerdict (..), Worker (..), runRound)
import Hegel.Property.Internal
  ( Env (..),
    Journal (..),
    PropertyT,
    Scope (..),
    askEnv,
    failureDetails,
    nested,
    note,
    noteFailure,
    registerFinalizer,
    withScope,
  )
import Hegel.Report (NoteKind (Annotation, Response, StepHeader), renderValue)
import UnliftIO (MonadUnliftIO, throwIO, withRunInIO)
import UnliftIO.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)

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
{-# INLINE stepNote #-}

-- | Declare the current rule's result, for the failure report.
--
-- The event log renders it as the right-hand side of the step's
-- @call → response@ line (@read h₁ → Right "a"@); the headline
-- quotes it as the observed actual. A step's /last/ 'respond' wins. Rules
-- that never call it render without a response segment.
--
-- @
-- Stateful.Rule "read" \\s -> do
--   h <- forAll (Pool.reuse s.handles)
--   r <- liftIO (readHandle h)
--   Stateful.respond (T.pack (show r))
--   r === modelRead s h
--   pure s
-- @
respond :: (HasCallStack, MonadIO m) => Text -> PropertyT m ()
respond = note Response (callSite callStack)
{-# INLINE respond #-}

-- | 'respond' a value via its 'Show' instance.
respondShow :: (HasCallStack, MonadIO m, Show a) => a -> PropertyT m ()
respondShow = withFrozenCallStack (respond . renderValue)
{-# INLINE respondShow #-}

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
-- checks all invariants, then drives a round loop until the engine reports the
-- machine is done or the choice budget is exhausted.
--
-- Each round pulls rules from the engine until its own budget is exhausted,
-- then invariants are checked once at the round's join point.
--
-- The engine owns the step cap and bounds every case to at most
-- 'Hegel.Settings.statefulStepCount' steps.
run :: forall s m. (MonadUnliftIO m) => Machine s m -> PropertyT m ()
run machine = do
  when (null machine.rules) $
    throwIO $
      MalformedTest "Hegel.Stateful.run: a Machine must have at least one rule"

  env <- askEnv
  let tc = env.testCase

      -- Journal a real failure as an in-band 'Failure' note, then re-throw so
      -- the runner still sees the counterexample and drives shrinking.
      -- 'onFailure' passes control signals and async exceptions through
      -- untouched (see its haddock); do not replace it with a bare
      -- @catch \@SomeException@, which would swallow discard\/stop signals.
      --
      -- Under 'Silent' the bracket is skipped entirely: the failure note
      -- would go nowhere and the failure propagates to the runner either
      -- way, so only the 'Recording' reconstruction replay pays for the
      -- per-step catch machinery.
      withFailureNote :: forall a. PropertyT m a -> PropertyT m a
      withFailureNote = case env.journal of
        Silent -> id
        Recording _ -> \act ->
          withRunInIO \runInIO ->
            runInIO act `onFailure` \e ->
              let (message, loc, diff) = failureDetails e
               in runInIO (noteFailure loc diff message)

      -- Each invariant's draws (and any failure) report one level below the
      -- step header, via 'nested'.
      checkInvariants s =
        forM_ machine.invariants \invariant ->
          nested (withFailureNote (withScope InStep (invariant.check s)))

  -- Acquire the state-machine handle and register its release atomically
  -- under 'mask_', the same fix 'Hegel.Property.Internal.resource' applies,
  -- but without its 'InStep' rejection: unlike a pool, a nested state
  -- machine's own bookkeeping is fine to acquire from inside a rule's
  -- @apply@ (see the "nested Stateful.run inside a Rule's apply" test),
  -- and 'registerFinalizer' carries no such restriction on its own.
  sm <-
    withRunInIO \runInIO ->
      mask_ do
        sm <- newStateMachine tc (map (.name) machine.rules) (map (.name) machine.invariants)
        runInIO (registerFinalizer (freeStateMachine tc sm))
        pure sm

  s0 <- withFailureNote (withScope CaseSetup machine.initial)
  stepNote "Initial invariant check."
  checkInvariants s0

  -- The current model, threaded by 'Hegel.Internal.StatefulRound.Worker's
  -- IO-shaped dispatch rather than by return value: the generic round driver
  -- underneath knows nothing of a rule's state-by-value shape, since a
  -- genuinely concurrent worker couldn't thread state that way at all. The
  -- sequential 'Machine' here is the one-worker case, so bridging its
  -- by-value 'Rule.apply' onto that dispatch shape via a mutable cell costs
  -- nothing observable: only this one worker ever touches it.
  stateRef <- liftIO (newIORef s0)
  -- Total rule dispatches so far this case, for 'StepHeader' numbering;
  -- counts a rejected dispatch the same as a successful one, matching
  -- 'Hegel.Settings.statefulStepCount'\'s own accounting.
  attemptsRef <- liftIO (newIORef (0 :: Int))
  -- Whether any rule this round was rejected, so the round's span closes
  -- discarded and the shrinker can delete the whole round at once.
  rejectedRef <- liftIO (newIORef False)

  -- The engine halts the loop once it has handed out 'Hegel.Settings.statefulStepCount'
  -- steps, inclusive of steps with an 'assume'.
  --
  -- On the root handle, it asks the engine whether another round should run,
  -- then lets 'Hegel.Internal.StatefulRound.runRound' pull this round's rules
  -- until the one worker's own budget is exhausted.
  withRunInIO \runInIO -> do
    let dispatch ruleIndex = do
          let rule = case lookup ruleIndex (zip [0 ..] machine.rules) of
                Just r -> r
                -- @libhegel@ guarantees indices in @[0, num_rules)@, so
                -- this is unreachable unless the engine itself is
                -- misbehaving.
                Nothing ->
                  error
                    ( "Hegel.Stateful.run: libhegel returned rule index "
                        <> show ruleIndex
                        <> " for a machine with "
                        <> show (length machine.rules)
                        <> " rules. This should be impossible; please report it as a libhegel bug."
                    )
          stepIndex <- atomicModifyIORef' attemptsRef \a -> (a + 1, a + 1)
          runInIO $
            note
              (StepHeader stepIndex rule.name)
              Nothing
              ("Step " <> T.pack (show stepIndex) <> ": " <> rule.name)
          s <- readIORef stateRef
          s' <- runInIO (nested (withFailureNote (withScope InStep (rule.apply s))))
          writeIORef stateRef s'

        onRejected = do
          writeIORef rejectedRef True
          runInIO (stepNote "Rule stopped early due to violated assumption.")

        worker = Worker {testCase = tc, dispatch, onRejected}

        -- A round's draws share one 'LabelStatefulRule' span. On
        -- 'ContinueRound' it closes discarded exactly when some rule this
        -- round was rejected, so the shrinker can delete the whole round.
        roundLoop = do
          startSpan tc LabelStatefulRule
          mGroupId <- stateMachineNextGroup tc sm
          case mGroupId of
            -- HEGEL_STATE_MACHINE_DONE: the whole machine is done stepping.
            Nothing -> stopSpan tc False
            Just _groupId -> do
              writeIORef rejectedRef False
              (verdict, _dropped) <- runRound sm [worker]
              case verdict of
                ContinueRound -> do
                  rejected <- readIORef rejectedRef
                  stopSpan tc rejected
                  s' <- readIORef stateRef
                  runInIO (checkInvariants s')
                  roundLoop
                -- NOTE: This /must/ be 'Control.Exception.throwIO', as
                -- 'UnliftIO.throwIO' applies a synchronous exception wrapper,
                -- which can mess up our async control signals.
                Conclude e -> Control.Exception.throwIO e

    roundLoop
{-# INLINEABLE run #-}
{-# SPECIALIZE run :: Machine s IO -> PropertyT IO () #-}
