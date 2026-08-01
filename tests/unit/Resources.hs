-- | Unit tests for 'Hegel.Property.resource' and 'Hegel.Property.resource_'.
module Resources (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (displayException, fromException)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (def)
import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Internal.Control (MalformedTest (..))
import Hegel.Property
  ( assert,
    check,
    discard,
    registerFinalizer,
    resource,
    resource_,
  )
import Hegel.Property.Branch qualified as Branch
import Hegel.Property.Fork qualified as Fork
import Hegel.Report (Abort (..), Report (..), Result (..))
import Hegel.Settings (Settings (..), defaultSettings)
import Hegel.Stateful qualified as Stateful
import Test.Hspec
import UnliftIO.Exception (throwIO)
import UnliftIO.IORef (modifyIORef', newIORef, readIORef, writeIORef)

-- | A counter model reused across cases: 'increment' always succeeds, so a
-- machine built from it only fails when a rule or invariant deliberately
-- does.
newtype Counter = Counter Int

increment :: Stateful.Rule Counter IO
increment = Stateful.Rule "increment" \(Counter n) -> pure (Counter (n + 1))

-- | A deliberately violated invariant, giving a machine a deterministic
-- counterexample (and thus a reconstruction replay) when nothing else about
-- it aborts the run first.
neverAboveFive :: Stateful.Invariant Counter IO
neverAboveFive =
  Stateful.Invariant "never_above_five" \(Counter n) ->
    assert (n <= 5) "counter does not exceed 5"

isOk :: Result -> Bool
isOk = \case
  Ok -> True
  _ -> False

-- | True when the run aborted specifically because 'resource' refused to
-- acquire, identified by the 'MalformedTest' message rather than just the
-- 'Aborted' \/ 'Errored' shape, so an unrelated abort (a leaked fork, a
-- clone-depth guard) can't make a guard test pass for the wrong reason.
isResourceGuardAbort :: Result -> Bool
isResourceGuardAbort = \case
  Aborted (Errored e) -> case fromException e of
    Just (MalformedTest msg) -> "resource:" `T.isInfixOf` msg
    Nothing -> False
  _ -> False

-- | A property body that never completes on its own, so a caller can only
-- observe it settling if something asynchronously kills it first.
--
-- Blocks on real time rather than looping on a draw, for the same reason
-- 'ForkProperties.spinForever' does: an unbounded draw loop would inflate the
-- engine's initial-size estimate for the case by however many iterations run
-- before the kill lands.
spinForever :: IO ()
spinForever = threadDelay maxBound

spec :: Spec
spec = describe "resource" do
  it "runs open, then drains close at case end, in a plain property" do
    opened <- newIORef (0 :: Int)
    closed <- newIORef (0 :: Int)
    report <- check def do
      _ <- resource (modifyIORef' opened (+ 1)) (const (modifyIORef' closed (+ 1)))
      pure ()
    report.result `shouldSatisfy` isOk
    o <- readIORef opened
    c <- readIORef closed
    o `shouldSatisfy` (> 0)
    c `shouldBe` o

  it "resource_ behaves like resource with no handle to thread through" do
    opened <- newIORef False
    closed <- newIORef False
    report <- check def do
      resource_ (writeIORef opened True) (writeIORef closed True)
    report.result `shouldSatisfy` isOk
    readIORef opened `shouldReturn` True
    readIORef closed `shouldReturn` True

  it "runs open/close once per case in Machine.initial, with no leak across shrinks or the reconstruction replay" do
    opened <- newIORef (0 :: Int)
    closed <- newIORef (0 :: Int)
    let machine =
          Stateful.Machine
            { initial = do
                _ <- resource (modifyIORef' opened (+ 1)) (const (modifyIORef' closed (+ 1)))
                pure (Counter 0),
              rules = [increment],
              invariants = [neverAboveFive]
            }
    report <- check def (Stateful.run machine)
    case report.result of
      Counterexample {} -> pure ()
      other -> expectationFailure ("expected Counterexample, got: " <> show other)
    o <- readIORef opened
    c <- readIORef closed
    o `shouldSatisfy` (> 0)
    c `shouldBe` o

  it "throws MalformedTest inside a Rule's apply, even when the rule fires exactly once" do
    -- Proves the guard is scope-gated, not count-gated: this rule never gets
    -- a second application, since the guard aborts the run on the first, so
    -- a count-gated guard tolerating "it only happened once" would pass here
    -- when it shouldn't.
    let onceRule :: Stateful.Rule Counter IO
        onceRule =
          Stateful.Rule "acquire_once" \(Counter n) -> do
            _ <- resource (pure ()) (const (pure ()))
            pure (Counter (n + 1))
        machine =
          Stateful.Machine
            { initial = pure (Counter 0),
              rules = [onceRule],
              invariants = []
            }
    report <- check def (Stateful.run machine)
    report.result `shouldSatisfy` isResourceGuardAbort

  it "throws MalformedTest inside an Invariant's check after a successful step" do
    let checkAfterStep :: Stateful.Invariant Counter IO
        checkAfterStep =
          Stateful.Invariant "resource_after_step" \(Counter n) ->
            when (n > 0) do
              _ <- resource (pure ()) (const (pure ()))
              pure ()
        machine =
          Stateful.Machine
            { initial = pure (Counter 0),
              rules = [increment],
              invariants = [checkAfterStep]
            }
    report <- check def (Stateful.run machine)
    report.result `shouldSatisfy` isResourceGuardAbort

  it "throws MalformedTest inside an Invariant's initial check, before any step runs" do
    -- Unlike the previous case, this invariant calls resource unconditionally,
    -- so it trips on the very first, pre-loop checkInvariants call rather than
    -- only after a step. Pins that the guard covers that call site too, not
    -- just the post-step ones.
    let checkOnInitial :: Stateful.Invariant Counter IO
        checkOnInitial =
          Stateful.Invariant "resource_on_initial" \_ -> do
            _ <- resource (pure ()) (const (pure ()))
            pure ()
        machine =
          Stateful.Machine
            { initial = pure (Counter 0),
              rules = [increment],
              invariants = [checkOnInitial]
            }
    report <- check def (Stateful.run machine)
    report.result `shouldSatisfy` isResourceGuardAbort

  it "still allows registerFinalizer called directly inside a Rule's apply" do
    ran <- newIORef (0 :: Int)
    let bumpingRule :: Stateful.Rule Counter IO
        bumpingRule =
          Stateful.Rule "bump_and_register" \(Counter n) -> do
            registerFinalizer (modifyIORef' ran (+ 1))
            pure (Counter (n + 1))
        machine =
          Stateful.Machine
            { initial = pure (Counter 0),
              rules = [bumpingRule],
              invariants = []
            }
    report <- check def (Stateful.run machine)
    report.result `shouldSatisfy` isOk
    readIORef ran >>= (`shouldSatisfy` (> 0))

  it "drains multiple resource acquisitions LIFO" do
    order <- newIORef ([] :: [Text])
    _ <- check (defaultSettings {testCases = 1}) do
      _ <- resource (modifyIORef' order (++ ["a-open"])) (const (modifyIORef' order (++ ["a-close"])))
      _ <- resource (modifyIORef' order (++ ["b-open"])) (const (modifyIORef' order (++ ["b-close"])))
      pure ()
    readIORef order `shouldReturn` ["a-open", "b-open", "b-close", "a-close"]

  it "still drains close on a discarded case" do
    closed <- newIORef False
    report <- check def do
      _ <- resource (pure ()) (const (writeIORef closed True))
      discard
    readIORef closed `shouldReturn` True
    case report.result of
      GaveUp _ -> pure ()
      other -> expectationFailure ("expected GaveUp, got: " <> show other)

  it "aborts the run as Errored when a resource's close throws" do
    report <- check def do
      _ <- resource (pure ()) (const (throwIO (userError "close boom")))
      pure ()
    case report.result of
      Aborted (Errored e) -> T.pack (displayException e) `shouldSatisfy` T.isInfixOf "close boom"
      other -> expectationFailure ("expected Aborted Errored, got: " <> show other)

  it "keeps InStep scope through a nested Stateful.run inside a Rule's apply" do
    -- withScope only ever tightens (max): an inner Machine's own initial is
    -- marked CaseSetup, but that cannot downgrade the outer rule's InStep, so
    -- resource still throws inside it.
    let innerMachine :: Stateful.Machine Counter IO
        innerMachine =
          Stateful.Machine
            { initial = do
                _ <- resource (pure ()) (const (pure ()))
                pure (Counter 0),
              rules = [increment],
              invariants = []
            }
        outerRule :: Stateful.Rule Counter IO
        outerRule =
          Stateful.Rule "run_inner_machine" \(Counter n) -> do
            Stateful.run innerMachine
            pure (Counter (n + 1))
        outerMachine =
          Stateful.Machine
            { initial = pure (Counter 0),
              rules = [outerRule],
              invariants = []
            }
    report <- check def (Stateful.run outerMachine)
    report.result `shouldSatisfy` isResourceGuardAbort

  describe "concurrent branches and forks" do
    it "throws MalformedTest for resource inside Branch.concurrently within a Rule's apply" do
      let bugRule :: Stateful.Rule Counter IO
          bugRule =
            Stateful.Rule "branch_resource" \(Counter n) -> do
              _ <- Branch.concurrently (resource (pure ()) (const (pure ()))) (pure ())
              pure (Counter (n + 1))
          machine =
            Stateful.Machine
              { initial = pure (Counter 0),
                rules = [bugRule],
                invariants = []
              }
      report <- check def (Stateful.run machine)
      report.result `shouldSatisfy` isResourceGuardAbort

    it "throws MalformedTest for resource inside a joined Fork.spawn within a Rule's apply" do
      let bugRule :: Stateful.Rule Counter IO
          bugRule =
            Stateful.Rule "fork_resource" \(Counter n) -> do
              f <- Fork.spawn (resource (pure ()) (const (pure ())))
              Fork.join f
              pure (Counter (n + 1))
          machine =
            Stateful.Machine
              { initial = pure (Counter 0),
                rules = [bugRule],
                invariants = []
              }
      report <- check def (Stateful.run machine)
      report.result `shouldSatisfy` isResourceGuardAbort

    it "allows resource inside Branch.concurrently in a plain, non-stateful property" do
      -- Same combinator as above, but the ambient scope is Unrestricted
      -- outside any rule, so this is distinguished by scope, not by which
      -- combinator is used.
      ran <- newIORef False
      report <- check def do
        ((), n) <- Branch.concurrently (resource (pure ()) (const (writeIORef ran True))) (pure (1 :: Int))
        assert (n == 1) "branch result survives"
      report.result `shouldSatisfy` isOk
      readIORef ran `shouldReturn` True

    it "still drains a resource's close when Fork.cancel asynchronously kills the fork body" do
      -- By the time this fork blocks in spinForever, resource has already
      -- acquired and registered: the finalizer is live before the kill
      -- lands. This checks that an async exception delivered afterward, from
      -- Fork.cancel's Async.uninterruptibleCancel, still lets runBranch's own
      -- exception handling drain it, rather than leaking it.
      closed <- newIORef False
      report <- check def do
        f <- Fork.spawn do
          _ <- resource (pure ()) (const (writeIORef closed True))
          liftIO spinForever
        Fork.cancel f
      report.result `shouldSatisfy` isOk
      readIORef closed `shouldReturn` True
