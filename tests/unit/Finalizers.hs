-- | Unit tests for 'Hegel.Property.registerFinalizer' and the per-case drain.
module Finalizers (spec) where

import Control.Exception (displayException)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as T
import Hegel (Gen)
import Hegel.Gen qualified as Gen
import Hegel.Internal.Control (MalformedTest (..))
import Hegel.Property
  ( assert,
    check,
    discard,
    failure,
    forAll,
    registerFinalizer,
    (===),
  )
import Hegel.Property.Internal (Env (..), Journal (..), askEnv)
import Hegel.Report (Abort (..), Note (..), NoteKind (..), Report (..), Result (..), Stats (..))
import Hegel.Settings (Settings (..), defaultSettings)
import Hegel.Stateful qualified as Stateful
import Test.Hspec
import UnliftIO.Exception (throwIO)
import UnliftIO.IORef (modifyIORef', newIORef, readIORef, writeIORef)

intR :: (Int, Int) -> Gen Int
intR (lo, hi) = Gen.integral & Gen.min lo & Gen.max hi & Gen.build

-- A counter machine whose only rule climbs past the invariant bound, giving a
-- deterministic counterexample (and thus a reconstruction replay).
newtype Counter = Counter Int

increment :: Stateful.Rule Counter IO
increment = Stateful.Rule "increment" \(Counter n) -> pure (Counter (n + 1))

neverAboveFive :: Stateful.Invariant Counter IO
neverAboveFive =
  Stateful.Invariant "never_above_five" \(Counter n) ->
    assert (n <= 5) "counter does not exceed 5"

spec :: Spec
spec = describe "registerFinalizer" do
  it "runs once per case on success, with no cross-case bleed" do
    ref <- newIORef (0 :: Int)
    report <- check defaultSettings do
      registerFinalizer (modifyIORef' ref (+ 1))
      pure ()
    count <- readIORef ref
    report.result `shouldSatisfy` \case
      Ok -> True
      _ -> False
    count `shouldSatisfy` (> 0)
    -- One drain per attempted case, valid or discarded.
    count `shouldBe` report.stats.valid + report.stats.invalid

  it "runs on a failing case" do
    ran <- newIORef False
    report <- check defaultSettings do
      registerFinalizer (writeIORef ran True)
      _ <- forAll (intR (0, 100))
      failure "always fails"
    readIORef ran `shouldReturn` True
    case report.result of
      Counterexample {} -> pure ()
      other -> expectationFailure ("expected Counterexample, got: " <> show other)

  it "runs on a discarded case" do
    ran <- newIORef False
    report <- check defaultSettings do
      registerFinalizer (writeIORef ran True)
      discard
    readIORef ran `shouldReturn` True
    case report.result of
      GaveUp _ -> pure ()
      other -> expectationFailure ("expected GaveUp, got: " <> show other)

  it "runs finalizers LIFO (last registered, first run)" do
    order <- newIORef ([] :: [Text])
    _ <- check (defaultSettings {testCases = 1}) do
      registerFinalizer (modifyIORef' order (++ ["a"]))
      registerFinalizer (modifyIORef' order (++ ["b"]))
      pure ()
    readIORef order `shouldReturn` ["b", "a"]

  it "aborts the run as Errored when a finalizer throws" do
    report <- check defaultSettings do
      registerFinalizer (throwIO (userError "teardown boom"))
      pure ()
    case report.result of
      Aborted (Errored _) -> pure ()
      other -> expectationFailure ("expected Aborted Errored, got: " <> show other)

  it "captures a control signal thrown by a finalizer as an abort (does not escape check)" do
    -- A finalizer runs after the case is markComplete'd, so a discard/stop it
    -- throws is misuse, not a live signal to honor. The drain must capture it
    -- (→ Errored), not let it escape check uncaught and crash the host.
    report <- check defaultSettings do
      registerFinalizer (discard :: IO ())
      pure ()
    case report.result of
      Aborted (Errored _) -> pure ()
      other -> expectationFailure ("expected Aborted Errored, got: " <> show other)

  it "finalizer failure wins over a body counterexample, but surfaces the case origin" do
    report <- check defaultSettings do
      registerFinalizer (throwIO (userError "teardown boom"))
      _ <- forAll (intR (0, 100))
      failure "body fails too"
    case report.result of
      Aborted (Errored e) -> do
        let msg = T.pack (displayException e)
        msg `shouldSatisfy` T.isInfixOf "teardown boom"
        -- The counterexample is discarded, but its origin is not silently lost.
        msg `shouldSatisfy` T.isInfixOf "already failed"
      other -> expectationFailure ("expected Aborted Errored (Errored wins), got: " <> show other)

  it "a draining finalizers does not mask a test run failure" do
    -- The body throws a MalformedTest *and* a registered finalizer throws.
    --
    -- The run's own exception is the primary diagnostic and must survive
    -- the drain, rather than being replaced by the finalizer's FinalizerFailed.
    report <- check defaultSettings do
      registerFinalizer (throwIO (userError "teardown boom"))
      throwIO (MalformedTest "malformed body")
    case report.result of
      Aborted (Errored e) -> do
        let msg = T.pack (displayException e)
        msg `shouldSatisfy` T.isInfixOf "malformed body"
        msg `shouldSatisfy` (not . T.isInfixOf "teardown boom")
      other -> expectationFailure ("expected the MalformedTest to win, got: " <> show other)

  it "footnotes a reconstruction-replay finalizer failure, keeping the counterexample" do
    -- The finalizer throws only when the case is recording — i.e. the terminal
    -- reconstruction replay, never the live/shrink cases (which are Silent).
    -- Decision: keep the counterexample, attach a footnote (no further cases to
    -- contaminate), rather than aborting as on the live path.
    report <- check defaultSettings do
      env <- askEnv
      let recording = case env.journal of
            Recording _ -> True
            Silent -> False
      registerFinalizer (when recording (throwIO (userError "replay teardown boom")))
      x <- forAll (intR (0, 100))
      x === x + 1
    case report.result of
      Counterexample {notes} -> do
        let foots =
              [ n
              | n <- notes,
                n.kind == Footnote,
                "replay teardown boom" `T.isInfixOf` n.text
              ]
        foots `shouldNotSatisfy` null
      other -> expectationFailure ("expected Counterexample, got: " <> show other)

  it "releases a stateful resource acquired in initial, with no leak across replays" do
    -- `live` stands in for an OS-level resource handle (e.g. a spawned thread):
    -- acquire in `initial`, register release. A failing invariant drives the
    -- full lifecycle — live cases, shrink replays, and the reconstruction replay
    -- — each of which acquires and must release, so the net balance is zero.
    live <- newIORef (0 :: Int)
    let machine =
          Stateful.Machine
            { initial = do
                liftIO (modifyIORef' live (+ 1))
                registerFinalizer (modifyIORef' live (subtract 1))
                pure (Counter 0),
              rules = [increment],
              invariants = [neverAboveFive]
            }
    report <- check defaultSettings (Stateful.run machine)
    case report.result of
      Counterexample {} -> pure ()
      other -> expectationFailure ("expected Counterexample, got: " <> show other)
    readIORef live `shouldReturn` 0
