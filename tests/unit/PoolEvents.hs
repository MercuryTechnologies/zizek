-- | End-to-end tests for the pool-event stream ("Hegel.Internal.Event"):
-- events recorded during the final reconstruction replay, sharing one clock
-- with the note journal.
module PoolEvents (spec) where

import Data.List (nub, sort)
import Hegel.Property (assert)
import Hegel.Report
  ( Event (..),
    Note (..),
    NoteKind (..),
    Operation (..),
    Report (..),
    Result (..),
    Tick (..),
  )
import Hegel.Runner (check)
import Hegel.Settings (defaultSettings)
import Hegel.Stateful qualified as Stateful
import Test.Hspec
import TraceFixtures (eventfulMachine)

-- | Run 'eventfulMachine' to a counterexample and hand its journal and event
-- stream to the assertion body.
withEventfulCounterexample :: ([Note] -> [Event] -> Expectation) -> Expectation
withEventfulCounterexample body = do
  report <- check defaultSettings (Stateful.run eventfulMachine)
  case report.result of
    Counterexample {notes, events} -> body notes events
    other -> expectationFailure ("expected Counterexample, got: " <> show other)

clocks :: [Event] -> [Tick]
clocks = fmap (.clock)

spec :: Spec
spec = describe "pool-event stream" do
  it "records all three event kinds for the eventful counterexample" do
    withEventfulCounterexample \_notes events -> do
      let kinds = fmap (.kind) events
      kinds `shouldSatisfy` any (\case Born _ -> True; _ -> False)
      kinds `shouldSatisfy` elem Reused
      kinds `shouldSatisfy` elem Consumed

  it "event clocks are strictly increasing" do
    withEventfulCounterexample \_notes events -> do
      let cs = clocks events
      cs `shouldSatisfy` \xs -> and (zipWith (<) xs (drop 1 xs))

  it "Born precedes every draw of the same value; Consumed is terminal" do
    withEventfulCounterexample \_notes events -> do
      -- (poolId, vid) pair.
      let refs = nub [e.var | e <- events]
          lifeOf ref = [e | e <- events, e.var == ref]
      sequence_
        [ do
            let es = lifeOf ref
            fmap (.kind) (take 1 es) `shouldBe` [Born Nothing]
            -- Nothing follows a consuming draw: the engine has no
            -- pool_remove, so Consumed is the death event.
            dropWhile (\e -> e.kind /= Consumed) es `shouldSatisfy` \case
              [] -> True
              [e] -> e.kind == Consumed
              _ -> False
        | ref <- refs
        ]

  it "every pool draw's event immediately precedes its Drawn note (clock adjacency)" do
    withEventfulCounterexample \notes events -> do
      let drawnClocks = [n.clock | n <- notes, n.kind == Drawn]
      sequence_
        [ succ e.clock `shouldSatisfy` (`elem` drawnClocks)
        | e <- events,
          e.kind == Reused || e.kind == Consumed
        ]

  it "notes and events share one clock (no duplicate stamps across streams)" do
    withEventfulCounterexample \notes events -> do
      let merged = sort (clocks events <> fmap (.clock) notes)
      merged `shouldSatisfy` \xs -> and (zipWith (<) xs (drop 1 xs))

  it "a pool-free machine records no events" do
    let machine =
          Stateful.Machine
            { initial = pure (0 :: Int),
              rules = [Stateful.Rule "increment" \n -> pure (n + 1)],
              invariants =
                [ Stateful.Invariant "never_above_five" \n ->
                    assert (n <= 5) "counter does not exceed 5"
                ]
            }
    report <- check defaultSettings (Stateful.run machine)
    case report.result of
      Counterexample {events} -> events `shouldBe` []
      other -> expectationFailure ("expected Counterexample, got: " <> show other)
