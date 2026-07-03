-- | End-to-end tests for the pool-event stream ("Hegel.Internal.Event"):
-- events recorded during the final reconstruction replay, sharing one clock
-- with the note journal.
module PoolEvents (spec) where

import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.List (nub, sort)
import Hegel (Gen)
import Hegel.Gen qualified as Gen
import Hegel.Pool (Pool)
import Hegel.Pool qualified as Pool
import Hegel.Property (assert, forAll)
import Hegel.Property.Internal (Env (..), askEnv)
import Hegel.Report
  ( Clock (..),
    Event (..),
    EventKind (..),
    Note (..),
    NoteKind (..),
    Report (..),
    Result (..),
  )
import Hegel.Runner (check)
import Hegel.Settings (defaultSettings)
import Hegel.Stateful qualified as Stateful
import Test.Hspec

intGen :: Gen Int
intGen = Gen.int & Gen.min 0 & Gen.max 100 & Gen.build

-- | A machine whose failure requires all three pool-event kinds to have
-- occurred: the invariant trips only once a reusable draw /and/ a consuming
-- draw have both happened, so the minimal counterexample must keep at least
-- one 'Born', one 'Reused', and one 'Consumed' event.
data Model = Model
  { pool :: Pool Int,
    reused :: Bool,
    consumed :: Bool
  }

eventfulMachine :: Stateful.Machine Model IO
eventfulMachine =
  Stateful.Machine
    { initial = do
        env <- askEnv
        p <- liftIO (Pool.new env.testCase)
        pure Model {pool = p, reused = False, consumed = False},
      rules =
        [ Stateful.Rule "register" \m -> do
            n <- forAll intGen
            liftIO (Pool.add m.pool n)
            pure m,
          Stateful.Rule "reuse" \m -> do
            _ <- forAll (Pool.valuesReusable m.pool)
            pure m {reused = True},
          Stateful.Rule "consume" \m -> do
            _ <- forAll (Pool.valuesConsumed m.pool)
            pure m {consumed = True}
        ],
      invariants =
        [ Stateful.Invariant "never_reuse_and_consume" \m ->
            assert (not (m.reused && m.consumed)) "reuse and consume never both happen (bug)"
        ]
    }

-- | Run 'eventfulMachine' to a counterexample and hand its journal and event
-- stream to the assertion body.
withEventfulCounterexample :: ([Note] -> [Event] -> Expectation) -> Expectation
withEventfulCounterexample body = do
  report <- check defaultSettings (Stateful.run eventfulMachine)
  case report.result of
    Counterexample {notes, events} -> body notes events
    other -> expectationFailure ("expected Counterexample, got: " <> show other)

clocks :: [Event] -> [Clock]
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
