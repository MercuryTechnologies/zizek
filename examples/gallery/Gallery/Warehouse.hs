-- | A sequential stateful machine whose failure splices a rule and an
-- invariant side by side.
--
-- The warehouse keeps on-hand stock, the table of pending orders, and a
-- per-item reservation total that the rules maintain incrementally as a cache
-- of that table. Cancelling an order releases the newest order's reservation
-- instead of the cancelled one, which is harmless exactly when the two
-- coincide. The minimal counterexample therefore needs two orders for the
-- same item in different quantities, then a cancellation of the older one.
--
-- The log shows labeled draws and each new order's id as the step's response.
-- The failing step splices the rule that broke the cache and the invariant
-- that noticed. The failure is filed in the example database, so the report
-- ends with the stored footer.
module Gallery.Warehouse (scenario) where

import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Gallery.Scenario
import Hegel.Database (Database (..))
import Hegel.Gen qualified as Gen
import Hegel.Property (annotate, assert, assume, forAllWithLabel, (===))
import Hegel.Report (FailureEvidence (..), Report (..), Reproduction (..), renderValue)
import Hegel.Settings (Settings (..))
import Hegel.Stateful qualified as Stateful

scenario :: Scenario
scenario =
  Scenario
    { name = "warehouse",
      title = "sequential stateful: a stale reservation cache",
      settings =
        seeded
          { database = Just (DatabaseDirectory databaseDirectory),
            databaseKey = Just "gallery/warehouse"
          },
      property = Stateful.run machine,
      ascii = False,
      attempts = 1,
      expect = pure . check
    }

-- | Where the warehouse files its failure. The gallery clears it before every
-- run, so each run finds the failure afresh.
databaseDirectory :: FilePath
databaseDirectory = ".hegel/gallery"

data Warehouse = Warehouse
  { -- | On-hand quantity per item.
    stock :: Map Text Int,
    -- | Reservation totals per item, kept incrementally by the rules.
    reserved :: Map Text Int,
    -- | Open orders, from id to item and quantity.
    pending :: Map Int (Text, Int),
    nextOrder :: Int
  }

-- | Two items, because the bug needs two orders for the same item and each
-- extra item makes that coincidence rarer.
items :: [Text]
items = ["apple", "banana"]

-- | Adjust a per-item tally, dropping entries that reach zero.
tally :: Text -> Int -> Map Text Int -> Map Text Int
tally item dq = Map.filter (> 0) . Map.insertWith (+) item dq

restock :: Stateful.Rule Warehouse IO
restock =
  Stateful.rule "restock" \w -> do
    item <- forAllWithLabel "item" (Gen.element items)
    qty <- forAllWithLabel "qty" (Gen.int & Gen.min 5 & Gen.max 10 & Gen.build)
    pure w {stock = tally item qty w.stock}

-- | Reserve unreserved stock for a new order and respond with its id.
placeOrder :: Stateful.Rule Warehouse IO
placeOrder =
  Stateful.rule "place_order" \w -> do
    item <- forAllWithLabel "item" (Gen.element items)
    qty <- forAllWithLabel "qty" (Gen.int & Gen.min 1 & Gen.max 3 & Gen.build)
    assume (qty <= Map.findWithDefault 0 item w.stock - Map.findWithDefault 0 item w.reserved)
    Stateful.respondShow w.nextOrder
    pure
      w
        { pending = Map.insert w.nextOrder (item, qty) w.pending,
          reserved = tally item qty w.reserved,
          nextOrder = w.nextOrder + 1
        }

-- | Ship an order, consuming both its stock and its reservation.
fulfillOrder :: Stateful.Rule Warehouse IO
fulfillOrder =
  Stateful.rule "fulfill_order" \w -> do
    assume (not (Map.null w.pending))
    oid <- forAllWithLabel "order" (Gen.element (Map.keys w.pending))
    let (item, qty) = w.pending Map.! oid
    pure
      w
        { pending = Map.delete oid w.pending,
          reserved = tally item (negate qty) w.reserved,
          stock = tally item (negate qty) w.stock
        }

cancelOrder :: Stateful.Rule Warehouse IO
cancelOrder =
  Stateful.rule "cancel_order" \w -> do
    assume (not (Map.null w.pending))
    oid <- forAllWithLabel "order" (Gen.element (Map.keys w.pending))
    -- BUG: releases the reservation of the newest pending order instead of
    -- the cancelled one.
    case Map.lookupMax w.pending of
      Nothing -> pure w
      Just (newest, (item, qty)) -> do
        annotate ("releasing the hold of order " <> renderValue newest)
        pure w {pending = Map.delete oid w.pending, reserved = tally item (negate qty) w.reserved}

-- | The cache equals the totals recomputed from the order table.
--
-- This is the claim the cache exists to keep, and recomputing it is cheap, so
-- it runs after every step and the failure lands on the step that broke it.
reservationsMatchOrders :: Stateful.Invariant Warehouse IO
reservationsMatchOrders =
  Stateful.alwaysInvariant "reservations_match_orders" \w ->
    w.reserved === Map.filter (> 0) (Map.fromListWith (+) (Map.elems w.pending))

-- | Broader sanity claims, sampled at a few join points per test case and
-- always checked on the final state.
stockCoversReservations :: Stateful.Invariant Warehouse IO
stockCoversReservations =
  Stateful.invariant "stock_covers_reservations" \w ->
    assert
      (and [Map.findWithDefault 0 item w.stock >= q | (item, q) <- Map.toList w.reserved])
      "every reservation is backed by on-hand stock"

stockNonNegative :: Stateful.Invariant Warehouse IO
stockNonNegative =
  Stateful.invariant "stock_non_negative" \w ->
    assert (all (>= 0) w.stock) "stock never goes negative"

machine :: Stateful.Machine Warehouse IO
machine =
  Stateful.Machine
    { initial = pure Warehouse {stock = Map.empty, reserved = Map.empty, pending = Map.empty, nextOrder = 1},
      rules = [restock, placeOrder, fulfillOrder, cancelOrder],
      invariants = [reservationsMatchOrders, stockCoversReservations, stockNonNegative],
      stepCount = Stateful.defaultStepCount
    }

check :: Report -> [Text]
check report = case captured report of
  Right [evidence] ->
    let rows = logRows evidence
     in ensureEqual "message" "=== failed, values are not equal" evidence.message
          <> ensureEqual "steps" ["restock", "place_order", "place_order", "cancel_order"] (stepRules rows)
          <> ensureEqual "failing step" [False, False, False, True] (map (.failed) rows)
          <> ensure (isStored report.reproduction) ("expected a stored reproduction, got " <> renderValue report.reproduction)
  Right evidence -> ["expected one failure, got " <> renderValue (length evidence)]
  Left mismatch -> [mismatch]
  where
    isStored = \case
      Stored _ -> True
      _ -> False
