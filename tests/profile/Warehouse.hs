-- | Profiling copy of the demo warehouse machine
-- (@tests/demo/stateful-rich/Main.hs@, scenario 7), with the deliberate
-- @cancel_order@ bug made toggleable: 'Fixed' gives a passing machine (the
-- @mixed@ scenario), 'Buggy' a failing one (the @shrink@ scenario).
--
-- Deliberately duplicated rather than shared with the demo: the demo's
-- source /text/ is its content — the rich renderer splices those exact
-- declarations — so profiling-motivated edits here must not perturb it.
module Warehouse
  ( Bug (..),
    machine,
  )
where

import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Hegel.Assertion (assert)
import Hegel.Gen qualified as Gen
import Hegel.Property (annotate, assume, forAll, (===))
import Hegel.Report (renderValue)
import Hegel.Stateful qualified as Stateful

-- | Whether 'machine' carries the deliberate @cancel_order@ bug.
data Bug = Fixed | Buggy

-- | A warehouse whose model couples three structures: on-hand stock, a
-- __denormalized__ per-SKU reservation cache, and the pending-order table
-- the cache summarizes. The consistency invariant recomputes the cache from
-- the table on every step.
data Warehouse = Warehouse
  { -- | On-hand quantity per SKU.
    stock :: Map Text Int,
    -- | Reservation totals per SKU — a cache of 'pending', kept
    -- incrementally by the rules (that's where the bug lives).
    reserved :: Map Text Int,
    -- | Open orders: id → (SKU, quantity).
    pending :: Map Int (Text, Int),
    nextOrder :: Int
  }
  deriving stock (Show)

-- | Two SKUs, not more: the bug needs two same-SKU orders, and each extra
-- SKU multiplies the engine's search for that conjunction.
skus :: [Text]
skus = ["apple", "banana"]

-- | Adjust a per-SKU tally, dropping entries at (or below) zero.
tally :: Text -> Int -> Map Text Int -> Map Text Int
tally sku dq = Map.filter (> 0) . Map.insertWith (+) sku dq

restock :: Stateful.Rule Warehouse IO
restock =
  Stateful.Rule "restock" \w -> do
    sku <- forAll (Gen.element skus)
    qty <- forAll (Gen.int & Gen.min 5 & Gen.max 10 & Gen.build)
    pure w {stock = tally sku qty w.stock}

-- | Reserve stock for a new order; only as much as is unreserved.
placeOrder :: Stateful.Rule Warehouse IO
placeOrder =
  Stateful.Rule "place_order" \w -> do
    sku <- forAll (Gen.element skus)
    qty <- forAll (Gen.int & Gen.min 1 & Gen.max 3 & Gen.build)
    let available =
          Map.findWithDefault 0 sku w.stock - Map.findWithDefault 0 sku w.reserved
    assume (qty <= available)
    annotate ("order #" <> renderValue w.nextOrder <> " reserves " <> renderValue qty <> " " <> sku)
    pure
      w
        { pending = Map.insert w.nextOrder (sku, qty) w.pending,
          reserved = tally sku qty w.reserved,
          nextOrder = w.nextOrder + 1
        }

-- | Ship an order: consumes both the stock and the reservation.
fulfillOrder :: Stateful.Rule Warehouse IO
fulfillOrder =
  Stateful.Rule "fulfill_order" \w -> do
    assume (not (Map.null w.pending))
    oid <- forAll (Gen.element (Map.keys w.pending))
    let (sku, qty) = w.pending Map.! oid
    annotate ("fulfilling order #" <> renderValue oid <> ": " <> renderValue qty <> " " <> sku)
    pure
      w
        { pending = Map.delete oid w.pending,
          reserved = tally sku (negate qty) w.reserved,
          stock = tally sku (negate qty) w.stock
        }

cancelOrder :: Bug -> Stateful.Rule Warehouse IO
cancelOrder bug =
  Stateful.Rule "cancel_order" \w -> do
    assume (not (Map.null w.pending))
    oid <- forAll (Gen.element (Map.keys w.pending))
    annotate ("canceling order #" <> renderValue oid)
    -- 'Buggy' releases the reservation held by the *newest* pending order
    -- instead of the canceled one — harmless exactly when they coincide.
    let released = case bug of
          Fixed -> Map.lookup oid w.pending
          Buggy -> snd <$> Map.lookupMax w.pending
    pure case released of
      -- Unreachable: the 'assume' above guarantees a pending order.
      Nothing -> w
      Just (sku, qty) ->
        w
          { pending = Map.delete oid w.pending,
            reserved = tally sku (negate qty) w.reserved
          }

-- | The cross-structure consistency claim: the incremental cache always
-- equals the reservation totals recomputed from the order table.
reservationsMatchOrders :: Stateful.Invariant Warehouse IO
reservationsMatchOrders =
  Stateful.Invariant "reservations_match_orders" \w ->
    w.reserved === Map.filter (> 0) (Map.fromListWith (+) (Map.elems w.pending))

stockCoversReservations :: Stateful.Invariant Warehouse IO
stockCoversReservations =
  Stateful.Invariant "stock_covers_reservations" \w ->
    assert
      (and [Map.findWithDefault 0 sku w.stock >= q | (sku, q) <- Map.toList w.reserved])
      "every reservation is backed by on-hand stock"

stockNonNegative :: Stateful.Invariant Warehouse IO
stockNonNegative =
  Stateful.Invariant "stock_non_negative" \w ->
    assert (all (>= 0) w.stock) "stock never goes negative"

machine :: Bug -> Stateful.Machine Warehouse IO
machine bug =
  Stateful.Machine
    { initial =
        pure
          Warehouse
            { stock = Map.empty,
              reserved = Map.empty,
              pending = Map.empty,
              nextOrder = 1
            },
      rules = [restock, placeOrder, fulfillOrder, cancelOrder bug],
      invariants = [reservationsMatchOrders, stockCoversReservations, stockNonNegative]
    }
