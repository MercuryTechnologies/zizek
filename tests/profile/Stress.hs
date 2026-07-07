-- | Deliberately memory-pathological profiling workloads.
--
-- Where "Warehouse" mirrors the demo machine faithfully, these scenarios are
-- built to stress the allocator and the residency profile:
--
-- * 'heavyMachine' — a scaled warehouse whose rules append lazily-rendered
--   state snapshots to an in-state audit log (a classic thunk-chain leak
--   /shape/, resetting per case) and journal fat annotations every step.
-- * 'churnProperty' — the pre-encoding worst case: every draw constructs a
--   fresh generator whose bounds depend on the previous value, so the cached
--   schema encoding can never be reused.
-- * 'hoardProperty' — the pre-encoding retention case: ten thousand distinct
--   generators held alive for the whole run (a top-level CAF), so every
--   cached encoding is forced /and retained/; max residency shows the cost.
module Stress
  ( heavyMachine,
    churnProperty,
    hoardProperty,
  )
where

import Control.Monad (void)
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Hegel (Gen)
import Hegel.Assertion (assert)
import Hegel.Gen qualified as Gen
import Hegel.Property (Property, annotate, assume, forAll, forAllSilent, (===))
import Hegel.Report (renderValue)
import Hegel.Stateful qualified as Stateful

-- * Heavy warehouse

-- | Two dozen SKUs: bigger maps for every rule and invariant to walk, and a
-- wider rendered snapshot per audit entry.
skus :: [Text]
skus = [T.pack ("sku-" <> show i) | i <- [1 :: Int .. 24]]

data Ledger = Ledger
  { stock :: Map Text Int,
    reserved :: Map Text Int,
    pending :: Map Int (Text, Int),
    nextOrder :: Int,
    -- | Grows every step within a case, then resets with the next case's
    -- 'Stateful.Machine.initial'. The entries are deliberately /lazy/
    -- renderings ('StrictData' only forces the cons cell): each thunk
    -- retains the ledger it snapshots until something forces it, giving the
    -- heap profile a sawtooth worth of drag to expose.
    audit :: [Text]
  }
  deriving stock (Show)

-- | Adjust a per-SKU tally, dropping entries at (or below) zero.
tally :: Text -> Int -> Map Text Int -> Map Text Int
tally sku dq = Map.filter (> 0) . Map.insertWith (+) sku dq

-- | Render the interesting parts of the ledger. A few hundred bytes of Text
-- per call at steady state.
snapshot :: Ledger -> Text
snapshot w =
  "stock=" <> renderValue (Map.toList w.stock) <> " reserved=" <> renderValue (Map.toList w.reserved)

-- | Append a (lazy) snapshot to the audit log.
audited :: Text -> Ledger -> Ledger
audited tag w = w {audit = (tag <> ": " <> snapshot w) : w.audit}

restock :: Stateful.Rule Ledger IO
restock =
  Stateful.Rule "restock" \w -> do
    sku <- forAll (Gen.element skus)
    qty <- forAll (Gen.int & Gen.min 50 & Gen.max 100 & Gen.build)
    pure (audited "restock" w {stock = tally sku qty w.stock})

placeOrder :: Stateful.Rule Ledger IO
placeOrder =
  Stateful.Rule "place_order" \w -> do
    sku <- forAll (Gen.element skus)
    qty <- forAll (Gen.int & Gen.min 1 & Gen.max 10 & Gen.build)
    let available =
          Map.findWithDefault 0 sku w.stock - Map.findWithDefault 0 sku w.reserved
    assume (qty <= available)
    -- Fat journal entry: the whole reservation table, every step.
    annotate ("reserved now: " <> renderValue (Map.toList w.reserved))
    pure
      ( audited
          "place"
          w
            { pending = Map.insert w.nextOrder (sku, qty) w.pending,
              reserved = tally sku qty w.reserved,
              nextOrder = w.nextOrder + 1
            }
      )

fulfillOrder :: Stateful.Rule Ledger IO
fulfillOrder =
  Stateful.Rule "fulfill_order" \w -> do
    assume (not (Map.null w.pending))
    oid <- forAll (Gen.element (Map.keys w.pending))
    let (sku, qty) = w.pending Map.! oid
    annotate ("fulfilling order #" <> renderValue oid <> ": " <> renderValue qty <> " " <> sku)
    pure
      ( audited
          "fulfill"
          w
            { pending = Map.delete oid w.pending,
              reserved = tally sku (negate qty) w.reserved,
              stock = tally sku (negate qty) w.stock
            }
      )

cancelOrder :: Stateful.Rule Ledger IO
cancelOrder =
  Stateful.Rule "cancel_order" \w -> do
    assume (not (Map.null w.pending))
    oid <- forAll (Gen.element (Map.keys w.pending))
    annotate ("canceling order #" <> renderValue oid)
    pure case Map.lookup oid w.pending of
      -- Unreachable: the 'assume' above guarantees a pending order.
      Nothing -> w
      Just (sku, qty) ->
        audited
          "cancel"
          w
            { pending = Map.delete oid w.pending,
              reserved = tally sku (negate qty) w.reserved
            }

reservationsMatchOrders :: Stateful.Invariant Ledger IO
reservationsMatchOrders =
  Stateful.Invariant "reservations_match_orders" \w ->
    w.reserved === Map.filter (> 0) (Map.fromListWith (+) (Map.elems w.pending))

stockCoversReservations :: Stateful.Invariant Ledger IO
stockCoversReservations =
  Stateful.Invariant "stock_covers_reservations" \w ->
    assert
      (and [Map.findWithDefault 0 sku w.stock >= q | (sku, q) <- Map.toList w.reserved])
      "every reservation is backed by on-hand stock"

-- | Forces the audit spine (but not its entries) every step, so the log
-- can't be optimized away while its thunks still accumulate.
auditNeverForgets :: Stateful.Invariant Ledger IO
auditNeverForgets =
  Stateful.Invariant "audit_never_forgets" \w ->
    assert (length w.audit >= 0) "audit log is well-formed"

-- | Passing (bug-free) machine; the pathology is memory, not search.
heavyMachine :: Stateful.Machine Ledger IO
heavyMachine =
  Stateful.Machine
    { initial =
        pure
          Ledger
            { stock = Map.empty,
              reserved = Map.empty,
              pending = Map.empty,
              nextOrder = 1,
              audit = []
            },
      rules = [restock, placeOrder, fulfillOrder, cancelOrder],
      invariants = [reservationsMatchOrders, stockCoversReservations, auditNeverForgets]
    }

-- * Generator churn

-- | 100 draws per case, each from a /freshly constructed/ generator whose
-- upper bound depends on the previous draw — the bound chain defeats any
-- sharing, so every draw pays generator construction plus the
-- encode-at-construction that pre-encoding moved there. Compare against
-- @scalars@ (same draw count, fully cached) to price the churn.
churnProperty :: Property ()
churnProperty = go (100 :: Int) 1000
  where
    go 0 _ = pure ()
    go n hi = do
      x <- forAllSilent (Gen.int & Gen.min 0 & Gen.max (Prelude.max 1 hi) & Gen.build)
      go (n - 1) (x + 1)

-- * Generator hoard

-- | Ten thousand distinct basic generators, alive for the whole run as a
-- top-level CAF. Drawing from each forces its cached 'encoded' schema, which
-- is then retained until process exit — max residency directly prices the
-- pre-encoding's retention overhead (schema 'Value' + encoded bytes per
-- generator).
hoard :: [Gen Int]
hoard = [Gen.int & Gen.min 0 & Gen.max hi & Gen.build | hi <- [1 .. 10_000]]

-- | One draw from each generator in a 1000-wide window of the hoard, at a
-- per-case offset. (Drawing all 10k in one case overruns the engine's
-- per-case choice budget; the varying window still forces — and the CAF
-- still retains — the whole hoard across a run.)
hoardProperty :: Property ()
hoardProperty = do
  offset <- forAllSilent (Gen.int & Gen.min 0 & Gen.max 9000 & Gen.build)
  mapM_ (void . forAllSilent) (take 1000 (drop offset hoard))
