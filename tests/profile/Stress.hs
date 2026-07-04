-- | Deliberately memory-pathological profiling workloads.
--
-- Where "Warehouse" mirrors the demo machine faithfully, these scenarios are
-- built to stress the allocator and the residency profile:
--
-- * 'heavyMachine' — a scaled warehouse whose rules append lazily-rendered
--   state snapshots to an in-state audit log (a classic thunk-chain leak
--   /shape/, resetting per case) and journal fat annotations every step.
-- * 'strgenChurnProperty' — the string-generator-handle worst case: every
--   draw constructs a fresh @hegel_string_generator_regex@ handle whose
--   character class depends on the previous value, so the per-'Gen'-value
--   caching can never amortize —
--   every draw pays a fresh construction, then races the GC to free the
--   handle before the next one is built.
-- * 'strgenHoardProperty' — the string-generator-handle retention case: two
--   thousand distinct regex generators held alive for the whole run (a
--   top-level CAF), so every handle is built once and retained; max
--   residency shows the cost. This is the caching layer working exactly as
--   intended — contrast with the @strgen-reclaim@ profiling probe
--   ("Main"), which checks that an /unreferenced/ handle actually gets
--   GC-reclaimed rather than pinned.
module Stress
  ( heavyMachine,
    strgenChurnProperty,
    strgenHoard,
    strgenHoardProperty,
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
    -- renderings, since 'StrictData' only forces the cons cell. Each thunk
    -- retains the ledger it snapshots until something forces it, and that
    -- retention is what the heap profile is meant to expose.
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
    -- Annotate the whole reservation table so every step carries a fat
    -- journal entry.
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

-- * String-generator churn

-- | A regex pattern matching a run of lowercase letters @a@..@<w-th letter>@,
-- for @w@ in @[1,25]@.
charClassOfWidth :: Int -> Text
charClassOfWidth w = "[a-" <> T.singleton (toEnum (fromEnum 'a' + w - 1)) <> "]+"

-- | 100 draws per case, each from a /freshly constructed/ regex generator
-- whose character-class width depends on the previous draw — the chain
-- defeats any sharing, so every draw pays a fresh
-- @hegel_string_generator_regex@ construction (and pattern validation) that
-- the per-'Gen'-value caching can never amortize. Compare against @draws@
-- (same draw count, an @int@ leaf with no handle to build at all) to price
-- the churn.
strgenChurnProperty :: Property ()
strgenChurnProperty = go (100 :: Int) 1
  where
    go :: Int -> Int -> Property ()
    go 0 _ = pure ()
    go n width = do
      x <- forAllSilent (Gen.int & Gen.min 1 & Gen.max 25 & Gen.build)
      _ <- forAllSilent (Gen.regex (charClassOfWidth width) & Gen.build)
      go (n - 1) x

-- * String-generator hoard

-- | Two thousand distinct regex generators, half plain and half built with
-- an explicit 'Gen.alphabet' (which compounds a /second/, alphabet-only
-- handle per generator — see 'Hegel.Gen.Regex.alphabet') — alive for the
-- whole run as a top-level CAF. Drawing from each forces its handle to be
-- built (on first draw only) and retained until process exit; max residency
-- and 'Hegel.Internal.DataSource.currentLiveStringGenerators' both price the
-- retention, and the alphabet half should show roughly double the handle
-- count of the plain half.
strgenHoard :: [Gen Text]
strgenHoard =
  [ let pat = charClassOfWidth (1 + (i `mod` 25))
     in if even i
          then Gen.regex pat & Gen.build
          else
            Gen.regex pat
              & Gen.alphabet (Gen.char & Gen.minCodepoint 97 & Gen.maxCodepoint 122)
              & Gen.build
  | i <- [1 .. 2_000]
  ]

-- | One draw from each generator in a 200-wide window of the hoard, at a
-- per-case offset. (Drawing all 2k in one case would overrun the engine's
-- per-case choice budget; the varying window still forces — and the CAF
-- still retains — the whole hoard across a run.)
strgenHoardProperty :: Property ()
strgenHoardProperty = do
  offset <- forAllSilent (Gen.int & Gen.min 0 & Gen.max 1800 & Gen.build)
  mapM_ (void . forAllSilent) (take 200 (drop offset strgenHoard))
