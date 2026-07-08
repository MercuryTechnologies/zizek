-- | A gallery of deliberately-failing properties: the permanent eyeball
-- harness for the failure renderers. Five scenarios span the spectrum of report
-- shapes, each earning its place. Every stateful failure renders as the one
-- chronological event log (oldest step to the failing step, then that step's
-- source splice); the log has two views — 'Focused' on a single pool value
-- (others elided) and 'Unfocused' with every step shown:
--
--   1. plain property — the non-stateful base case: drawn values splice into
--      their source and a '(===)' failure carries a structural diff, with no
--      event log or footer
--   2. stack palindrome — the smallest /stateful/ spliced report: a multi-rule
--      '(===)' failure with a structural diff in-band, no pool values, so an
--      unfocused log (compact call rows) leads the splice
--   3. warehouse — the realistic no-pool report: four rules over three
--      coupled structures, cross-structure invariants, a minimal
--      counterexample that interleaves three distinct rules — an unfocused log
--      with annotation detail rows and 'forAllWithLabel'-labeled draws
--      (@restock item="apple" qty=5@)
--   4. connection pool — the flagship focused lifeline: a pooled connection
--      threads idle → active → in-tx → active → idle over its life, so one
--      value's story crosses four pool boundaries ('Pool.transfer' each). A
--      use-after-checkin leak whose /minimal/ counterexample needs an unrelated
--      connect between begin_tx and commit shows the full vocabulary — lifecycle
--      gutters (each 'Pool.transfer' a @◉@ handoff), an elision row (naming the
--      unrelated @conn₂@ it hides), a long lineage across four pools, named
--      values, and an elided-lifeline footer. Printed in unicode and ascii,
--      beside the unicode report.
--   5. ledger — a /multi-value/ failure: the settling step consumes two
--      distinct funded accounts, so the failing step touches two lineage roots
--      and the log stays unfocused — every step shown with per-step lifecycle
--      glyphs. Multi-subject blame cites both accounts on a @↳@ row — an explicit
--      subset, since the load-bearing @accrue@ step touches no pool value and is
--      shown but uncited.
--
-- Run with @just gallery@ from the repo root (source splicing resolves
-- @srcLocFile@ relative to the working directory). Every scenario renders
-- through 'renderReportRichAnsi', the same path real failures take.
--
-- Always exits 0; this is an eyeballing harness, not an assertion.
module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.IO qualified as T
import Hegel.Assertion (assert)
import Hegel.Gen qualified as Gen
import Hegel.Pool (Pool)
import Hegel.Pool qualified as Pool
import Hegel.Property (Property, annotate, assume, forAll, forAllWithLabel, (===))
import Hegel.Report (Report (..), renderReportRichAnsi, renderReportRichAnsiWith, renderValue)
import Hegel.Report.Glyph qualified as Glyph
import Hegel.Report.Style (defaultStyle)
import Hegel.Runner (check)
import Hegel.Settings (defaultSettings)
import Hegel.Stateful qualified as Stateful
import UnliftIO.IORef (IORef, modifyIORef', newIORef, readIORef)

main :: IO ()
main = do
  runScenario "1: plain property — drawn values spliced, === diff" plainProperty
  runScenario "2: stack palindrome — === diff, spliced" (Stateful.run palindromeMachine)
  runScenario "3: warehouse — realistic interleaving, spliced" (Stateful.run warehouseMachine)
  runTraceScenario True "4: connection pool — use-after-checkin, focused lifeline" (Stateful.run connectionMachine)
  runTraceScenario False "5: ledger — two accounts settled, unfocused log" (Stateful.run ledgerMachine)

runScenario :: Text -> Property () -> IO ()
runScenario title prop = showReport title =<< check defaultSettings prop

-- | Print one report through the wired rich ANSI renderer.
showReport :: Text -> Report -> IO ()
showReport title report = do
  T.putStrLn ("\n━━━━━ scenario " <> title <> " ━━━━━")
  T.putStrLn =<< renderReportRichAnsi report

-- | The trace scenarios through the /wired/ path — 'renderReportRichAnsi'
-- composes the event log, the failing step's splice, and the footer
-- itself — plus the ascii table via the options variant.
runTraceScenario :: Bool -> Text -> Property () -> IO ()
runTraceScenario withAscii title prop = do
  report <- check defaultSettings prop
  showReport title report
  if withAscii
    then do
      T.putStrLn "── ascii ──"
      T.putStrLn . Glyph.sevenBitClean
        =<< renderReportRichAnsiWith (defaultStyle Glyph.ascii) report
    else pure ()

-- | Naive count-with-noun pluralization for demo messages:
-- @pluralize 1 "apple" = "1 apple"@, @pluralize 3 "apple" = "3 apples"@.
pluralize :: Int -> Text -> Text
pluralize 1 noun = "1 " <> noun
pluralize n noun = renderValue n <> " " <> noun <> "s"

-- * Scenario 1: a plain property

-- | The non-stateful base case: two draws and a false claim (subtraction does
-- not commute), so the report is just the spliced draws and the '(===)' diff —
-- no event log or footer.
plainProperty :: Property ()
plainProperty = do
  a <- forAll (Gen.int & Gen.min 0 & Gen.max 9 & Gen.build)
  b <- forAll (Gen.int & Gen.min 0 & Gen.max 9 & Gen.build)
  a - b === b - a

-- * Scenario 2: stack palindrome

-- | A stack of small integers, with a parseable 'Show' so '(===)' produces a
-- structural diff.
newtype Stack = Stack [Int]
  deriving stock (Eq, Show)

-- | Push a drawn value onto the stack.
push :: Stateful.Rule Stack IO
push =
  Stateful.Rule "push" \(Stack xs) -> do
    n <- forAll (Gen.int & Gen.min 0 & Gen.max 9 & Gen.build)
    pure (Stack (n : xs))

-- | A deliberately false claim: that the stack always reads the same
-- forwards and backwards. Fails via '(===)' as soon as two distinct values
-- have been pushed.
checkPalindrome :: Stateful.Rule Stack IO
checkPalindrome =
  Stateful.Rule "check_palindrome" \s@(Stack xs) -> do
    Stack xs === Stack (reverse xs)
    pure s

palindromeMachine :: Stateful.Machine Stack IO
palindromeMachine =
  Stateful.Machine
    { initial = pure (Stack []),
      rules = [push, checkPalindrome],
      invariants = []
    }

-- * Scenario 3: warehouse

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
    sku <- forAllWithLabel "item" (Gen.element skus)
    qty <- forAllWithLabel "qty" (Gen.int & Gen.min 5 & Gen.max 10 & Gen.build)
    pure w {stock = tally sku qty w.stock}

-- | Reserve stock for a new order; only as much as is unreserved.
placeOrder :: Stateful.Rule Warehouse IO
placeOrder =
  Stateful.Rule "place_order" \w -> do
    sku <- forAllWithLabel "item" (Gen.element skus)
    qty <- forAllWithLabel "qty" (Gen.int & Gen.min 1 & Gen.max 3 & Gen.build)
    let available =
          Map.findWithDefault 0 sku w.stock - Map.findWithDefault 0 sku w.reserved
    assume (qty <= available)
    annotate ("order #" <> renderValue w.nextOrder <> " reserves " <> pluralize qty sku)
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
    oid <- forAllWithLabel "order" (Gen.element (Map.keys w.pending))
    let (sku, qty) = w.pending Map.! oid
    annotate ("fulfilling order #" <> renderValue oid <> ": " <> pluralize qty sku)
    pure
      w
        { pending = Map.delete oid w.pending,
          reserved = tally sku (negate qty) w.reserved,
          stock = tally sku (negate qty) w.stock
        }

cancelOrder :: Stateful.Rule Warehouse IO
cancelOrder =
  Stateful.Rule "cancel_order" \w -> do
    assume (not (Map.null w.pending))
    oid <- forAllWithLabel "order" (Gen.element (Map.keys w.pending))
    annotate ("canceling order #" <> renderValue oid)
    -- BUG: releases the reservation held by the *newest* pending order
    -- instead of the canceled one — harmless exactly when they coincide.
    pure case Map.lookupMax w.pending of
      -- Unreachable: the 'assume' above guarantees a pending order.
      Nothing -> w
      Just (_, (sku, qty)) ->
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

warehouseMachine :: Stateful.Machine Warehouse IO
warehouseMachine =
  Stateful.Machine
    { initial =
        pure
          Warehouse
            { stock = Map.empty,
              reserved = Map.empty,
              pending = Map.empty,
              nextOrder = 1
            },
      rules = [restock, placeOrder, fulfillOrder, cancelOrder],
      invariants = [reservationsMatchOrders, stockCoversReservations, stockNonNegative]
    }

-- * Scenario 4: connection pool (focused lifeline, flagship)

-- | A pooled connection threads through several pools over its life — @idle@
-- when available, @active@ when checked out, @in-tx@ mid-transaction — so one
-- value's lifeline crosses four pool boundaries ('Pool.transfer' each): the
-- richest focused story the report renders.
--
-- The SUT bug: @commit@ clears the connection's open-transaction flag —
-- /unless/ the pool grew (another connection opened) since @begin_tx@, in which
-- case the clear is skipped and the transaction leaks. A later @query@ of the
-- now-idle, checked-in connection then sees the stale transaction and errors.
-- The resize forces an unrelated @connect@ between @begin_tx@ and @commit@ (an
-- elision row and a second lifeline shrinking cannot remove); the leak forces
-- the whole checkout → begin_tx → commit → checkin chain onto the subject's
-- lifeline, so the report shows a value crossing every pool boundary.
data ConnModel = ConnModel
  { idle :: Pool Int,
    active :: Pool Int,
    inTx :: Pool Int,
    nextConn :: IORef Int,
    -- | SUT: grows on every connect (the pool table "epoch").
    epoch :: IORef Int,
    -- | SUT: the epoch at each connection's begin_tx.
    txEpoch :: IORef (Map Int Int),
    -- | SUT: does the connection have an uncommitted transaction?
    txOpen :: IORef (Map Int Bool)
  }

connectionMachine :: Stateful.Machine ConnModel IO
connectionMachine =
  Stateful.Machine
    { initial = do
        idle <- Pool.named "conn"
        active <- Pool.new
        inTx <- Pool.new
        nextConn <- newIORef 0
        epoch <- newIORef 0
        txEpoch <- newIORef Map.empty
        txOpen <- newIORef Map.empty
        pure ConnModel {idle, active, inTx, nextConn, epoch, txEpoch, txOpen},
      rules =
        [ Stateful.Rule "connect" \m -> do
            liftIO do
              c <- readIORef m.nextConn
              modifyIORef' m.nextConn (+ 1)
              modifyIORef' m.epoch (+ 1)
              Pool.add m.idle c
            pure m,
          Stateful.Rule "checkout" \m -> do
            _ <- forAll (Pool.transfer m.idle m.active)
            pure m,
          Stateful.Rule "begin_tx" \m -> do
            c <- forAll (Pool.transfer m.active m.inTx)
            liftIO do
              e <- readIORef m.epoch
              modifyIORef' m.txEpoch (Map.insert c e)
              modifyIORef' m.txOpen (Map.insert c True)
            pure m,
          Stateful.Rule "commit" \m -> do
            c <- forAll (Pool.transfer m.inTx m.active)
            liftIO do
              e <- readIORef m.epoch
              began <- Map.findWithDefault 0 c <$> readIORef m.txEpoch
              -- BUG: only clears the transaction when the table hasn't grown
              -- since begin_tx; a resize makes commit silently leak it.
              if e > began
                then pure () -- leak!
                else modifyIORef' m.txOpen (Map.insert c False)
            pure m,
          Stateful.Rule "checkin" \m -> do
            _ <- forAll (Pool.transfer m.active m.idle)
            pure m,
          Stateful.Rule "query" \m -> do
            c <- forAll (Pool.reuse m.idle)
            open <- liftIO (Map.findWithDefault False c <$> readIORef m.txOpen)
            Stateful.respondShow open
            assert (not open) "a checked-in connection has no open transaction"
            pure m
        ],
      invariants = []
    }

-- * Scenario 5: ledger (multi-value, unfocused log)

-- | The failing step touches /two/ distinct pool values: @settle@ consumes two
-- funded accounts and trips a claim about both. Two lineage roots at the failing
-- step keep the log 'Unfocused' — every step shown with per-step lifecycle
-- glyphs. Multi-subject blame cites /both/ accounts on a @↳ cites@ row below the
-- failing step. Because @accrue@ (the funding step) touches no pool value, it is
-- load-bearing but shown-and-uncited, so the citation is an explicit subset
-- (@↳ cites \@1, \@2@) — the row that a fully-cited failure would drop. A
-- standing fixture for multi-value failures.
--
-- The claim is deliberately false — nothing in the model stops two accounts from
-- holding funds at once — in the spirit of scenario 2's palindrome.
data LedgerModel = LedgerModel
  { accounts :: Pool Int,
    balances :: IORef (Map Int Int),
    nextAccount :: IORef Int
  }

ledgerMachine :: Stateful.Machine LedgerModel IO
ledgerMachine =
  Stateful.Machine
    { initial = do
        accounts <- Pool.named "account"
        balances <- newIORef Map.empty
        nextAccount <- newIORef 0
        pure LedgerModel {accounts, balances, nextAccount},
      rules =
        [ Stateful.Rule "open" \m -> do
            acc <- liftIO do
              n <- readIORef m.nextAccount
              modifyIORef' m.nextAccount (+ 1)
              modifyIORef' m.balances (Map.insert n 0)
              pure n
            liftIO (Pool.add m.accounts acc)
            pure m,
          -- Posts interest to every account at once. It draws nothing from the
          -- pool, so this step touches no pool value: a blank gutter, and the
          -- failing settle never cites it — even though it is load-bearing (with
          -- no accrual no account is funded, and settle passes). That is what
          -- makes the citation an explicit subset (@↳ cites \@1, \@2@); a failure
          -- whose every shown step is cited drops the row entirely.
          Stateful.Rule "accrue" \m -> do
            liftIO (modifyIORef' m.balances (Map.map (+ 1)))
            pure m,
          -- Consumes two distinct accounts (a consuming draw removes the first,
          -- so the second is necessarily different), giving the failing step two
          -- pool subjects.
          Stateful.Rule "settle" \m -> do
            a <- forAll (Pool.consume m.accounts)
            b <- forAll (Pool.consume m.accounts)
            bals <- liftIO (readIORef m.balances)
            let funded acc = Map.findWithDefault 0 acc bals > 0
            assert (not (funded a && funded b)) "funds stay consolidated in one account"
            pure m
        ],
      invariants = []
    }
