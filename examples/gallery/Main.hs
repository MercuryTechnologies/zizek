-- | A gallery of deliberately-failing properties: the permanent eyeball
-- harness for the failure renderers. Five scenarios span the whole spectrum
-- of report forms, each earning its place:
--
--   1. plain property — the non-stateful base case: drawn values splice into
--      their source and a '(===)' failure carries a structural diff, with no
--      step timeline, spine, or footer
--   2. stack palindrome — the smallest /stateful/ spliced report: a multi-rule
--      '(===)' failure with a structural diff in-band
--   3. warehouse — the realistic spliced report: four rules over three
--      coupled structures, cross-structure invariants, a minimal
--      counterexample that interleaves three distinct rules (the decisive
--      scenario of the Timeline-layout evaluation)
--   4. file handles — the flagship citation spine: a use-after-close bug
--      whose /minimal/ counterexample needs an unrelated open between the
--      write and the close, so the spine shows the full vocabulary — a
--      three-edge link, an elision row, a lineage-linked lifeline across
--      two pools ('Pool.transfer'), named values, and an elided-lifeline
--      footer. Printed in unicode and ascii, beside the unicode report.
--   5. poked value — a flat story (born, then poked): no death or handoff,
--      so it degrades to the step timeline with a compact lead
--   6. ledger — a /multi-subject/ failure: the settling step consumes two
--      distinct funded accounts, so the failure is caused by two pool values
--      at once. Blame surfaces one value; the chronological timeline shows both
--      accounts' activity. A standing fixture for multi-value failures.
--
-- Run with @just gallery@ from the repo root (source
-- splicing resolves @srcLocFile@ relative to the working directory).
-- Every scenario renders through 'renderReportRichAnsi', the same path real
-- failures take: scenario 4's handoff earns the composed trace report — the
-- chronological citation spine (oldest step to the failing step), then that
-- step's source splice, then the off-spine lifelines; scenario 5's flat story
-- degrades to the step timeline with a lead.
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
import Hegel.Property (Property, annotate, assume, forAll, (===))
import Hegel.Report (Report (..), renderReportRichAnsi, renderReportRichAnsiWith, renderValue)
import Hegel.Report.Glyph qualified as Glyph
import Hegel.Report.Style (defaultStyle)
import Hegel.Property.Internal (Env (..), askEnv)
import Hegel.Runner (check)
import Hegel.Settings (defaultSettings)
import Hegel.Stateful qualified as Stateful
import UnliftIO.IORef (IORef, modifyIORef', newIORef, readIORef)

main :: IO ()
main = do
  runScenario "1: plain property — drawn values spliced, === diff" plainProperty
  runScenario "2: stack palindrome — === diff, spliced" (Stateful.run palindromeMachine)
  runScenario "3: warehouse — realistic interleaving, spliced" (Stateful.run warehouseMachine)
  runTraceScenario True "4: file handles — use-after-close, citation spine" (Stateful.run fileHandleMachine)
  runTraceScenario False "5: poked value — flat story, degrades to lead" (Stateful.run overflowMachine)
  runTraceScenario False "6: ledger — two accounts settled (multi-subject candidate)" (Stateful.run ledgerMachine)

runScenario :: Text -> Property () -> IO ()
runScenario title prop = showReport title =<< check defaultSettings prop

-- | Print one report through the wired rich ANSI renderer.
showReport :: Text -> Report -> IO ()
showReport title report = do
  T.putStrLn ("\n━━━━━ scenario " <> title <> " ━━━━━")
  T.putStrLn =<< renderReportRichAnsi report

-- | The trace scenarios through the /wired/ path — 'renderReportRichAnsi'
-- composes the citation spine, the failing step's splice, and the footer
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
-- no step timeline, spine, or footer.
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
    oid <- forAll (Gen.element (Map.keys w.pending))
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
    oid <- forAll (Gen.element (Map.keys w.pending))
    annotate ("cancelling order #" <> renderValue oid)
    -- BUG: releases the reservation held by the *newest* pending order
    -- instead of the cancelled one — harmless exactly when they coincide.
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

-- * Scenario 4: file handles (citation spine, flagship)

-- | Handles live in the @handle@ pool while open and 'Pool.transfer' into the
-- @closed@ pool on close, so the report keeps one lifeline across both.
--
-- The SUT bug: close drops the handle's buffered content — /unless/ the
-- handle table was resized (another handle was opened) since that handle's
-- last write, in which case the content leaks and a later read of the
-- closed handle returns stale bytes. The resize is what makes the minimal
-- counterexample visually rich: it must keep an unrelated @open@ between
-- the write and the close (an elision row and a second lifeline that
-- shrinking cannot remove), plus the full born\/accessed\/consumed link.
data FileModel = FileModel
  { openHandles :: Pool Int,
    closedHandles :: Pool Int,
    nextHandle :: IORef Int,
    -- | SUT: buffered content per handle.
    contents :: IORef (Map Int Text),
    -- | SUT: table size ("epoch") at each handle's last write.
    lastWriteEpoch :: IORef (Map Int Int),
    -- | SUT: grows on every open.
    epoch :: IORef Int
  }

fileHandleMachine :: Stateful.Machine FileModel IO
fileHandleMachine =
  Stateful.Machine
    { initial = do
        env <- askEnv
        openHandles <- liftIO (Pool.named "handle" env.testCase)
        closedHandles <- liftIO (Pool.named "closed" env.testCase)
        nextHandle <- newIORef 0
        contents <- newIORef Map.empty
        lastWriteEpoch <- newIORef Map.empty
        epoch <- newIORef 0
        pure FileModel {openHandles, closedHandles, nextHandle, contents, lastWriteEpoch, epoch},
      rules =
        [ Stateful.Rule "open" \m -> do
            h <- liftIO do
              h <- readIORef m.nextHandle
              modifyIORef' m.nextHandle (+ 1)
              modifyIORef' m.epoch (+ 1)
              pure h
            liftIO (Pool.add m.openHandles h)
            pure m,
          Stateful.Rule "write" \m -> do
            h <- forAll (Pool.valuesReusable m.openHandles)
            v <- forAll (Gen.text & Gen.minSize 1 & Gen.maxSize 4 & Gen.build)
            liftIO do
              e <- readIORef m.epoch
              modifyIORef' m.contents (Map.insert h v)
              modifyIORef' m.lastWriteEpoch (Map.insert h e)
            pure m,
          Stateful.Rule "close" \m -> do
            h <- forAll (Pool.transfer m.openHandles m.closedHandles)
            liftIO do
              e <- readIORef m.epoch
              wrote <- Map.lookup h <$> readIORef m.lastWriteEpoch
              -- BUG: skips the cleanup when the table was resized since the
              -- handle's last write.
              case wrote of
                Just we | e > we -> pure () -- leak!
                _ -> modifyIORef' m.contents (Map.delete h)
            pure m,
          Stateful.Rule "read_closed" \m -> do
            h <- forAll (Pool.valuesReusable m.closedHandles)
            r <- liftIO (Map.lookup h <$> readIORef m.contents)
            Stateful.respondShow r
            r === Nothing
            pure m
        ],
      invariants = []
    }

-- * Scenario 5: poked value (link overflow)

-- | More citations than the link budget (numeric fallback) plus a second
-- lifeline that must survive shrinking (the elided-lifelines footer). The
-- subject is registered in @machine.initial@, so its birth lands in the
-- prelude step.
data OverflowModel = OverflowModel
  { poked :: Pool Text,
    decoys :: Pool Int,
    pokes :: Int,
    decoyed :: Bool,
    lastWasPoke :: Bool
  }

overflowMachine :: Stateful.Machine OverflowModel IO
overflowMachine =
  Stateful.Machine
    { initial = do
        env <- askEnv
        poked <- liftIO (Pool.new env.testCase)
        decoys <- liftIO (Pool.named "decoy" env.testCase)
        liftIO (Pool.add poked "the-value")
        pure OverflowModel {poked, decoys, pokes = 0, decoyed = False, lastWasPoke = False},
      rules =
        [ Stateful.Rule "poke" \m -> do
            _ <- forAll (Pool.valuesReusable m.poked)
            pure m {pokes = m.pokes + 1, lastWasPoke = True},
          Stateful.Rule "decoy" \m -> do
            n <- forAll (Gen.int & Gen.min 0 & Gen.max 9 & Gen.build)
            liftIO (Pool.add m.decoys n)
            pure m {decoyed = True, lastWasPoke = False}
        ],
      invariants =
        [ -- The 'lastWasPoke' conjunct pins the failing step to a poke of
          -- the subject (rather than the decoy), so the failure cites the
          -- subject's full touch history — more than the link budget.
          Stateful.Invariant "few_pokes" \m ->
            assert
              (not (m.pokes >= 5 && m.decoyed && m.lastWasPoke))
              "at most four pokes once a decoy exists"
        ]
    }

-- * Scenario 6: ledger (multi-subject blame candidate)

-- | The failing step touches /two/ distinct pool values, each with its own
-- deposit history: @settle@ consumes two funded accounts and trips a claim
-- about both. Today blame keeps one account for the spine and relegates the
-- other to a footer lead; once multi-subject blame lands, both operands' stories
-- should weave into one report. A permanent before/after fixture for that work
-- (see @notes/roadmap.md@).
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
        env <- askEnv
        accounts <- liftIO (Pool.named "account" env.testCase)
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
          Stateful.Rule "deposit" \m -> do
            acc <- forAll (Pool.valuesReusable m.accounts)
            amt <- forAll (Gen.int & Gen.min 1 & Gen.max 9 & Gen.build)
            liftIO (modifyIORef' m.balances (Map.adjust (+ amt) acc))
            pure m,
          -- Consumes two distinct accounts (a consuming draw removes the first,
          -- so the second is necessarily different), giving the failing step two
          -- pool subjects.
          Stateful.Rule "settle" \m -> do
            a <- forAll (Pool.valuesConsumed m.accounts)
            b <- forAll (Pool.valuesConsumed m.accounts)
            bals <- liftIO (readIORef m.balances)
            let funded acc = Map.findWithDefault 0 acc bals > 0
            assert (not (funded a && funded b)) "funds stay consolidated in one account"
            pure m
        ],
      invariants = []
    }
