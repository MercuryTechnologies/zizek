-- | A gallery of deliberately-failing stateful machines rendered through the
-- wired-in rich ANSI renderer ('renderReportRichAnsi') — the report a user
-- actually sees on failure. Each scenario stresses one rendering decision.
--
-- Run with @cabal run stateful-report-gallery@ from the repo root (source
-- splicing resolves @srcLocFile@ relative to the working directory).
--
-- The scenarios exercise the rendering decisions:
--
--   1. multi-rule machine failing via '(===)' — structural diff in-band
--   2. failing 'Stateful.Invariant' — the assertion lives outside any rule
--   3. rule with two draws and an 'annotate' — several notes per step
--   4. long counterexample of identical steps — repetition on the spine
--   5. machine defined inline in one large binding — every splice lands in
--      the same (big) enclosing declaration
--   6. synthetic journal mixing a real location with an unreadable one —
--      per-step fallback mixing
--   7. warehouse with interconnected state — four rules over three coupled
--      structures (stock, a denormalized reservation cache, and a pending
--      order table), cross-structure consistency invariants, and a bug
--      whose minimal counterexample interleaves three distinct rules; the
--      least contrived exercise of the rich renderer on a realistic
--      failure
--
-- Always exits 0; this is an eyeballing harness, not an assertion.
module Main (main) where

import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.IO qualified as T
import GHC.Stack (HasCallStack, SrcLoc (..), callStack, getCallStack)
import Hegel.Assertion (assert)
import Hegel.Gen qualified as Gen
import Hegel.Property (Property, annotate, assume, forAll, (===))
import Hegel.Report
  ( Clock (..),
    Note (..),
    NoteKind (..),
    Report (..),
    Result (..),
    Stats (..),
    renderReportRichAnsi,
    renderValue,
  )
import Hegel.Runner (check)
import Hegel.Settings (defaultSettings)
import Hegel.Stateful qualified as Stateful

main :: IO ()
main = do
  runScenario "1: multi-rule === diff (stack palindrome)" (Stateful.run palindromeMachine)
  runScenario "2: failing invariant assert (counter)" (Stateful.run counterMachine)
  runScenario "3: two draws and an annotation per step (transfer)" (Stateful.run transferMachine)
  runScenario "4: long run of identical steps (ticker)" (Stateful.run tickerMachine)
  runScenario "5: machine defined inline in one large binding" (Stateful.run inlineMachine)
  showReport "6: synthetic mixed-loc journal (degrade mixing)" syntheticReport
  runScenario "7: interconnected state, cross-structure invariants (warehouse)" (Stateful.run warehouseMachine)

runScenario :: Text -> Property () -> IO ()
runScenario title prop = showReport title =<< check defaultSettings prop

-- | Print one report through the wired rich ANSI renderer.
showReport :: Text -> Report -> IO ()
showReport title report = do
  T.putStrLn ("\n━━━━━ scenario " <> title <> " ━━━━━")
  T.putStrLn =<< renderReportRichAnsi report

-- | Naive count-with-noun pluralization for demo messages:
-- @pluralize 1 "apple" = "1 apple"@, @pluralize 3 "apple" = "3 apples"@.
pluralize :: Int -> Text -> Text
pluralize 1 noun = "1 " <> noun
pluralize n noun = renderValue n <> " " <> noun <> "s"

-- * Scenario 1: multi-rule machine failing via '(===)'

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

-- * Scenario 2: failing invariant

-- | Bump the counter by a drawn step size.
increment :: Stateful.Rule Int IO
increment =
  Stateful.Rule "increment" \n -> do
    delta <- forAll (Gen.int & Gen.min 1 & Gen.max 3 & Gen.build)
    pure (n + delta)

-- | Fails once a couple of increments accumulate; the assertion call site is
-- in this declaration, not in any rule.
staysSmall :: Stateful.Invariant Int IO
staysSmall =
  Stateful.Invariant "stays_small" \n ->
    assert (n < 5) "counter stays below 5"

counterMachine :: Stateful.Machine Int IO
counterMachine =
  Stateful.Machine
    { initial = pure 0,
      rules = [increment],
      invariants = [staysSmall]
    }

-- * Scenario 3: several notes per step

-- | Move a drawn amount between two accounts, losing a drawn fee — so the
-- conservation check below eventually fails.
transfer :: Stateful.Rule (Int, Int) IO
transfer =
  Stateful.Rule "transfer" \(a, b) -> do
    amount <- forAll (Gen.int & Gen.min 1 & Gen.max 10 & Gen.build)
    fee <- forAll (Gen.int & Gen.min 0 & Gen.max 2 & Gen.build)
    annotate ("moving " <> renderValue amount <> " with fee " <> renderValue fee)
    pure (a - amount, b + amount - fee)

-- | Total balance is conserved — false whenever a nonzero fee was drawn.
conserves :: Stateful.Invariant (Int, Int) IO
conserves =
  Stateful.Invariant "conserves_total" \(a, b) ->
    a + b === 100

transferMachine :: Stateful.Machine (Int, Int) IO
transferMachine =
  Stateful.Machine
    { initial = pure (100, 0),
      rules = [transfer],
      invariants = [conserves]
    }

-- * Scenario 4: long counterexample of identical steps

-- | Draw-free rule: every step is textually identical in the journal.
tick :: Stateful.Rule Int IO
tick = Stateful.Rule "tick" \n -> pure (n + 1)

-- | Only violated after seven ticks, forcing a long minimal counterexample.
staysUnderSeven :: Stateful.Invariant Int IO
staysUnderSeven =
  Stateful.Invariant "stays_under_seven" \n ->
    assert (n < 7) ("ticked " <> pluralize n "time" <> ", expected fewer than 7")

tickerMachine :: Stateful.Machine Int IO
tickerMachine =
  Stateful.Machine
    { initial = pure 0,
      rules = [tick],
      invariants = [staysUnderSeven]
    }

-- * Scenario 5: machine defined inline in one large binding

-- | Rules, invariant, and machine record all inline: declaration discovery
-- attributes every splice to this single (large) declaration, stressing
-- context trimming.
inlineMachine :: Stateful.Machine Int IO
inlineMachine =
  Stateful.Machine
    { initial = pure 0,
      rules =
        [ Stateful.Rule "add" \n -> do
            d <- forAll (Gen.int & Gen.min 1 & Gen.max 5 & Gen.build)
            pure (n + d)
        ],
      invariants =
        [ Stateful.Invariant "stays_under_eight" \n ->
            assert (n < 8) "inline counter stays under 8"
        ]
    }

-- * Scenario 7: warehouse with interconnected state

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

-- * Scenario 6: synthetic journal with mixed locations

-- | The 'SrcLoc' of the call site — a real location in this file, so the
-- notes that carry it can splice.
hereLoc :: (HasCallStack) => SrcLoc
hereLoc = case getCallStack callStack of
  (_, l) : _ -> l
  [] -> error "hereLoc: empty call stack"

-- | A hand-built counterexample: step 1's draw and the failure carry a real
-- location in this declaration; step 2's draw points at a file that does not
-- exist, so it must fall back to its structured line.
syntheticReport :: Report
syntheticReport = Report {result, stats = Stats {valid = 7, invalid = 0}}
  where
    realLoc = hereLoc -- scenario 6 splices this line
    fakeLoc = SrcLoc "zizek" "Main" "no/such/file.hs" 1 1 1 9
    note kind text loc depth = Note {kind, text, loc, depth, clock = Clock 0}
    result =
      Counterexample
        { message = "synthetic failure",
          notes =
            [ note Annotation "Step 1: real_loc" Nothing 0,
              note Drawn "42" (Just realLoc) 1,
              note Annotation "Step 2: fake_loc" Nothing 0,
              note Drawn "\"unspliceable\"" (Just fakeLoc) 1,
              note (Failure Nothing) "synthetic failure" (Just realLoc) 1
            ],
          events = [],
          loc = Just realLoc,
          diff = Nothing
        }
