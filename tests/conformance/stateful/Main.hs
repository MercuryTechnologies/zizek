-- | Conformance test for stateful (model-based) testing.
--
-- Mirrors the IntegerStack example from @references/hegel-rust/src/stateful.rs@.
-- Verifies that:
--
--   * A correctly-specified stack machine passes.
--   * A deliberately-buggy machine finds a counterexample.
--   * The counterexample reproduces from its blob (replay alignment gate).
--
-- Exit codes: 0 = all assertions passed, non-zero = failure.
module Main (main) where

import Control.Monad (unless, when)
import Data.Function ((&))
import Hegel.Gen qualified as Gen
import Hegel.Property (assert, assume, forAll)
import Hegel.Report (Report (..), Result (..))
import Hegel.Runner (check)
import Hegel.Settings (defaultSettings)
import Hegel.Stateful qualified as Stateful
import System.Exit (die, exitSuccess)

-- ---------------------------------------------------------------------------
-- Model

-- | A simple integer stack.
newtype Stack = Stack [Int]

-- ---------------------------------------------------------------------------
-- Rules

push :: Stateful.Rule Stack IO
push =
  Stateful.Rule "push" \(Stack xs) -> do
    n <- forAll (Gen.int & Gen.min (-100) & Gen.max 100 & Gen.build)
    pure (Stack (n : xs))

pop :: Stateful.Rule Stack IO
pop =
  Stateful.Rule "pop" \(Stack xs) -> do
    case xs of
      [] -> pure (Stack [])
      _ : rest -> pure (Stack rest)

-- | Push then pop should return the same element (correct).
pushPop :: Stateful.Rule Stack IO
pushPop =
  Stateful.Rule "push_pop" \(Stack xs) -> do
    n <- forAll (Gen.int & Gen.min (-100) & Gen.max 100 & Gen.build)
    let xs' = n : xs
    let top = head xs' -- safe: we just pushed
    assert (top == n) "pushed element is on top"
    pure (Stack (tail xs')) -- pop it back off

-- | Pop then push — requires a non-empty stack.
popPush :: Stateful.Rule Stack IO
popPush =
  Stateful.Rule "pop_push" \(Stack xs) -> do
    assume (not (null xs))
    let top = head xs
    let rest = tail xs
    pure (Stack (top : rest))

-- | A buggy pushPop: claims the pushed element is always 0.
buggyPushPop :: Stateful.Rule Stack IO
buggyPushPop =
  Stateful.Rule "buggy_push_pop" \(Stack xs) -> do
    n <- forAll (Gen.int & Gen.min (-100) & Gen.max 100 & Gen.build)
    -- Bug: asserts that n == 0, which fails for any non-zero n.
    assert (n == 0) "pushed element is zero (bug: should be n)"
    pure (Stack xs)

-- ---------------------------------------------------------------------------
-- Machines

correctMachine :: Stateful.Machine Stack IO
correctMachine =
  Stateful.Machine
    { initial = pure (Stack []),
      rules = [push, pop, pushPop, popPush],
      invariants = []
    }

buggyMachine :: Stateful.Machine Stack IO
buggyMachine =
  Stateful.Machine
    { initial = pure (Stack []),
      rules = [push, pop, buggyPushPop],
      invariants = []
    }

-- ---------------------------------------------------------------------------
-- Main

assertOk :: String -> Report -> IO ()
assertOk label report = case report.result of
  Ok -> pure ()
  GaveUp _ -> die (label <> ": gave up (too many discards)")
  Counterexample {} -> die (label <> ": unexpected counterexample")
  Aborted e -> die (label <> ": aborted: " <> show e)

assertFails :: String -> Report -> IO ()
assertFails label report = case report.result of
  Counterexample {} -> pure ()
  other -> die (label <> ": expected a counterexample, got: " <> show other)

main :: IO ()
main = do
  -- 1. Correct machine should pass.
  check defaultSettings (Stateful.run correctMachine)
    >>= assertOk "correct machine"

  -- 2. Buggy machine should find a counterexample.
  check defaultSettings (Stateful.run buggyMachine)
    >>= assertFails "buggy machine"

  exitSuccess
