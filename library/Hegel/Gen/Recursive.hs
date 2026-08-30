-- | Recursively defined data: trees, JSON documents, and other
-- self-referential shapes.
--
-- > data Json = Number Double | Array [Json]
-- >
-- > jsonGen :: Gen Json
-- > jsonGen =
-- >   Gen.recursive
-- >     (Number <$> (Gen.double & Gen.build))
-- >     (\_ctx subtrees -> Array <$> (Gen.list subtrees & Gen.maxSize 5 & Gen.build))
-- >     & Gen.maxDepth 4
-- >     & Gen.maxLeaves 20
-- >     & Gen.build
module Hegel.Gen.Recursive
  ( RecursiveBuilder,
    recursive,
    maxDepth,
    maxLeaves,
    RecursionContext (..),
  )
where

import Control.Exception (Handler (..), bracket, catches)
import Control.Monad (when)
import Data.Word (Word64)
import Foreign (Ptr)
import Hegel.Gen.Builder (Build (..), checkNonNegative)
import Hegel.Gen.Internal (Gen (..), draw)
import Hegel.Internal.Control (AttemptMispriced (..), LeafBudgetExceeded (..))
import Hegel.Internal.DataSource
  ( HegelRecursion,
    Label (..),
    freeRecursion,
    newRecursion,
    recursionBranch,
    recursionFinish,
    recursionLeaf,
    recursionRetry,
    startSpan,
    stopSpan,
  )
import Hegel.Internal.TestCase (TestCase)

-- | A branch node's position in the recursion, handed to the branch function
-- alongside its subtree generator.
data RecursionContext = RecursionContext
  { -- | The nesting depth of the branch node about to be built: 0 for the
    -- root, one more than its parent for every deeper branch.
    depth :: !Word64,
    -- | The generator's configured 'maxDepth': a sub-value at this depth is
    -- always a leaf.
    maxDepth :: !Word64
  }
  deriving stock (Show, Eq)

data RecursiveBuilder a = RecursiveBuilder
  { rLeaf :: !(Gen a),
    rBranch :: !(RecursionContext -> Gen a -> Gen a),
    rMaxDepth :: !Int,
    rMaxLeaves :: !Int
  }

-- | Generate recursively defined data by decomposing it into non-recursive
-- base cases and a rule for combining sub-values into one more level of
-- structure.
--
-- @leaf@ generates the base cases. @branch@ receives the branch node's
-- 'RecursionContext' and a generator of sub-values of the same type, and
-- returns a generator combining some number of them into a compound value;
-- it runs once per branch node generated.
--
-- The engine owns branch probability, the depth cap ('maxDepth', default 32),
-- the leaf budget ('maxLeaves', default 100), and the per-value target size,
-- so a generator built this way is identically distributed across every
-- @hegel@ frontend, and the shrinker can replace a generated value with one
-- of its own subtrees.
--
-- 'Hegel.Gen.Internal.defer' remains how to write recursion that doesn't
-- decompose into a leaf generator and a branch function over sub-values;
-- 'recursive' trades that generality for the depth cap, leaf budget, and
-- subtree-replacement shrinking above.
recursive :: Gen a -> (RecursionContext -> Gen a -> Gen a) -> RecursiveBuilder a
recursive leaf branch =
  RecursiveBuilder {rLeaf = leaf, rBranch = branch, rMaxDepth = 32, rMaxLeaves = 100}

-- | Set the maximum nesting depth of branches (default 32).
--
-- A sub-value at this depth is always a leaf, so a 'maxDepth' of 0 generates
-- only leaves.
maxDepth :: Int -> RecursiveBuilder a -> RecursiveBuilder a
maxDepth n b = b {rMaxDepth = n}

-- | Set the maximum number of leaves one generated value may contain
-- (default 100).
--
-- Each generated value steers toward a target size drawn from across this
-- budget, adapting to the number of sub-values the branch function actually
-- draws, so typical sizes span the whole budget. An attempt that draws more
-- than 'maxLeaves' leaves is discarded and retried steering toward a smaller
-- target; the test case is rejected as invalid when several retries in a row
-- fail to fit.
maxLeaves :: Int -> RecursiveBuilder a -> RecursiveBuilder a
maxLeaves n b = b {rMaxLeaves = n}

instance Build (RecursiveBuilder a) a where
  build b = Draw \tc -> do
    checkNonNegative "Hegel.Gen.Recursive" b.rMaxDepth
    checkNonNegative "Hegel.Gen.Recursive" b.rMaxLeaves
    bracket
      (newRecursion tc (fromIntegral b.rMaxDepth) (fromIntegral b.rMaxLeaves))
      (freeRecursion tc)
      (retryLoop tc b)

-- | Regenerate the whole value from the root after a 'LeafBudgetExceeded' or
-- 'AttemptMispriced' unwind out of 'subtree'.
retryLoop :: TestCase -> RecursiveBuilder a -> Ptr HegelRecursion -> IO a
retryLoop tc b recursion =
  subtree tc recursion b 0
    `catches` [ Handler \LeafBudgetExceeded -> recursionRetry tc recursion *> retryLoop tc b recursion,
                Handler \AttemptMispriced -> retryLoop tc b recursion
              ]

-- | Draw one sub-value at @depth@: the leaf-or-branch decision, then either a
-- leaf or a further call through the caller's branch function.
--
-- The engine closes and discards every span this attempt opened when either
-- retry signal fires, so this never runs 'stopSpan' on the way out through an
-- exception: the span open\/close here is a plain sequence with no
-- 'bracket'\/'finally' around it, which is what leaves an exception room to
-- skip 'stopSpan' on unwind instead of closing a span the engine already
-- closed.
subtree :: TestCase -> Ptr HegelRecursion -> RecursiveBuilder a -> Word64 -> IO a
subtree tc recursion b depth = do
  startSpan tc LabelRecursive
  isBranch <- recursionBranch tc recursion depth
  result <-
    if isBranch
      then do
        let ctx = RecursionContext {depth, maxDepth = fromIntegral b.rMaxDepth}
        draw tc (b.rBranch ctx (childGen recursion b depth))
      else recursionLeaf tc recursion *> draw tc b.rLeaf
  when (depth == 0) (recursionFinish tc recursion)
  stopSpan tc False
  pure result

-- | The generator handed to the caller's branch function: one more sub-value,
-- one level deeper.
childGen :: Ptr HegelRecursion -> RecursiveBuilder a -> Word64 -> Gen a
childGen recursion b depth = Draw \tc -> subtree tc recursion b (depth + 1)
