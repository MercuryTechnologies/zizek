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

    -- * Exposed for testing
    retryLoopWith,
  )
where

{- Note [span-discipline]
~~~~~~~~~~~~~~~~~~~~~~~~~
Every span a recursive value opens carries the generator's own label, so the
shrinker can replace a tree with one of its own subtrees. 'draw' opens one
around the whole value, retries included, and one around each child
sub-value drawn through 'childGen'.

A discarded attempt unwinds with an exception and leaves its child spans
open. The engine closes those spans itself, so the retry loop must not call
'stopSpan' for them. It only resets the case's open-span count to where the
attempt started.
-}

import Control.Exception (Handler (..), bracket, catches)
import Control.Monad (when)
import Data.Word (Word64)
import Foreign (Ptr)
import GHC.Stack (withFrozenCallStack)
import Hegel.Gen.Builder (Build (..), checkNonNegativeNamed)
import Hegel.Gen.Internal (Gen (..), draw, labelOf)
import Hegel.Internal.Control (AttemptMispriced (..), LeafBudgetExceeded (..))
import Hegel.Internal.DataSource
  ( HegelRecursion,
    Label (..),
    combineLabels,
    forgetSpansTo,
    freeRecursion,
    newRecursion,
    openSpanDepth,
    recursionBranch,
    recursionFinish,
    recursionLeaf,
    recursionRetry,
    spanLabel,
  )
import Hegel.Internal.TestCase (TestCase)

-- | A branch node's position in the recursion, handed to the branch function
-- alongside its subtree generator.
data RecursionContext = RecursionContext
  { -- | The nesting depth of the branch node about to be built.
    depth :: !Word64,
    -- | The generator's configured 'maxDepth'; anything at this depth must
    -- be a leaf value.
    maxDepth :: !Word64
  }
  deriving stock (Show, Eq)

-- | Builder for a recursively defined generator, bounded with 'maxDepth' and
-- 'maxLeaves'.
data RecursiveBuilder a = RecursiveBuilder
  { rLeaf :: !(Gen a),
    rBranch :: !(RecursionContext -> Gen a -> Gen a),
    rMaxDepth :: !Int,
    rMaxLeaves :: !Int
  }

-- | Given two generators for a self-referential type: a generator for leaf
-- values and a function that can unfold into additional recursive sub-structure,
-- produce a generator
-- | A generator for recursive data types, such that @libhegel@ can shrink
-- around its sub-structure.
--
-- 'Hegel.defer' is slightly more convenient for simple recursive types, but
-- this combinator should be preferred for anything complicated or particularly
-- self-referential (e.g. complex JSON documents).
recursive :: Gen a -> (RecursionContext -> Gen a -> Gen a) -> RecursiveBuilder a
recursive leaf branch =
  RecursiveBuilder {rLeaf = leaf, rBranch = branch, rMaxDepth = 32, rMaxLeaves = 100}

-- | Set the maximum nesting depth of branches produced by a generator.
maxDepth :: Int -> RecursiveBuilder a -> RecursiveBuilder a
maxDepth n b = b {rMaxDepth = n}

-- | Set the maximum number of leaves the generator should produce.
maxLeaves :: Int -> RecursiveBuilder a -> RecursiveBuilder a
maxLeaves n b = b {rMaxLeaves = n}

instance Build (RecursiveBuilder a) a where
  build b = withFrozenCallStack $ Draw label \tc -> do
    checkNonNegativeNamed "Hegel.Gen.Recursive" "maxDepth" b.rMaxDepth
    checkNonNegativeNamed "Hegel.Gen.Recursive" "maxLeaves" b.rMaxLeaves
    bracket
      (newRecursion tc (fromIntegral b.rMaxDepth) (fromIntegral b.rMaxLeaves))
      (freeRecursion tc)
      (retryLoop tc b label)
    where
      label = combineLabels [spanLabel LabelRecursive, labelOf b.rLeaf]

-- | Regenerate the whole value from the root after a 'LeafBudgetExceeded' or
-- 'AttemptMispriced' unwind out of 'subtree'.
retryLoop :: TestCase -> RecursiveBuilder a -> Word64 -> Ptr HegelRecursion -> IO a
retryLoop tc b label recursion = do
  base <- openSpanDepth tc
  -- See Note [span-discipline] for why each attempt forgets, rather than
  -- closes, the spans an earlier attempt left open.
  retryLoopWith (forgetSpansTo tc base *> subtree tc recursion b label 0) (recursionRetry tc recursion)

-- | Draw the sub-value at @depth@ in its caller's span.
subtree :: TestCase -> Ptr HegelRecursion -> RecursiveBuilder a -> Word64 -> Word64 -> IO a
subtree tc recursion b label depth = do
  isBranch <- recursionBranch tc recursion depth
  result <-
    if isBranch
      then do
        let ctx = RecursionContext {depth, maxDepth = fromIntegral b.rMaxDepth}
        draw tc (b.rBranch ctx (childGen recursion b label depth))
      else recursionLeaf tc recursion *> draw tc b.rLeaf
  when (depth == 0) (recursionFinish tc recursion)
  pure result

-- | Helper to handle looping on a recursive draw, exposed for testing only.
retryLoopWith :: IO a -> IO () -> IO a
retryLoopWith attempt onLeafBudgetExceeded =
  attempt
    `catches` [ Handler \LeafBudgetExceeded -> onLeafBudgetExceeded *> retryLoopWith attempt onLeafBudgetExceeded,
                Handler \AttemptMispriced -> retryLoopWith attempt onLeafBudgetExceeded
              ]

-- | The generator handed to the caller's branch function: one more sub-value,
-- one level deeper, in a span with the whole generator's @label@.
childGen :: Ptr HegelRecursion -> RecursiveBuilder a -> Word64 -> Word64 -> Gen a
childGen recursion b label depth = Draw label \tc -> subtree tc recursion b label (depth + 1)
