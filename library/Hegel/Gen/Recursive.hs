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
Normally, libhegel never discards an opened span on its own, so the function
that opens a span is responsible for closing it (including on an exception).

Recursive draws, however, can be closed by libhegel at which point we must
consider the span to have been closed by the engine (and thus not issue a
call to 'stopSpan' ourselves). This is indicated with an exception, and so
we /don't/ bracket `subtree` with `startSpan`/`stopSpan`.
-}

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
  { -- | The nesting depth of the branch node about to be built.
    depth :: !Word64,
    -- | The generator's configured 'maxDepth'; anything at this depth must
    -- be a leaf value.
    maxDepth :: !Word64
  }
  deriving stock (Show, Eq)

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
retryLoop tc b recursion = retryLoopWith (subtree tc recursion b 0) (recursionRetry tc recursion)

-- see [span-discipline] above for why this doesn't open/close spans under
-- a `bracket`.

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

-- | Helper to handle looping on a recursive draw, exposed for testing only.
retryLoopWith :: IO a -> IO () -> IO a
retryLoopWith attempt onLeafBudgetExceeded =
  attempt
    `catches` [ Handler \LeafBudgetExceeded -> onLeafBudgetExceeded *> retryLoopWith attempt onLeafBudgetExceeded,
                Handler \AttemptMispriced -> retryLoopWith attempt onLeafBudgetExceeded
              ]

-- | The generator handed to the caller's branch function: one more sub-value,
-- one level deeper.
childGen :: Ptr HegelRecursion -> RecursiveBuilder a -> Word64 -> Gen a
childGen recursion b depth = Draw \tc -> subtree tc recursion b (depth + 1)
