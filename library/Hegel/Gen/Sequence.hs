-- | Generators that pick from a fixed list of values: reorderings,
-- order-preserving subsets, and variable-length samples.
--
-- > Gen.permutation [1 .. 5 :: Int]
-- > Gen.subsequence "abcdef" & Gen.minSize 1 & Gen.build
-- > Gen.sample "abc" & Gen.withoutReplacement & Gen.maxSize 2 & Gen.build
module Hegel.Gen.Sequence
  ( permutation,
    SubsequenceBuilder,
    subsequence,
    SampleBuilder,
    sample,
    withReplacement,
    withoutReplacement,
  )
where

import Control.Exception (throwIO)
import Control.Monad (when)
import Data.List (sort)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)
import Hegel.Collection (Collection)
import Hegel.Collection qualified as Collection
import Hegel.Exception (Diagnostic (..), ValidationError (..))
import Hegel.Gen.Builder (Build (..), HasSize (..), checkSizeBounds)
import Hegel.Gen.Internal (Gen (..))
import Hegel.Internal.DataSource (Label (..), drawInteger, spanLabel)
import Hegel.Internal.TestCase (TestCase)

-- | Generate a reordering of the given values.
--
-- A permutation shrinks toward the original order.
permutation :: [a] -> Gen [a]
permutation xs = Draw (spanLabel LabelPermutation) \tc ->
  fmap (Vector.unsafeIndex values) <$> drawIndexSample tc n n n
  where
    values = Vector.fromList xs
    n = Vector.length values

-- | Builder for a generator of order-preserving subsets of a fixed list,
-- sized with 'HasSize'.
data SubsequenceBuilder a = SubsequenceBuilder
  { subElements :: !(Vector a),
    subMinSize :: !Int,
    subMaxSize :: !(Maybe Int)
  }

-- | Generate a subset of the given values that keeps their original order.
--
-- Each position counts separately, so equal values at different positions
-- can appear together. A subsequence shrinks toward fewer values taken from
-- the front of the list.
subsequence :: [a] -> SubsequenceBuilder a
subsequence xs =
  SubsequenceBuilder {subElements = Vector.fromList xs, subMinSize = 0, subMaxSize = Nothing}

instance HasSize (SubsequenceBuilder a) where
  minSize n b = b {subMinSize = n}
  maxSize n b = b {subMaxSize = Just n}

instance Build (SubsequenceBuilder a) [a] where
  build b = withFrozenCallStack $ Draw (spanLabel LabelSubsequence) \tc -> do
    checkSizeBounds here b.subMinSize b.subMaxSize
    checkFits b.subMinSize n
    indices <- drawIndexSample tc n b.subMinSize (clampMax n b.subMaxSize)
    pure (Vector.unsafeIndex b.subElements <$> sort indices)
    where
      n = Vector.length b.subElements

-- | Builder for a generator of variable-length samples from a fixed list,
-- sized with 'HasSize' and drawn 'withReplacement' unless set
-- 'withoutReplacement'.
data SampleBuilder a = SampleBuilder
  { smpElements :: !(Vector a),
    smpMinSize :: !Int,
    smpMaxSize :: !(Maybe Int),
    smpReplacement :: !Bool
  }

-- | Generate a list of values drawn from the given values.
--
-- By default each draw may repeat an earlier one, and the result shrinks
-- toward a short list repeating the first value.
sample :: [a] -> SampleBuilder a
sample xs =
  SampleBuilder
    { smpElements = Vector.fromList xs,
      smpMinSize = 0,
      smpMaxSize = Nothing,
      smpReplacement = True
    }

-- | Let each draw repeat a value an earlier draw already took.
withReplacement :: SampleBuilder a -> SampleBuilder a
withReplacement b = b {smpReplacement = True}

-- | Take each position at most once, keeping values in the order they were
-- drawn.
--
-- The result shrinks toward the first values of the list in their original
-- order.
withoutReplacement :: SampleBuilder a -> SampleBuilder a
withoutReplacement b = b {smpReplacement = False}

instance HasSize (SampleBuilder a) where
  minSize n b = b {smpMinSize = n}
  maxSize n b = b {smpMaxSize = Just n}

instance Build (SampleBuilder a) [a] where
  build b = withFrozenCallStack $ Draw (spanLabel LabelSample) \tc -> do
    checkSizeBounds here b.smpMinSize b.smpMaxSize
    if b.smpReplacement
      then do
        when (n == 0 && b.smpMinSize > 0) $
          throwIO
            ( ValidationError
                Diagnostic
                  { context = here,
                    detail = "a nonempty sample needs at least one value to draw from",
                    values = [("minSize", T.pack (show b.smpMinSize))],
                    callStack = callStack
                  }
            )
        -- An empty list can only produce an empty sample, so the collection
        -- is capped at zero whatever maxSize says.
        let cap = if n == 0 then Just 0 else b.smpMaxSize
        Collection.with tc b.smpMinSize cap \coll -> drawRepeated tc coll b.smpElements
      else do
        checkFits b.smpMinSize n
        indices <- drawIndexSample tc n b.smpMinSize (clampMax n b.smpMaxSize)
        pure (Vector.unsafeIndex b.smpElements <$> indices)
    where
      n = Vector.length b.smpElements

here :: Text
here = "Hegel.Gen.Sequence"

-- | Require @minSize@ to be at most the number of values to choose from.
checkFits :: (HasCallStack) => Int -> Int -> IO ()
checkFits minSz n
  | minSz <= n = pure ()
  | otherwise =
      throwIO
        ( ValidationError
            Diagnostic
              { context = here,
                detail = "minSize must be at most the number of values to choose from",
                values = [("minSize", T.pack (show minSz)), ("values", T.pack (show n))],
                callStack = callStack
              }
        )

-- | The largest size a draw without replacement can reach: @maxSize@ when set,
-- and never more than the @n@ values available.
clampMax :: Int -> Maybe Int -> Int
clampMax n = maybe n (Prelude.min n)

-- | Draw between @minSz@ and @maxSz@ distinct indices into @[0, n)@, in the
-- order they were drawn.
--
-- The result shrinks toward fewer indices and toward the earliest index not
-- yet taken, so its minimal value is @[0 .. minSz - 1]@.
drawIndexSample :: TestCase -> Int -> Int -> Int -> IO [Int]
drawIndexSample tc n minSz maxSz
  -- With nothing to choose from, no collection is opened, so an empty input
  -- consumes no choices. This matches the other frontends' draw sequence.
  | n == 0 = pure []
  | otherwise = Collection.with tc minSz (Just maxSz) \coll -> go coll (Seq.fromList [0 .. n - 1]) []
  where
    go :: Collection -> Seq Int -> [Int] -> IO [Int]
    go coll remaining acc
      -- The emptiness check comes first, so a draw that took every index
      -- never asks the collection for more.
      | Seq.null remaining = pure (reverse acc)
      | otherwise = do
          keepGoing <- Collection.more coll
          if not keepGoing
            then pure (reverse acc)
            else do
              j <- fromInteger <$> drawInteger tc 0 (toInteger (Seq.length remaining - 1))
              -- Removing the chosen index keeps the rest in their original
              -- order, which is what makes a draw of all zeros the identity.
              go coll (Seq.deleteAt j remaining) (Seq.index remaining j : acc)

-- | Draw values with replacement until the collection says to stop.
drawRepeated :: TestCase -> Collection -> Vector a -> IO [a]
drawRepeated tc coll values = go []
  where
    top = toInteger (Vector.length values - 1)
    go acc = do
      keepGoing <- Collection.more coll
      if not keepGoing
        then pure (reverse acc)
        else do
          i <- drawInteger tc 0 top
          go (Vector.unsafeIndex values (fromInteger i) : acc)
