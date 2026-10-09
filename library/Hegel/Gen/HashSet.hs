-- | 'Data.HashSet.HashSet' generator.
--
-- > Gen.hashSet (Gen.int & Gen.min 0 & Gen.max 100 & Gen.build)
-- >   & Gen.minSize 1
-- >   & Gen.maxSize 10
-- >   & Gen.build
module Hegel.Gen.HashSet
  ( HashSetBuilder,
    hashSet,
  )
where

import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Hashable (Hashable)
import GHC.Stack (withFrozenCallStack)
import Hegel.Collection qualified as Collection
import Hegel.Gen.Builder (Build (..), HasSize (..), checkSizeBounds)
import Hegel.Gen.Internal (Gen (..), draw, labelOf)
import Hegel.Internal.DataSource (Label (..), combineLabels, spanLabel)

-- | Builder for a 'HashSet' generator from an element generator, sized with
-- 'HasSize'.
data HashSetBuilder a = HashSetBuilder
  { sElement :: !(Gen a),
    sMinSize :: !Int,
    sMaxSize :: !(Maybe Int)
  }

-- | Generate a random hash set whose elements are drawn from the given generator.
hashSet :: Gen a -> HashSetBuilder a
hashSet g = HashSetBuilder {sElement = g, sMinSize = 0, sMaxSize = Nothing}

instance HasSize (HashSetBuilder a) where
  minSize n b = b {sMinSize = n}
  maxSize n b = b {sMaxSize = Just n}

instance (Hashable a) => Build (HashSetBuilder a) (HashSet a) where
  build b = withFrozenCallStack $ Draw (combineLabels [spanLabel LabelSet, labelOf b.sElement]) \tc -> do
    checkSizeBounds "Hegel.Gen.HashSet" b.sMinSize b.sMaxSize
    -- See Note [Variable-size mode required for reject] in Hegel.Collection.
    let poolMax = case b.sMaxSize of
          Nothing -> Nothing
          Just mx -> Just (Prelude.max (b.sMinSize + 1) mx)
    result <- Collection.with tc b.sMinSize poolMax \coll -> do
      let loop acc = do
            keepGoing <- Collection.more coll
            if not keepGoing
              then pure acc
              else do
                x <- draw tc b.sElement
                if HashSet.member x acc
                  then Collection.reject coll (Just "duplicate element") *> loop acc
                  else loop (HashSet.insert x acc)
      loop HashSet.empty
    let trimmed = case b.sMaxSize of
          Just mx | HashSet.size result > mx -> HashSet.fromList (take mx (HashSet.toList result))
          _ -> result
    pure trimmed
