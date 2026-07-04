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
import Hegel.Collection qualified as Collection
import Hegel.Gen.Builder (Build (..), HasSize (..), requireOrdered)
import Hegel.Gen.Internal (Gen (..), draw)
import Hegel.Internal.DataSource (Label (..), startSpan, stopSpan)

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
  build b = case b.sMaxSize of
    Just hi -> requireOrdered "Gen.hashSet" b.sMinSize hi go
    Nothing -> go
    where
      go = Draw $ \tc -> do
        startSpan tc LabelList
        -- See Note [Variable-size mode required for reject] in Hegel.Collection.
        let poolMax = case b.sMaxSize of
              Nothing -> Nothing
              Just mx -> Just (Prelude.max (b.sMinSize + 1) mx)
        coll <- Collection.new tc b.sMinSize poolMax
        let loop acc = do
              keepGoing <- Collection.more coll
              if not keepGoing
                then pure acc
                else do
                  x <- draw tc b.sElement
                  if HashSet.member x acc
                    then Collection.reject coll (Just "duplicate element") *> loop acc
                    else loop (HashSet.insert x acc)
        result <- loop HashSet.empty
        let trimmed = case b.sMaxSize of
              Just mx | HashSet.size result > mx -> HashSet.fromList (take mx (HashSet.toList result))
              _ -> result
        stopSpan tc False
        pure trimmed
