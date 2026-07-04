-- | @[a]@ generator.
--
-- > Gen.list (Gen.int & Gen.min 0 & Gen.max 100 & Gen.build)
-- >   & Gen.minSize 1
-- >   & Gen.maxSize 10
-- >   & Gen.build
module Hegel.Gen.List
  ( ListBuilder,
    list,
    unique,
  )
where

import Hegel.Collection qualified as Collection
import Hegel.Gen.Builder (Build (..), HasSize (..), checkSizeBounds)
import Hegel.Gen.Internal (Gen (..), draw)
import Hegel.Internal.DataSource (Label (..), startSpan, stopSpan)

data ListBuilder a = ListBuilder
  { lElement :: !(Gen a),
    lMinSize :: !Int,
    lMaxSize :: !(Maybe Int),
    -- | When 'Just', uniqueness is enforced; the predicate decides equality.
    lUnique :: !(Maybe (a -> a -> Bool))
  }

-- | Generate a random list whose elements are drawn from the given generator.
list :: Gen a -> ListBuilder a
list g = ListBuilder {lElement = g, lMinSize = 0, lMaxSize = Nothing, lUnique = Nothing}

-- | Require all elements to be distinct according to the given equality
-- predicate. The predicate is used locally to reject duplicates as they are
-- drawn.
unique :: (a -> a -> Bool) -> ListBuilder a -> ListBuilder a
unique eq b = b {lUnique = Just eq}

instance HasSize (ListBuilder a) where
  minSize n b = b {lMinSize = n}
  maxSize n b = b {lMaxSize = Just n}

instance Build (ListBuilder a) [a] where
  build b = Draw $ \tc -> do
    checkSizeBounds "Hegel.Gen.List" b.lMinSize b.lMaxSize
    startSpan tc LabelList
    -- For unique lists, see Note [Variable-size mode required for reject]
    -- in Hegel.Collection.
    --
    -- Non-unique lists don't call 'Collection.reject' so we don't need to
    -- normalize the bounds.
    let poolMax = case (b.lUnique, b.lMaxSize) of
          (Just _, Just mx) -> Just (Prelude.max (b.lMinSize + 1) mx)
          _ -> b.lMaxSize
    coll <- Collection.new tc b.lMinSize poolMax
    let dup = case b.lUnique of
          Just eq -> \x xs -> any (eq x) xs
          Nothing -> \_ _ -> False
        loop acc = do
          keepGoing <- Collection.more coll
          if not keepGoing
            then pure (reverse acc)
            else do
              x <- draw tc b.lElement
              if dup x acc
                then Collection.reject coll (Just "duplicate element") *> loop acc
                else loop (x : acc)
    result <- loop []
    let trimmed = case b.lMaxSize of
          Just mx | length result > mx -> take mx result
          _ -> result
    stopSpan tc False
    pure trimmed
