-- | @[a]@ generator.
--
-- Build a list generator by chaining modifiers onto 'list' and materialising
-- with 'Hegel.Gen.Builder.build':
--
-- > Gen.list (Gen.int & Gen.min 0 & Gen.max 100 & Gen.build)
-- >   & Gen.minSize 1
-- >   & Gen.maxSize 10
-- >   & Gen.build
--
-- When the element generator is basic (expressible as a single schema
-- request), the list is generated in a single round-trip using the @list@
-- schema. When it is not (e.g. after 'Hegel.Gen.Internal.filtered'), the
-- interactive @new_collection@ \/ @collection_more@ loop is used instead.
module Hegel.Gen.List
  ( ListBuilder,
    list,
    unique,
  )
where

import CBOR.Value (Value (..))
import Data.Maybe (isJust)
import Data.Vector qualified as V
import Hegel.Collection qualified as Collection
import Hegel.Gen.Builder (Build (..), HasSize (..))
import Hegel.Gen.Internal (BasicGenerator (..), Gen (..), basic, draw, materialize, toBasic)
import Hegel.Protocol.Cbor (ParseError (..))
import Hegel.Schema qualified as Schema
import Hegel.TestCase (Label (..), startSpan, stopSpan)

data ListBuilder a = ListBuilder
  { lElement :: !(Gen a),
    lMinSize :: !Int,
    lMaxSize :: !(Maybe Int),
    -- | When 'Just', uniqueness is enforced; the predicate decides equality
    -- on the interactive path.
    lUnique :: !(Maybe (a -> a -> Bool))
  }

-- | Generate a random list whose elements are drawn from the given generator.
list :: Gen a -> ListBuilder a
list g = ListBuilder {lElement = g, lMinSize = 0, lMaxSize = Nothing, lUnique = Nothing}

-- | Require all elements to be distinct according to the given equality
-- predicate. On the basic path the server enforces uniqueness using its own
-- representation equality; on the interactive path the predicate is used
-- client-side to reject duplicates.
unique :: (a -> a -> Bool) -> ListBuilder a -> ListBuilder a
unique eq b = b {lUnique = Just eq}

instance HasSize (ListBuilder a) where
  minSize n b = b {lMinSize = n}
  maxSize n b = b {lMaxSize = Just n}

instance Build (ListBuilder a) [a] where
  build b = case toBasic b.lElement of
    Just be ->
      basic
        (Schema.list (materialize be.schema) b.lMinSize b.lMaxSize (isJust b.lUnique))
        (parseList be.parse)
    Nothing ->
      Draw $ \tc -> do
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

parseList :: (Value -> Either ParseError a) -> Value -> Either ParseError [a]
parseList p (Array vec) = traverse p (V.toList vec)
parseList _ v = Left ParseError {expected = "array", got = v}
