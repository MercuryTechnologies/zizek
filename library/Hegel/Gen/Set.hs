-- | 'Data.Set.Set' generator.
--
-- Build a set generator by chaining modifiers onto 'set' and materialising
-- with 'Hegel.Gen.Builder.build':
--
-- > Gen.set (Gen.int & Gen.min 0 & Gen.max 100 & Gen.build)
-- >   & Gen.minSize 1
-- >   & Gen.maxSize 10
-- >   & Gen.build
--
-- Sets are always unique. When the element generator is basic, a @list@
-- schema with @unique=true@ is used for a single round-trip. Otherwise the
-- interactive @new_collection@ \/ @collection_more@ loop is used with
-- server-side duplicate rejection.
module Hegel.Gen.Set
  ( SetBuilder,
    set,
  )
where

import CBOR.Value (Value (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector qualified as V
import Hegel.Collection qualified as Collection
import Hegel.Gen.Builder (Build (..), HasSize (..))
import Hegel.Gen.Internal (BasicGenerator (..), Gen (..), basic, draw, materialize, toBasic)
import Hegel.Protocol.Cbor (ParseError (..))
import Hegel.Schema qualified as Schema
import Hegel.TestCase (Label (..), startSpan, stopSpan)

data SetBuilder a = SetBuilder
  { sElement :: !(Gen a),
    sMinSize :: !Int,
    sMaxSize :: !(Maybe Int)
  }

-- | Generate a random set whose elements are drawn from the given generator.
set :: Gen a -> SetBuilder a
set g = SetBuilder {sElement = g, sMinSize = 0, sMaxSize = Nothing}

instance HasSize (SetBuilder a) where
  minSize n b = b {sMinSize = n}
  maxSize n b = b {sMaxSize = Just n}

instance (Ord a) => Build (SetBuilder a) (Set a) where
  build b = case toBasic b.sElement of
    Just be ->
      basic
        (Schema.list (materialize be.schema) b.sMinSize b.sMaxSize True)
        (parseSet be.parse)
    Nothing ->
      Draw $ \tc -> do
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
                  if Set.member x acc
                    then Collection.reject coll (Just "duplicate element") *> loop acc
                    else loop (Set.insert x acc)
        result <- loop Set.empty
        let trimmed = case b.sMaxSize of
              Just mx | Set.size result > mx -> Set.take mx result
              _ -> result
        stopSpan tc False
        pure trimmed

parseSet :: (Ord a) => (Value -> Either ParseError a) -> Value -> Either ParseError (Set a)
parseSet p (Array vec) = Set.fromList <$> traverse p (V.toList vec)
parseSet _ v = Left ParseError {expected = "array", got = v}
