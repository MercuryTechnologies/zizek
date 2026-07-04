-- | 'Data.HashMap.Strict.HashMap' generator.
--
-- > Gen.hashMap (Gen.text & Gen.build) (Gen.int & Gen.build)
-- >   & Gen.minSize 1
-- >   & Gen.maxSize 10
-- >   & Gen.build
module Hegel.Gen.HashMap
  ( HashMapBuilder,
    hashMap,
  )
where

import CBOR.Value (Value (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.Vector qualified as V
import Hegel.Collection qualified as Collection
import Hegel.Gen.Builder (Build (..), HasSize (..))
import Hegel.Gen.Internal (BasicGenerator (..), Gen (..), basic, draw, materialize, toBasic)
import Hegel.Internal.DataSource (Label (..), startSpan, stopSpan)
import Hegel.Internal.Foreign.CBOR (ParseError (..))
import Hegel.Internal.Foreign.Schema qualified as Schema

data HashMapBuilder k v = HashMapBuilder
  { mKeys :: !(Gen k),
    mValues :: !(Gen v),
    mMinSize :: !Int,
    mMaxSize :: !(Maybe Int)
  }

-- | Generate a random hash map with keys and values drawn from the given generators.
hashMap :: Gen k -> Gen v -> HashMapBuilder k v
hashMap k v = HashMapBuilder {mKeys = k, mValues = v, mMinSize = 0, mMaxSize = Nothing}

instance HasSize (HashMapBuilder k v) where
  minSize n b = b {mMinSize = n}
  maxSize n b = b {mMaxSize = Just n}

instance (Hashable k) => Build (HashMapBuilder k v) (HashMap k v) where
  build b = case (toBasic b.mKeys, toBasic b.mValues) of
    (Just bk, Just bv) ->
      basic
        (Schema.map (materialize bk.schema) (materialize bv.schema) b.mMinSize b.mMaxSize)
        (parseHashMap bk.parse bv.parse)
    _ ->
      Draw $ \tc -> do
        startSpan tc LabelMap
        -- See Note [Variable-size mode required for reject] in Hegel.Collection.
        let poolMax = case b.mMaxSize of
              Nothing -> Nothing
              Just mx -> Just (Prelude.max (b.mMinSize + 1) mx)
        coll <- Collection.new tc b.mMinSize poolMax
        let loop acc = do
              keepGoing <- Collection.more coll
              if not keepGoing
                then pure acc
                else do
                  k <- draw tc b.mKeys
                  if HashMap.member k acc
                    then Collection.reject coll (Just "duplicate key") *> loop acc
                    else do
                      v <- draw tc b.mValues
                      loop (HashMap.insert k v acc)
        result <- loop HashMap.empty
        let trimmed = case b.mMaxSize of
              Just mx | HashMap.size result > mx -> HashMap.fromList (take mx (HashMap.toList result))
              _ -> result
        stopSpan tc False
        pure trimmed

parseHashMap ::
  (Hashable k) =>
  (Value -> Either ParseError k) ->
  (Value -> Either ParseError v) ->
  Value ->
  Either ParseError (HashMap k v)
parseHashMap pk pv (Array vec) = HashMap.fromList <$> traverse parsePair (V.toList vec)
  where
    parsePair (Array pair) = case V.toList pair of
      [k, v] -> (,) <$> pk k <*> pv v
      _ -> Left ParseError {expected = "[key, value] pair", got = Array pair}
    parsePair v = Left ParseError {expected = "array pair", got = v}
parseHashMap _ _ v = Left ParseError {expected = "array", got = v}
