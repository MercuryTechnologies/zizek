-- | 'Data.IntMap.Strict.IntMap' generator.
--
-- > let keys = Gen.int & Gen.min 0 & Gen.max 100 & Gen.build
-- >     vals = Gen.text & Gen.build
-- >  in Gen.intMap keys vals
-- >       & Gen.minSize 1
-- >       & Gen.maxSize 10
-- >       & Gen.build
module Hegel.Gen.IntMap
  ( IntMapBuilder,
    intMap,
  )
where

import CBOR.Value (Value (..))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Vector qualified as V
import Hegel.Cbor (ParseError (..))
import Hegel.Collection qualified as Collection
import Hegel.Gen.Builder (Build (..), HasSize (..))
import Hegel.Gen.Internal (BasicGenerator (..), Gen (..), basic, draw, materialize, toBasic)
import Hegel.Schema qualified as Schema
import Hegel.TestCase (Label (..), startSpan, stopSpan)

data IntMapBuilder v = IntMapBuilder
  { mKeys :: !(Gen Int),
    mValues :: !(Gen v),
    mMinSize :: !Int,
    mMaxSize :: !(Maybe Int)
  }

-- | Generate a random 'IntMap' with keys drawn from the given 'Int' generator
-- and values from the value generator.
intMap :: Gen Int -> Gen v -> IntMapBuilder v
intMap k v = IntMapBuilder {mKeys = k, mValues = v, mMinSize = 0, mMaxSize = Nothing}

instance HasSize (IntMapBuilder v) where
  minSize n b = b {mMinSize = n}
  maxSize n b = b {mMaxSize = Just n}

instance Build (IntMapBuilder v) (IntMap v) where
  build b = case (toBasic b.mKeys, toBasic b.mValues) of
    (Just bk, Just bv) ->
      basic
        (Schema.map (materialize bk.schema) (materialize bv.schema) b.mMinSize b.mMaxSize)
        (parseIntMap bk.parse bv.parse)
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
                  if IntMap.member k acc
                    then Collection.reject coll (Just "duplicate key") *> loop acc
                    else do
                      v <- draw tc b.mValues
                      loop (IntMap.insert k v acc)
        result <- loop IntMap.empty
        let trimmed = case b.mMaxSize of
              Just mx | IntMap.size result > mx -> IntMap.fromAscList (take mx (IntMap.toAscList result))
              _ -> result
        stopSpan tc False
        pure trimmed

parseIntMap ::
  (Value -> Either ParseError Int) ->
  (Value -> Either ParseError v) ->
  Value ->
  Either ParseError (IntMap v)
parseIntMap pk pv (Array vec) = IntMap.fromList <$> traverse parsePair (V.toList vec)
  where
    parsePair (Array pair) = case V.toList pair of
      [k, v] -> (,) <$> pk k <*> pv v
      _ -> Left ParseError {expected = "[key, value] pair", got = Array pair}
    parsePair v = Left ParseError {expected = "array pair", got = v}
parseIntMap _ _ v = Left ParseError {expected = "array", got = v}
