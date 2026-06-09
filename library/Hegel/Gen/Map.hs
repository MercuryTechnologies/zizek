-- | 'Data.Map.Strict.Map' generator (keys always unique).
--
-- > Gen.map (Gen.text & Gen.build) (Gen.int & Gen.build)
-- >   & Gen.minSize 1
-- >   & Gen.maxSize 10
-- >   & Gen.build
module Hegel.Gen.Map
  ( MapBuilder,
    map,
  )
where

import CBOR.Value (Value (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Hegel.Collection qualified as Collection
import Hegel.Gen.Builder (Build (..), HasSize (..))
import Hegel.Gen.Internal (BasicGenerator (..), Gen (..), basic, draw, materialize, toBasic)
import Hegel.Protocol.Cbor (ParseError (..))
import Hegel.Schema qualified as Schema
import Hegel.TestCase (Label (..), startSpan, stopSpan)
import Prelude hiding (map)

data MapBuilder k v = MapBuilder
  { mKeys :: !(Gen k),
    mValues :: !(Gen v),
    mMinSize :: !Int,
    mMaxSize :: !(Maybe Int)
  }

-- | Generate a random map with keys and values drawn from the given generators.
map :: Gen k -> Gen v -> MapBuilder k v
map k v = MapBuilder {mKeys = k, mValues = v, mMinSize = 0, mMaxSize = Nothing}

instance HasSize (MapBuilder k v) where
  minSize n b = b {mMinSize = n}
  maxSize n b = b {mMaxSize = Just n}

instance (Ord k) => Build (MapBuilder k v) (Map k v) where
  build b = case (toBasic b.mKeys, toBasic b.mValues) of
    (Just bk, Just bv) ->
      basic
        (Schema.map (materialize bk.schema) (materialize bv.schema) b.mMinSize b.mMaxSize)
        (parseMap bk.parse bv.parse)
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
                  if Map.member k acc
                    then Collection.reject coll (Just "duplicate key") *> loop acc
                    else do
                      v <- draw tc b.mValues
                      loop (Map.insert k v acc)
        result <- loop Map.empty
        let trimmed = case b.mMaxSize of
              Just mx | Map.size result > mx -> Map.take mx result
              _ -> result
        stopSpan tc False
        pure trimmed

parseMap ::
  (Ord k) =>
  (Value -> Either ParseError k) ->
  (Value -> Either ParseError v) ->
  Value ->
  Either ParseError (Map k v)
parseMap pk pv (Array vec) = Map.fromList <$> traverse parsePair (V.toList vec)
  where
    parsePair (Array pair) = case V.toList pair of
      [k, v] -> (,) <$> pk k <*> pv v
      _ -> Left ParseError {expected = "[key, value] pair", got = Array pair}
    parsePair v = Left ParseError {expected = "array pair", got = v}
parseMap _ _ v = Left ParseError {expected = "array", got = v}
