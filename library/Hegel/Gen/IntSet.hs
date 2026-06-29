-- | 'Data.IntSet.IntSet' generator (elements always unique).
--
-- > Gen.intSet (Gen.int & Gen.min 0 & Gen.max 100 & Gen.build)
-- >   & Gen.minSize 1
-- >   & Gen.maxSize 10
-- >   & Gen.build
module Hegel.Gen.IntSet
  ( IntSetBuilder,
    intSet,
  )
where

import CBOR.Value (Value (..))
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Vector qualified as V
import Hegel.Collection qualified as Collection
import Hegel.Gen.Builder (Build (..), HasSize (..))
import Hegel.Gen.Internal (BasicGenerator (..), Gen (..), basic, draw, materialize, toBasic)
import Hegel.Internal.CBOR (ParseError (..))
import Hegel.Internal.DataSource (Label (..), startSpan, stopSpan)
import Hegel.Internal.Schema qualified as Schema

data IntSetBuilder = IntSetBuilder
  { sElement :: !(Gen Int),
    sMinSize :: !Int,
    sMaxSize :: !(Maybe Int)
  }

-- | Generate a random 'IntSet' whose elements are drawn from the given generator.
intSet :: Gen Int -> IntSetBuilder
intSet g = IntSetBuilder {sElement = g, sMinSize = 0, sMaxSize = Nothing}

instance HasSize IntSetBuilder where
  minSize n b = b {sMinSize = n}
  maxSize n b = b {sMaxSize = Just n}

instance Build IntSetBuilder IntSet where
  build b = case toBasic b.sElement of
    Just be ->
      basic
        (Schema.list (materialize be.schema) b.sMinSize b.sMaxSize True)
        (parseIntSet be.parse)
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
                  if IntSet.member x acc
                    then Collection.reject coll (Just "duplicate element") *> loop acc
                    else loop (IntSet.insert x acc)
        result <- loop IntSet.empty
        let trimmed = case b.sMaxSize of
              Just mx | IntSet.size result > mx -> IntSet.fromAscList (take mx (IntSet.toAscList result))
              _ -> result
        stopSpan tc False
        pure trimmed

parseIntSet ::
  (Value -> Either ParseError Int) ->
  Value ->
  Either ParseError IntSet
parseIntSet p (Array vec) = IntSet.fromList <$> traverse p (V.toList vec)
parseIntSet _ v = Left ParseError {expected = "array", got = v}
