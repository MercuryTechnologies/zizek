-- | @NonEmpty a@ generator.
--
-- > Gen.nonEmpty (Gen.int & Gen.min 0 & Gen.max 100 & Gen.build)
-- >   & Gen.maxSize 10
-- >   & Gen.build
module Hegel.Gen.NonEmpty
  ( NonEmptyBuilder,
    nonEmpty,
  )
where

import Control.Exception (throwIO)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as T
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)
import Hegel.Collection qualified as Collection
import Hegel.Exception (Diagnostic (..))
import Hegel.Gen.Builder (Build (..), HasSize (..), ValidationError (..), checkSizeBounds)
import Hegel.Gen.Internal (Gen (..), draw)
import Hegel.Internal.DataSource (Label (..), startSpan, stopSpan)

data NonEmptyBuilder a = NonEmptyBuilder
  { neElement :: !(Gen a),
    neMinSize :: !Int,
    neMaxSize :: !(Maybe Int)
  }

-- | Generate a random non-empty list whose elements are drawn from the given
-- generator.
--
-- Defaults to a minimum size of 1. A 'minSize' below 1 is a
-- 'ValidationError', since it would contradict the non-empty guarantee.
nonEmpty :: Gen a -> NonEmptyBuilder a
nonEmpty g = NonEmptyBuilder {neElement = g, neMinSize = 1, neMaxSize = Nothing}

instance HasSize (NonEmptyBuilder a) where
  minSize n b = b {neMinSize = n}
  maxSize n b = b {neMaxSize = Just n}

instance Build (NonEmptyBuilder a) (NonEmpty a) where
  build b = withFrozenCallStack $ Draw $ \tc -> do
    checkSizeBounds "Hegel.Gen.NonEmpty" b.neMinSize b.neMaxSize
    checkAtLeastOne b.neMinSize
    startSpan tc LabelList
    result <- Collection.with tc b.neMinSize b.neMaxSize \coll -> do
      let loop acc = do
            keepGoing <- Collection.more coll
            if not keepGoing
              then pure (reverse acc)
              else do
                x <- draw tc b.neElement
                loop (x : acc)
      loop []
    let trimmed = case b.neMaxSize of
          Just mx | length result > mx -> take mx result
          _ -> result
    stopSpan tc False
    pure (NonEmpty.fromList trimmed)

-- | Require @n >= 1@, throwing 'ValidationError' otherwise.
checkAtLeastOne :: (HasCallStack) => Int -> IO ()
checkAtLeastOne n
  | n >= 1 = pure ()
  | otherwise =
      throwIO
        (ValidationError Diagnostic {context = "Hegel.Gen.NonEmpty", detail = "minSize must be at least 1", values = [("minSize", T.pack (show n))], callStack = callStack})
