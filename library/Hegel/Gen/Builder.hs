{-# LANGUAGE FunctionalDependencies #-}

-- | Typeclasses that make up the modifier vocabulary for generator builders.
--
-- Each builder type implements the subset of these classes that applies to
-- it.
module Hegel.Gen.Builder
  ( Build (..),
    HasMin (..),
    HasMax (..),
    HasSize (..),
    HasYear (..),

    -- * Validation
    -- $validation
    ValidationError (..),
    checkOrdered,
    checkOrderedMaybe,
    checkNonNegative,
    checkNonNegativeNamed,
    checkSizeBounds,
  )
where

import Control.Exception (throwIO)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)
import Hegel.Exception (Diagnostic (..), ValidationError (..))
import Hegel.Gen.Internal (Gen)

-- | Materialize a configured builder, validating it when drawn.
class Build b a | b -> a where
  build :: (HasCallStack) => b -> Gen a

-- | Builders that accept an inclusive lower bound.
class HasMin b a | b -> a where
  min :: a -> b -> b

-- | Builders that accept an inclusive upper bound.
class HasMax b a | b -> a where
  max :: a -> b -> b

-- | Builders that accept length bounds.
class HasSize b where
  minSize :: Int -> b -> b
  maxSize :: Int -> b -> b

-- | Builders that accept bounds expressed as whole calendar years.
class HasYear b where
  -- | Set the lower bound to January 1st of the given year.
  minYear :: Integer -> b -> b

  -- | Set the upper bound to December 31st of the given year.
  maxYear :: Integer -> b -> b

-- $validation
-- Invalid builder configuration throws 'ValidationError' when drawn and
-- becomes a shrinkable counterexample during a property run.
--
-- Empty choices and nonpositive frequency weights throw the same exception
-- when the combinator is evaluated.

-- | Require @lo <= hi@, throwing 'ValidationError' otherwise.
checkOrdered :: (HasCallStack, Ord a, Show a) => Text -> a -> a -> IO ()
checkOrdered what lo hi
  | lo <= hi = pure ()
  | otherwise =
      throwIO
        (ValidationError Diagnostic {context = what, detail = "min must be at most max", values = [("min", T.pack (show lo)), ("max", T.pack (show hi))], callStack = callStack})

-- | Like 'checkOrdered', but only checks when both bounds are present, so an
-- absent bound is fine and only an explicit inversion counts as a misuse.
checkOrderedMaybe :: (HasCallStack, Ord a, Show a) => Text -> Maybe a -> Maybe a -> IO ()
checkOrderedMaybe what (Just lo) (Just hi) = withFrozenCallStack (checkOrdered what lo hi)
checkOrderedMaybe _ _ _ = pure ()

-- | Require @n >= 0@, throwing 'ValidationError' otherwise.
checkNonNegative :: (HasCallStack, Ord a, Num a, Show a) => Text -> a -> IO ()
checkNonNegative what n = withFrozenCallStack $ checkNonNegativeNamed what "value" n

-- | Require a named field to be nonnegative, throwing 'ValidationError' otherwise.
checkNonNegativeNamed :: (HasCallStack, Ord a, Num a, Show a) => Text -> Text -> a -> IO ()
checkNonNegativeNamed what field n
  | n >= 0 = pure ()
  | otherwise =
      throwIO (ValidationError Diagnostic {context = what, detail = field <> " must be nonnegative", values = [(field, T.pack (show n))], callStack = callStack})

-- | Validate a @minSize@\/@maxSize@ pair: each bound must be non-negative,
-- and ordered when both are set.
checkSizeBounds :: (HasCallStack) => Text -> Int -> Maybe Int -> IO ()
checkSizeBounds what lo mHi = withFrozenCallStack $ do
  checkNonNegativeNamed what "minSize" lo
  case mHi of
    Nothing -> pure ()
    Just hi -> checkNonNegativeNamed what "maxSize" hi *> checkOrdered what lo hi
