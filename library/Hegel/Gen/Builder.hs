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
    GenValidationError (..),
    checkOrdered,
    checkOrderedMaybe,
    checkNonNegative,
    checkSizeBounds,
  )
where

import Control.Exception (Exception (..), throwIO)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Stack (HasCallStack)
import Hegel.Gen.Internal (Gen)

-- | Materialize a fully-configured builder into a 'Gen'.
class Build b a | b -> a where
  build :: (HasCallStack) => b -> Gen a

-- | Builders that accept an inclusive lower bound.
class HasMin b a | b -> a where
  min :: a -> b -> b

-- | Builders that accept an inclusive upper bound.
class HasMax b a | b -> a where
  max :: a -> b -> b

-- | Builders that accept length bounds (e.g. byte counts, element counts).
class HasSize b where
  minSize :: Int -> b -> b
  maxSize :: Int -> b -> b

-- | Builders that accept a bound expressed as a whole calendar year, e.g.
-- bounding a date by year alone instead of spelling out a full Gregorian
-- date for the first and last day.
class HasYear b where
  -- | Set the lower bound to January 1st of the given year.
  minYear :: Integer -> b -> b

  -- | Set the upper bound to December 31st of the given year.
  maxYear :: Integer -> b -> b

-- $validation
-- 'build' is pure, so a misconfigured builder can only be caught once its
-- 'Gen' draws.
--
-- 'Build' instances run one of these checkers at the top of the draw closure,
-- turning a misuse into a 'GenValidationError' at first draw rather than an
-- opaque engine 'Hegel.Internal.Foreign.Raw.HegelError'.
--
-- A malformed /expression/, like an empty 'Hegel.Gen.oneOf' or a non-positive
-- 'Hegel.Gen.frequency' weight, instead 'error's eagerly in
-- "Hegel.Gen.Internal".

-- | Thrown when a materialized builder's configuration is invalid.
data GenValidationError = GenValidationError
  { -- | The fully-qualified module defining the builder that raised it, e.g.
    -- @\"Hegel.Gen.Text\"@.
    context :: !Text,
    -- | Human-readable description of what was invalid.
    detail :: !Text
  }
  deriving stock (Show)

instance Exception GenValidationError where
  displayException e = T.unpack e.context <> ": " <> T.unpack e.detail

-- | Require @lo <= hi@, throwing 'GenValidationError' otherwise.
checkOrdered :: (Ord a, Show a) => Text -> a -> a -> IO ()
checkOrdered what lo hi
  | lo <= hi = pure ()
  | otherwise =
      throwIO
        GenValidationError
          { context = what,
            detail = "min (" <> T.pack (show lo) <> ") > max (" <> T.pack (show hi) <> ")"
          }

-- | Like 'checkOrdered', but only checks when both bounds are present, so an
-- absent bound is fine and only an explicit inversion counts as a misuse.
checkOrderedMaybe :: (Ord a, Show a) => Text -> Maybe a -> Maybe a -> IO ()
checkOrderedMaybe what (Just lo) (Just hi) = checkOrdered what lo hi
checkOrderedMaybe _ _ _ = pure ()

-- | Require @n >= 0@, throwing 'GenValidationError' otherwise.
checkNonNegative :: (Ord a, Num a, Show a) => Text -> a -> IO ()
checkNonNegative what n
  | n >= 0 = pure ()
  | otherwise =
      throwIO GenValidationError {context = what, detail = "negative size (" <> T.pack (show n) <> ")"}

-- | Validate a @minSize@\/@maxSize@ pair: each bound must be non-negative,
-- and ordered when both are set.
checkSizeBounds :: Text -> Int -> Maybe Int -> IO ()
checkSizeBounds what lo mHi = do
  checkNonNegative what lo
  case mHi of
    Nothing -> pure ()
    Just hi -> checkNonNegative what hi *> checkOrdered what lo hi
