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
    requireOrdered,
    requireOrderedMaybe,
  )
where

import GHC.Stack (HasCallStack)
import Hegel.Gen.Internal (Gen)

-- | Materialize a fully-configured builder into a 'Gen'.
class Build b a | b -> a where
  build :: b -> Gen a

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

-- | Require @lo <= hi@, raising a descriptive error at the builder's call
-- site otherwise. 'Build' instances that carry bounds (@min@\/@max@,
-- @minSize@\/@maxSize@) call this on the materialized bounds before handing
-- them to the engine — inverted bounds are a builder misuse we can catch
-- eagerly at @build@, rather than letting libhegel reject them only on the
-- first draw.
requireOrdered :: (HasCallStack, Ord a, Show a) => String -> a -> a -> b -> b
requireOrdered what lo hi b
  | lo <= hi = b
  | otherwise = error (what <> ": min (" <> show lo <> ") > max (" <> show hi <> ")")

-- | 'requireOrdered', but only when both bounds are explicitly set — for
-- builders (e.g. 'Hegel.Gen.Float.FloatBuilder') where either bound may be
-- absent and only an explicit inversion is a misuse.
requireOrderedMaybe :: (HasCallStack, Ord a, Show a) => String -> Maybe a -> Maybe a -> b -> b
requireOrderedMaybe what (Just lo) (Just hi) b = requireOrdered what lo hi b
requireOrderedMaybe _ _ _ b = b
