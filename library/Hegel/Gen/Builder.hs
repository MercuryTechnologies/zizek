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
  )
where

import Hegel.Gen.Internal (Gen)

-- | Materialise a fully-configured builder into a 'Gen'.
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
