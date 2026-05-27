{-# LANGUAGE FunctionalDependencies #-}

module Hegel.Gen.Builder
  ( Build (..),
    HasMin (..),
    HasMax (..),
    HasSize (..),
  )
where

import Hegel.Gen.Internal (Generator)

class Build b a | b -> a where
  build :: b -> Generator a

class HasMin b a | b -> a where
  min :: a -> b -> b

class HasMax b a | b -> a where
  max :: a -> b -> b

class HasSize b where
  minSize :: Int -> b -> b
  maxSize :: Int -> b -> b
