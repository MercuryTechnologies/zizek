module Hegel.Range
  ( Range (..),
    bounded,
    between,
    singleton,
    lowerBound,
    upperBound,
  )
where

-- | An inclusive range from a lower to an upper bound.
data Range a = Range !a !a

-- | Full range of a 'Bounded' type.
bounded :: (Bounded a) => Range a
bounded = Range minBound maxBound

-- | Range between two explicit bounds.
between :: a -> a -> Range a
between = Range

-- | A range containing a single value.
singleton :: a -> Range a
singleton x = Range x x

lowerBound :: Range a -> a
lowerBound (Range lo _) = lo

upperBound :: Range a -> a
upperBound (Range _ hi) = hi
