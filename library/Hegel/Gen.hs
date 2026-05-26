-- | Umbrella module for generator combinators.
--
-- Designed for qualified import:
--
-- > import Hegel.Gen qualified as Gen
-- > import Hegel.Range qualified as Range
--
-- which brings @Gen.bool@, @Gen.integer@, @Gen.oneOf@, etc. into scope.
--
-- The @*Options@ types (@'Hegel.Gen.Integer.IntegerOptions'@,
-- @'Hegel.Gen.Float.FloatOptions'@, @'Hegel.Gen.Binary.BinaryOptions'@) are
-- intentionally /not/ re-exported here to avoid record-field ambiguity when
-- multiple options types share field names (e.g. @minValue@). Import them
-- directly from their per-type modules when you need to build custom options:
--
-- > import Hegel.Gen.Float (FloatOptions (..))
module Hegel.Gen
  ( -- * Core types
    Generator,
    BasicGenerator (..),
    pattern Schema,

    -- * Boolean
    bool,

    -- * Integer
    defaultIntegerOptions,
    integer,
    boundedIntegers,
    integerWith,

    -- * Float
    defaultFloatOptions,
    float,
    double,
    floatWith,
    doubleWith,

    -- * Binary
    defaultBinaryOptions,
    binary,
    binaryWith,

    -- * Combinators
    draw,
    oneOf,
    filtered,
    assume,

    -- * Exceptions
    InvalidTestCase (..),
    UnexpectedResponse (..),
  )
where

import Hegel.Gen.Binary
  ( binary,
    binaryWith,
    defaultBinaryOptions,
  )
import Hegel.Gen.Bool (bool)
import Hegel.Gen.Float
  ( defaultFloatOptions,
    double,
    doubleWith,
    float,
    floatWith,
  )
import Hegel.Gen.Integer
  ( boundedIntegers,
    defaultIntegerOptions,
    integer,
    integerWith,
  )
import Hegel.Gen.Internal
  ( BasicGenerator (..),
    Generator,
    InvalidTestCase (..),
    UnexpectedResponse (..),
    assume,
    draw,
    filtered,
    oneOf,
    pattern Schema,
  )
