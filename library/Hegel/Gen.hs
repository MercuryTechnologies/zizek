-- | Umbrella module for generator combinators.
--
-- Designed for qualified import:
--
-- > import Hegel.Gen qualified as Gen
--
-- which brings @Gen.bool@, @Gen.integer@, @Gen.double@, @Gen.build@, etc.
-- into scope. Build a generator by chaining modifiers and materialising with
-- @Gen.build@:
--
-- > -- simplest form
-- > gen1 = Gen.bool & Gen.build
-- >
-- > -- numeric bounds
-- > gen2 = Gen.integer @Int & Gen.min 0 & Gen.max 100 & Gen.build
-- >
-- > -- float with constraints
-- > gen3 = Gen.double & Gen.min 0 & Gen.max 1 & Gen.disallowNan & Gen.build
-- >
-- > -- sized binary
-- > gen4 = Gen.binary & Gen.minSize 4 & Gen.maxSize 64 & Gen.build
module Hegel.Gen
  ( -- * Core types
    Generator,
    BasicGenerator (..),
    pattern Schema,

    -- * Builder classes
    Build (build),
    HasMin (min),
    HasMax (max),
    HasSize (minSize, maxSize),

    -- * Boolean
    BoolBuilder,
    bool,

    -- * Integer
    IntegerBuilder,
    integer,
    int,
    int8,
    int16,
    int32,
    int64,
    word,
    word8,
    word16,
    word32,
    word64,

    -- * Enumeration
    enum,
    enumBounded,

    -- * Float
    FloatBuilder,
    float,
    double,
    exclusiveMin,
    exclusiveMax,
    disallowNan,
    disallowInfinity,

    -- * Binary
    BinaryBuilder,
    binary,

    -- * Choice
    oneOf,
    element,
    frequency,

    -- * Maybe & Either
    maybe,
    either,

    -- * Conditional
    draw,
    assume,
    discard,
    filtered,
    mapMaybe,
    just,

    -- * Exceptions
    InvalidTestCase (..),
    UnexpectedResponse (..),
  )
where

import Prelude hiding (either, maybe)

import Hegel.Gen.Binary (BinaryBuilder, binary)
import Hegel.Gen.Bool (BoolBuilder, bool)
import Hegel.Gen.Builder (Build (..), HasMax (..), HasMin (..), HasSize (..))
import Hegel.Gen.Float
  ( FloatBuilder,
    disallowInfinity,
    disallowNan,
    double,
    exclusiveMax,
    exclusiveMin,
    float,
  )
import Hegel.Gen.Integer
  ( IntegerBuilder,
    enum,
    enumBounded,
    int,
    int16,
    int32,
    int64,
    int8,
    integer,
    word,
    word16,
    word32,
    word64,
    word8,
  )
import Hegel.Gen.Internal
  ( BasicGenerator (..),
    Generator,
    InvalidTestCase (..),
    UnexpectedResponse (..),
    assume,
    discard,
    draw,
    either,
    element,
    filtered,
    frequency,
    just,
    mapMaybe,
    maybe,
    oneOf,
    pattern Schema,
  )
