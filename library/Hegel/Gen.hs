-- | Generator combinators, designed for qualified import as @Gen@.
--
-- Build a generator by chaining modifiers onto a builder and materialising
-- with 'build':
--
-- > import Data.Function ((&))
-- > import Hegel.Gen qualified as Gen
-- >
-- > gen1 = Gen.bool                                              & Gen.build
-- > gen2 = Gen.integral @Int & Gen.min 0 & Gen.max 100           & Gen.build
-- > gen3 = Gen.double & Gen.min 0 & Gen.max 1 & Gen.disallowNan  & Gen.build
-- > gen4 = Gen.binary & Gen.minSize 4 & Gen.maxSize 64           & Gen.build
--
-- Applying a modifier that doesn't belong to a builder (e.g.
-- @Gen.integral & Gen.disallowNan@) is a type error.
--
-- Enable @ApplicativeDo@ for component-wise shrinking of independent draws
-- in do-notation.
module Hegel.Gen
  ( -- * Core types
    BasicGenerator (..),

    -- * Builder classes
    Build (build),
    HasMin (min),
    HasMax (max),
    HasSize (minSize, maxSize),

    -- * Boolean
    BoolBuilder,
    bool,

    -- * Integral
    IntegralBuilder,
    integral,
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
    AssumeRejected (..),
    UnexpectedResponse (..),
  )
where

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
  ( IntegralBuilder,
    enum,
    enumBounded,
    int,
    int16,
    int32,
    int64,
    int8,
    integral,
    word,
    word16,
    word32,
    word64,
    word8,
  )
import Hegel.Gen.Internal
  ( AssumeRejected (..),
    BasicGenerator (..),
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
  )
import Prelude hiding (either, maybe)
