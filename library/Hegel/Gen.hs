-- | Generator combinators, designed for qualified import as @Gen@.
--
-- Build a generator by chaining modifiers onto a builder and materializing
-- with 'build':
--
-- > import Data.Function ((&))
-- > import Hegel.Gen qualified as Gen
-- >
-- > gen1 = Gen.bool                                             & Gen.build
-- > gen2 = Gen.int & Gen.min 0 & Gen.max 100                    & Gen.build
-- > gen3 = Gen.double & Gen.min 0 & Gen.max 1 & Gen.disallowNan & Gen.build
-- > gen4 = Gen.binary & Gen.minSize 4 & Gen.maxSize 64          & Gen.build
-- > gen5 = Gen.text  & Gen.minSize 1 & Gen.maxSize 64           & Gen.build
-- > gen6 = Gen.char                                             & Gen.build
-- > gen7 = Gen.regex "[a-z]+" & Gen.fullMatch                   & Gen.build
--
-- Applying a modifier that doesn't belong to a builder (e.g.
-- @Gen.integral & Gen.disallowNan@) is a type error.
--
-- Enable @ApplicativeDo@ for component-wise shrinking of independent draws
-- in do-notation.
module Hegel.Gen
  ( -- * Core types
    BasicGenerator (schema, parse),
    TestCase,

    -- * Builder classes
    Build (build),
    HasMin (min),
    HasMax (max),
    HasSize (minSize, maxSize),

    -- * Boolean
    BoolBuilder,
    bool,
    weighted,

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

    -- * List
    ListBuilder,
    list,
    unique,

    -- * Set
    SetBuilder,
    set,

    -- * HashSet
    HashSetBuilder,
    hashSet,

    -- * IntSet
    IntSetBuilder,
    intSet,

    -- * Map
    MapBuilder,
    map,

    -- * HashMap
    HashMapBuilder,
    hashMap,

    -- * IntMap
    IntMapBuilder,
    intMap,

    -- * Text
    TextBuilder,
    text,

    -- * Char
    CharBuilder,
    char,
    codec,
    minCodepoint,
    maxCodepoint,
    categories,
    excludeCategories,
    includeCharacters,
    excludeCharacters,

    -- * UUID
    UuidBuilder,
    uuid,
    version,

    -- * URI
    UriBuilder,
    uri,
    UriTextBuilder,
    uriText,

    -- * Domain
    DomainBuilder,
    domain,
    maxLength,

    -- * Regex
    RegexBuilder,
    regex,
    fullMatch,
    alphabet,

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
    defer,
    filtered,
    mapMaybe,
    just,
    enumerate,

    -- * Exceptions
    AssumeRejected (..),
    UnexpectedResponse (..),
  )
where

import Hegel.Gen.Binary (BinaryBuilder, binary)
import Hegel.Gen.Bool (BoolBuilder, bool, weighted)
import Hegel.Gen.Builder (Build (..), HasMax (..), HasMin (..), HasSize (..))
import Hegel.Gen.Char
  ( CharBuilder,
    categories,
    char,
    codec,
    excludeCategories,
    excludeCharacters,
    includeCharacters,
    maxCodepoint,
    minCodepoint,
  )
import Hegel.Gen.Domain (DomainBuilder, domain, maxLength)
import Hegel.Gen.Float
  ( FloatBuilder,
    disallowInfinity,
    disallowNan,
    double,
    exclusiveMax,
    exclusiveMin,
    float,
  )
import Hegel.Gen.HashMap (HashMapBuilder, hashMap)
import Hegel.Gen.HashSet (HashSetBuilder, hashSet)
import Hegel.Gen.IntMap (IntMapBuilder, intMap)
import Hegel.Gen.IntSet (IntSetBuilder, intSet)
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
    BasicGenerator (parse, schema),
    UnexpectedResponse (..),
    assume,
    defer,
    discard,
    draw,
    either,
    element,
    enumerate,
    filtered,
    frequency,
    just,
    mapMaybe,
    maybe,
    oneOf,
  )
import Hegel.Gen.List (ListBuilder, list, unique)
import Hegel.Gen.Map (MapBuilder, map)
import Hegel.Gen.Regex (RegexBuilder, alphabet, fullMatch, regex)
import Hegel.Gen.Set (SetBuilder, set)
import Hegel.Gen.Text (TextBuilder, text)
import Hegel.Gen.Uri (UriBuilder, UriTextBuilder, uri, uriText)
import Hegel.Gen.Uuid (UuidBuilder, uuid, version)
import Hegel.Internal.TestCase (TestCase)
import Prelude hiding (either, map, maybe)
