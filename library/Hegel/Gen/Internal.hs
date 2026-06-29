{-# LANGUAGE GADTs #-}

-- | Core generator machinery.
module Hegel.Gen.Internal
  ( -- * Generator type
    Gen (..),
    BasicGenerator (..),
    BasicSchema (..),
    basic,
    toBasic,
    materialize,
    schemaArity,

    -- * Combinators
    -- $combinators
    draw,
    assume,
    discard,
    defer,
    filtered,
    mapMaybe,
    just,
    oneOf,
    element,
    frequency,
    maybe,
    either,
    enumerate,

    -- * Exceptions
    -- $exceptions
    AssumeRejected (..),
    UnexpectedResponse (..),
  )
where

import CBOR.Class (ToCBOR (..))
import CBOR.Value (Value (Array, NInt, UInt))
import Control.Exception (Exception, throwIO)
import Data.Sequence (Seq, (<|), (|>))
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Vector qualified as V
import GHC.Stack (HasCallStack)
import Hegel.Internal.CBOR (ParseError (..))
import Hegel.Internal.Control (AssumeRejected (..))
import Hegel.Internal.DataSource (Label (..), generate, startSpan, stopSpan)
import Hegel.Internal.Schema qualified as Schema
import Hegel.Internal.TestCase (TestCase)
import Prelude hiding (either, maybe)

-- | A 'Gen' that can be expressed as a single schema request to @hegel@.
-- Construct one with 'basic'.
data BasicGenerator a = BasicGenerator
  { -- | The schema's structure: a scalar wire schema, or a tuple of two or
    -- more parts produced by 'Applicative' composition.
    schema :: !BasicSchema,
    -- | Converts the engine's response back to @a@. For a 'Scalar' schema the
    -- input is the raw scalar value; for a 'Tuple' schema it is the response
    -- 'Array' wrapping every flat component.
    parse :: Value -> Either ParseError a
  }

-- | The schema of a 'BasicGenerator'. 'Scalar' is one wire value; 'Tuple' is
-- the already-flattened component list (always two or more) that 'basicAp'
-- builds when composing basic generators.
data BasicSchema
  = Scalar !Value
  | Tuple !Value !Value !(Seq Value)

-- | Concatenate two schemas into a single flat tuple. Used by 'basicAp' to
-- combine the schemas of two basic generators without rebuilding the
-- materialised tuple schema on every step.
instance Semigroup BasicSchema where
  Scalar a <> Scalar b = Tuple a b Seq.empty
  Scalar a <> Tuple b c rest = Tuple a b (c <| rest)
  Tuple a b rest <> Scalar c = Tuple a b (rest |> c)
  Tuple a b rest <> Tuple c d more = Tuple a b (rest <> (c <| d <| more))

-- | Number of flat components in a 'BasicSchema'.
schemaArity :: BasicSchema -> Int
schemaArity Scalar {} = 1
schemaArity (Tuple _ _ rest) = 2 + Seq.length rest

instance Functor BasicGenerator where
  fmap f bg = bg {parse = fmap f . bg.parse}

-- | Materialize the wire 'Value' for a 'BasicSchema'. A 'Scalar' is its
-- value as-is; a 'Tuple' is wrapped in a tuple schema.
materialize :: BasicSchema -> Value
materialize (Scalar v) = v
materialize (Tuple a b rest) = toCBOR (Schema.tuple (a : b : foldr (:) [] rest))

-- | Construct a leaf 'Gen' from a typed schema and a parse function.
-- The schema is encoded via 'CBOR.Class.toCBOR' and sent in a single
-- round-trip to @hegel@.
--
-- See 'Hegel.Gen.Bool.bool' for a worked example.
basic :: (ToCBOR s) => s -> (Value -> Either ParseError a) -> Gen a
basic s p = Basic (BasicGenerator (Scalar (toCBOR s)) p)

-- | A generator that produces values of type @a@.
--
-- Each constructor represents a different generation strategy. When the
-- whole tree can be expressed as a single basic schema, the runner sends
-- one request; otherwise it falls back to step-by-step interactive
-- generation.
data Gen a where
  -- | A pre-computed constant. Contributes no schema element.
  Pure :: a -> Gen a
  -- | A single round-trip schema request.
  Basic :: BasicGenerator a -> Gen a
  -- | Arbitrary client-side action over a 'TestCase'; used for combinators
  -- (e.g. 'filtered', 'frequency') that don't fit a basic schema.
  Draw :: (TestCase -> IO a) -> Gen a
  -- | 'fmap' over a source generator, carrying a precomputed basic
  -- representation when the source is itself basic.
  Map :: !(Maybe (BasicGenerator a)) -> (b -> a) -> Gen b -> Gen a
  -- | Applicative composition: independent draws. Carries a precomputed
  -- basic representation (a @tuple@ schema) when both sides are basic.
  Ap :: !(Maybe (BasicGenerator a)) -> Gen (b -> a) -> Gen b -> Gen a
  -- | Monadic composition: dependent draws. Always falls back to
  -- interactive generation.
  Bind :: Gen b -> (b -> Gen a) -> Gen a
  -- | Choice among generators. Carries a precomputed basic representation
  -- (a @one_of@ schema) when every branch is basic.
  OneOf :: !(Maybe (BasicGenerator a)) -> [Gen a] -> Gen a

toBasic :: Gen a -> Maybe (BasicGenerator a)
toBasic (Basic bg) = Just bg
toBasic (Pure a) = Just (BasicGenerator (Scalar (toCBOR Schema.unit)) (\_ -> Right a))
toBasic (Map c _ _) = c
toBasic (Ap c _ _) = c
toBasic (OneOf c _) = c
toBasic _ = Nothing

instance Functor Gen where
  fmap f (Pure a) = Pure (f a)
  fmap f (Basic bg) = Basic (fmap f bg)
  fmap f (Map _ g x) = fmap (f . g) x
  fmap f g = Map (fmap f <$> toBasic g) f g

-- | @('<*>')@ and @('>>=')@ have deliberately different semantics:
--
-- * @('<*>')@ treats draws as independent: @hegel@ gets to shrink each
--   component separately. When both sides are basic it collapses to a single
--   request; otherwise it wraps the draws in a @TUPLE@ span.
-- * @('>>=')@ treats draws as dependent: the second draw may vary with the
--   first, so the two are grouped in a @FLAT_MAP@ span.
instance Applicative Gen where
  pure = Pure
  (<*>) gf ga = Ap (basicAp gf ga) gf ga
    where
      -- The basic-schema equivalent of @gf '<*>' ga@, when one exists.
      --
      -- Two basic generators combine into a flat tuple-schema request; if
      -- either side isn't basic, returns 'Nothing'. The Pure fast paths
      -- bypass tuple construction so the unit-schema sentinel never leaks
      -- into tuple element lists.
      basicAp :: Gen (b -> a) -> Gen b -> Maybe (BasicGenerator a)
      basicAp (Pure f) r = fmap f <$> toBasic r
      basicAp l (Pure a) = fmap ($ a) <$> toBasic l
      basicAp l r = do
        bf <- toBasic l
        ba <- toBasic r
        let leftArity = schemaArity bf.schema
            rightArity = schemaArity ba.schema
            n = leftArity + rightArity
            -- Each sub-spec's parser expects its input shaped to match the
            -- sub-spec: a scalar wants its raw component value; a tuple
            -- wants those components re-wrapped in an 'Array'.
            sliceFor spec offset arity arr = case spec of
              Scalar _ -> arr V.! offset
              Tuple {} -> Array (V.slice offset arity arr)
            p (Array arr)
              | V.length arr == n = do
                  f <- bf.parse (sliceFor bf.schema 0 leftArity arr)
                  a <- ba.parse (sliceFor ba.schema leftArity rightArity arr)
                  pure (f a)
            p v = Left ParseError {expected = T.pack (show n) <> "-element array", got = v}
        pure (BasicGenerator (bf.schema <> ba.schema) p)

instance Monad Gen where
  (>>=) = Bind

runBasic :: TestCase -> BasicGenerator a -> IO a
runBasic tc bg = do
  raw <- generate tc (materialize bg.schema)
  case bg.parse raw of
    Right a -> pure a
    Left err -> throwIO UnexpectedResponse {sentSchema = materialize bg.schema, received = raw, cause = err}

-- Count non-'Pure' leaves in an 'Ap' spine, to decide whether a TUPLE span
-- is needed: fewer than 2 real draws don't require one.
apLeafCount :: Gen a -> Int
apLeafCount (Ap _ gf ga) =
  apLeafCount gf + case ga of
    Pure _ -> 0
    _ -> 1
apLeafCount (Pure _) = 0
apLeafCount _ = 1

-- Recursively run the leaves of an @Ap@ spine left-to-right, applying
-- the accumulated function. Used for the non-basic fallback path.
runApSpine :: TestCase -> Gen a -> IO a
runApSpine tc (Ap _ gf ga) = do
  f <- runApSpine tc gf
  a <- runGenerator tc ga
  pure (f a)
runApSpine tc g = runGenerator tc g

runGenerator :: TestCase -> Gen a -> IO a
runGenerator _ (Pure a) = pure a
runGenerator tc g = case toBasic g of
  Just bg -> runBasic tc bg
  Nothing -> runInteractive tc g

runInteractive :: TestCase -> Gen a -> IO a
runInteractive tc (Draw f) = f tc
runInteractive tc (Map _ f g) = do
  startSpan tc LabelMapped
  a <- runGenerator tc g
  stopSpan tc False
  pure (f a)
runInteractive tc node@(Ap _ _ _)
  | apLeafCount node < 2 = runApSpine tc node
  | otherwise = do
      startSpan tc LabelTuple
      a <- runApSpine tc node
      stopSpan tc False
      pure a
runInteractive tc (Bind (Pure a) f) = runGenerator tc (f a)
runInteractive tc (Bind g f) = do
  startSpan tc LabelFlatMap
  a <- runGenerator tc g
  b <- runGenerator tc (f a)
  stopSpan tc False
  pure b
runInteractive tc (OneOf _ gens) = do
  let n = length gens
      indexSchema = toCBOR (Schema.integer (0 :: Int) (n - 1))
  startSpan tc LabelOneOf
  raw <- generate tc indexSchema
  i <- case parseIndex raw of
    Right k -> pure k
    Left err -> throwIO UnexpectedResponse {sentSchema = indexSchema, received = raw, cause = err}
  v <- runGenerator tc (gens !! i)
  stopSpan tc False
  pure v
runInteractive _ (Pure a) = pure a
runInteractive tc (Basic bg) = runBasic tc bg

parseIndex :: Value -> Either ParseError Int
parseIndex (UInt n) = Right (fromIntegral n)
parseIndex (NInt n) = Right (fromIntegral (negate (fromIntegral n :: Integer) - 1))
parseIndex v = Left ParseError {expected = "integer", got = v}

-- $combinators
-- Combinators for filtering and choosing between generators. Discarded test
-- cases are reported to @hegel@ as invalid rather than failing.

-- | Run a generator against a live test case, producing a value. May throw
-- 'AssumeRejected' (via 'assume', 'discard', or an exhausted 'filtered'
-- retry budget) or 'UnexpectedResponse' on an unparseable engine reply.
draw :: TestCase -> Gen a -> IO a
draw = runGenerator

-- | Discard the current test case when the condition is 'False'. Use this to
-- enforce preconditions on generated values without counting the case as a
-- failure.
assume :: Bool -> Gen ()
assume True = Pure ()
assume False = Draw \_ -> throwIO AssumeRejected

-- | Discard the current test case unconditionally. Polymorphic in the result
-- type so it can appear anywhere in a monadic generator expression.
discard :: Gen a
discard = Draw \_ -> throwIO AssumeRejected

-- | Apply a function to values drawn from a generator, retrying up to 3 times
-- when the function returns 'Nothing'. Discards the test case when all retries
-- are exhausted.
mapMaybe :: (a -> Prelude.Maybe b) -> Gen a -> Gen b
mapMaybe f g = Draw \tc -> go tc (3 :: Int)
  where
    go tc n = do
      startSpan tc LabelFilter
      v <- runGenerator tc g
      case f v of
        Prelude.Just b -> stopSpan tc False *> pure b
        Prelude.Nothing -> do
          stopSpan tc True
          if n > 1
            then go tc (n - 1)
            else throwIO AssumeRejected

-- | Draw a 'Just' value from a 'Maybe' generator, discarding test cases where
-- 'Nothing' is drawn.
just :: Gen (Prelude.Maybe a) -> Gen a
just = mapMaybe Prelude.id

-- | Filter values drawn from a generator, retrying up to 3 times before
-- discarding the test case. Exhaustion is treated as 'assume' 'False'.
--
-- When the source generator is finite (i.e. 'enumerate' returns @Just xs@),
-- the predicate is applied statically and the result is drawn from the
-- pre-filtered list in a single round-trip — no retry loop needed.
filtered :: (a -> Bool) -> Gen a -> Gen a
filtered p g = case enumerate g of
  Just xs -> case filter p xs of
    [] -> discard
    ys -> element ys
  Nothing -> Draw \tc -> go tc (3 :: Int)
  where
    go tc n = do
      startSpan tc LabelFilter
      v <- runGenerator tc g
      if p v
        then stopSpan tc False *> pure v
        else do
          stopSpan tc True
          if n > 1
            then go tc (n - 1)
            else throwIO AssumeRejected

-- | Choose one of the given generators. The list must be non-empty;
-- passing @[]@ raises an error at the call site.
--
-- /NOTE/: The empirical distribution across branches is __not__ uniform.
--
-- Hypothesis explores novel choice sequences rather than drawing uniformly,
-- so branches that produce more distinct outputs get visited more often.
--
-- For example, @oneOf [Gen.bool, Gen.int32]@ exhausts the @bool@ branch
-- after two cases (it can only produce 'True' or 'False'), so the rest of
-- the run draws almost exclusively from @int32@. See 'frequency' for the
-- underlying mechanism.
oneOf :: forall a. (HasCallStack) => [Gen a] -> Gen a
oneOf [] = error "Gen.oneOf: used with empty list"
oneOf gens = OneOf basicOneOf gens
  where
    -- The basic-schema equivalent of @oneOf gens@, when one exists.
    --
    -- Fast path: all branches are @Pure@ — emit an integer 0..(n-1) schema
    -- and index into the values directly. This is more compact on the wire
    -- than a @one_of@ schema whose branches are all @constant/null@.
    --
    -- Normal path: build a @one_of@ schema; any non-basic branch makes this
    -- return 'Nothing'.
    basicOneOf :: Maybe (BasicGenerator a)
    basicOneOf = case allPureOpt of
      Just b -> Just b
      Nothing -> fullOpt

    allPureOpt :: Maybe (BasicGenerator a)
    allPureOpt = do
      vals <- traverse pureVal gens
      let n = length vals
          vv = V.fromList vals
          sch = toCBOR (Schema.integer (0 :: Int) (n - 1))
          p v = case parseIndex v of
            Left err -> Left err
            Right i -> case vv V.!? i of
              Just a -> Right a
              Nothing ->
                Left
                  ParseError
                    { expected = "0 <= index < " <> T.pack (show n),
                      got = v
                    }
      pure (BasicGenerator (Scalar sch) p)

    pureVal :: Gen a -> Maybe a
    pureVal (Pure a) = Just a
    pureVal _ = Nothing

    fullOpt :: Maybe (BasicGenerator a)
    fullOpt = do
      bs <- traverse toBasic gens
      let sch = toCBOR (Schema.oneOf (fmap (materialize . (.schema)) bs))
          parsers = V.fromList (fmap (.parse) bs)
          p (Array arr) | V.length arr == 2 = do
            let idxV = arr V.! 0
                val = arr V.! 1
            i <- parseIndex idxV
            case parsers V.!? i of
              Just q -> q val
              Nothing ->
                Left
                  ParseError
                    { expected = "0 <= index < " <> T.pack (show (V.length parsers)),
                      got = idxV
                    }
          p v = Left ParseError {expected = "[index, value] array", got = v}
      pure (BasicGenerator (Scalar sch) p)

-- | Generate one of the given values uniformly. The list must be
-- non-empty; passing @[]@ raises an error at the call site.
element :: (HasCallStack) => [a] -> Gen a
element [] = error "Gen.element: used with empty list"
element xs = oneOf (fmap pure xs)

-- | Wrap a generator so that schema expansion terminates when it appears
-- on a recursive edge.  Without 'defer', a self-referential generator causes
-- a @\<\<loop\>\>@ exception at construction time.
--
-- Example: a binary tree whose branches recurse through 'defer'.
--
-- > data Tree = Leaf Int | Branch Tree Tree
-- >
-- > treeGen :: Gen Tree
-- > treeGen = oneOf [leaf, branch]
-- >   where
-- >     leaf   = Leaf <$> (Gen.int & Gen.build)
-- >     branch = Branch <$> defer treeGen <*> defer treeGen
--
-- 'defer' always falls back to interactive generation (spans are emitted
-- normally, so shrinking still works).
defer :: Gen a -> Gen a
defer g = Draw \tc -> runGenerator tc g

-- | Return the finite set of values a generator can produce, or 'Nothing'
-- if the set is infinite or cannot be statically determined.
--
-- Useful as an optimisation signal: 'filtered' uses this to pre-filter
-- finite generators instead of retrying at runtime.
enumerate :: Gen a -> Maybe [a]
enumerate (Pure a) = Just [a]
enumerate (Map _ f g) = fmap f <$> enumerate g
enumerate (Ap _ gf ga) = do
  fs <- enumerate gf
  as <- enumerate ga
  pure [f a | f <- fs, a <- as]
enumerate (OneOf _ gs) = concat <$> traverse enumerate gs
enumerate _ = Nothing

-- | Choose one of the given generators, weighted by the accompanying 'Int'.
--
-- The list must be non-empty and all weights must be positive; violations
-- raise an error at the call site.
--
-- /NOTE/: Weights bias which branch the engine prefers, especially early in a
-- run, however they do __not__ describe a long-run sampling distribution:
--
-- Hypothesis explores novel choice sequences rather than drawing uniformly, so
-- if branches have different /entropy demand/ (i.e. produce different numbers
-- of distinct outputs) the output distribution will skew towards branches with
-- higher entropy, __not__ a distribution characterized by the given weights.
--
-- For example, imagine you have a recursive, tree-like data structure with a
-- @leaf@ generator that draws leaves & a @recursive@ generator that unfolds
-- more of the tree.
--
-- In this case, @frequency [(10, leaf), (1, recursive)]@ will spend most of
-- its budget on @recursive@ once @leaf@'s novel paths are exhausted.
frequency :: (HasCallStack) => [(Int, Gen a)] -> Gen a
frequency [] = error "Gen.frequency: used with empty list"
frequency pairs
  | any ((<= 0) . fst) pairs = error "Gen.frequency: all weights must be positive"
  | otherwise = Draw \tc -> do
      let total = sum (fmap fst pairs)
          indexSchema = toCBOR (Schema.integer (0 :: Int) (total - 1))
      startSpan tc LabelOneOf
      raw <- generate tc indexSchema
      i <- case parseIndex raw of
        Right k -> pure k
        Left err -> throwIO UnexpectedResponse {sentSchema = indexSchema, received = raw, cause = err}
      let chosen = prefixSelect i pairs
      v <- runGenerator tc chosen
      stopSpan tc False
      pure v
  where
    prefixSelect :: Int -> [(Int, Gen a)] -> Gen a
    prefixSelect _ [] = error "Gen.frequency: prefix-sum invariant violated (unreachable)"
    prefixSelect n ((w, g) : rest)
      | n < w = g
      | otherwise = prefixSelect (n - w) rest

-- | Generate either 'Nothing' or 'Just' a value from the given generator.
maybe :: Gen a -> Gen (Maybe a)
maybe g = oneOf [pure Nothing, Just <$> g]

-- | Generate a 'Left' value from the first generator or a 'Right' value
-- from the second.
either :: Gen a -> Gen b -> Gen (Either a b)
either ga gb = oneOf [Left <$> ga, Right <$> gb]

-- $exceptions
-- 'AssumeRejected' is re-exported from 'Hegel.Internal.TestCase', as it is used for
-- control flow within the runner rather than for surfacing test failures.

-- | Thrown when @hegel@ returns a value that cannot be parsed according
-- to the schema that was sent.
data UnexpectedResponse = UnexpectedResponse
  { -- | The CBOR schema sent to @hegel@.
    sentSchema :: !Value,
    -- | The raw value returned by the engine.
    received :: !Value,
    -- | The parse failure.
    cause :: !ParseError
  }
  deriving stock (Show)

instance Exception UnexpectedResponse
