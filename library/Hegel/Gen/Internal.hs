{-# LANGUAGE GADTs #-}

-- | Core generator machinery.
module Hegel.Gen.Internal
  ( -- * Generator type
    Gen (..),
    BasicGenerator (..),
    basic,
    toBasic,
    schema,

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
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Vector qualified as V
import Hegel.Protocol.Cbor (ParseError (..))
import Hegel.Schema qualified as Schema
import Hegel.TestCase
  ( Label (..),
    Status (..),
    TestCase (..),
    generate,
    markComplete,
    startSpan,
    stopSpan,
  )
import Prelude hiding (either, maybe)

-- | A 'Gen' that can be expressed as a single schema request to @hegel@.
-- Construct one with 'basic'.
data BasicGenerator a = BasicGenerator
  { -- | One or more schema parts. A singleton represents a scalar
    -- generator; multiple parts represent an already-flattened tuple.
    schemaParts :: !(NonEmpty Value),
    -- | Converts the server's response back to @a@.
    parse :: Value -> Either ParseError a
  }

instance Functor BasicGenerator where
  fmap f bg = bg {parse = fmap f . bg.parse}

-- | Materialise the wire 'Value' for a 'BasicGenerator'. A singleton
-- part is returned as-is; multiple parts are wrapped in a tuple schema.
schema :: BasicGenerator a -> Value
schema bg = case bg.schemaParts of
  v :| [] -> v
  v :| (w : ws) -> toCBOR (Schema.tuple (v : w : ws))

-- | Construct a leaf 'Gen' from a typed schema and a parse function.
-- The schema is encoded via 'CBOR.Class.toCBOR' and sent in a single
-- round-trip to @hegel@.
--
-- See 'Hegel.Gen.Bool.bool' for a worked example.
basic :: (ToCBOR s) => s -> (Value -> Either ParseError a) -> Gen a
basic s p = Basic (BasicGenerator (toCBOR s :| []) p)

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
  OneOf :: !(Maybe (BasicGenerator a)) -> NonEmpty (Gen a) -> Gen a

toBasic :: Gen a -> Maybe (BasicGenerator a)
toBasic (Basic bg) = Just bg
toBasic (Pure a) = Just (BasicGenerator (NE.singleton (toCBOR Schema.unit)) (\_ -> Right a))
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
        let leftArity = NE.length bf.schemaParts
            rightArity = NE.length ba.schemaParts
            n = leftArity + rightArity
            newParts = bf.schemaParts <> ba.schemaParts
            p (Array arr) | V.length arr == n = do
              let leftV =
                    if leftArity == 1
                      then arr V.! 0
                      else Array (V.slice 0 leftArity arr)
                  rightV =
                    if rightArity == 1
                      then arr V.! leftArity
                      else Array (V.slice leftArity rightArity arr)
              f <- bf.parse leftV
              a <- ba.parse rightV
              pure (f a)
            p v = Left ParseError {expected = T.pack (show n) <> "-element array", got = v}
        pure (BasicGenerator newParts p)

instance Monad Gen where
  (>>=) = Bind

runBasic :: TestCase -> BasicGenerator a -> IO a
runBasic tc bg = do
  raw <- generate tc (schema bg)
  case bg.parse raw of
    Right a -> pure a
    Left err -> throwIO UnexpectedResponse {sentSchema = schema bg, received = raw, cause = err}

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
  let n = NE.length gens
      indexSchema = toCBOR (Schema.integer (0 :: Int) (n - 1))
  startSpan tc LabelOneOf
  raw <- generate tc indexSchema
  i <- case parseIndex raw of
    Right k -> pure k
    Left err -> throwIO UnexpectedResponse {sentSchema = indexSchema, received = raw, cause = err}
  v <- runGenerator tc (NE.toList gens !! i)
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
-- retry budget) or 'UnexpectedResponse' on an unparseable server reply.
draw :: TestCase -> Gen a -> IO a
draw = runGenerator

-- | Discard the current test case when the condition is 'False'. Use this to
-- enforce preconditions on generated values without counting the case as a
-- failure.
assume :: Bool -> Gen ()
assume True = Pure ()
assume False = Draw \tc -> do
  markComplete tc Invalid
  throwIO AssumeRejected

-- | Discard the current test case unconditionally. Polymorphic in the result
-- type so it can appear anywhere in a monadic generator expression.
discard :: Gen a
discard = Draw \tc -> do
  markComplete tc Invalid
  throwIO AssumeRejected

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
            else markComplete tc Invalid *> throwIO AssumeRejected

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
    (y : ys) -> element (y :| ys)
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
            else markComplete tc Invalid *> throwIO AssumeRejected

-- | Choose one of the given generators uniformly.
oneOf :: forall a. NonEmpty (Gen a) -> Gen a
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
      let n = NE.length vals
          vv = V.fromList (NE.toList vals)
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
      pure (BasicGenerator (NE.singleton sch) p)

    pureVal :: Gen a -> Maybe a
    pureVal (Pure a) = Just a
    pureVal _ = Nothing

    fullOpt :: Maybe (BasicGenerator a)
    fullOpt = do
      bs <- traverse toBasic gens
      let sch = toCBOR (Schema.oneOf (NE.toList (fmap schema bs)))
          parsers = V.fromList (NE.toList (fmap (.parse) bs))
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
      pure (BasicGenerator (NE.singleton sch) p)

-- | Generate one of the given values.
element :: NonEmpty a -> Gen a
element = oneOf . fmap pure

-- | Wrap a generator so that schema expansion terminates when it appears
-- on a recursive edge.  Without 'defer', a self-referential generator causes
-- a @\<\<loop\>\>@ exception at construction time.
--
-- Example: a binary tree whose branches recurse through 'defer'.
--
-- > data Tree = Leaf Int | Branch Tree Tree
-- >
-- > treeGen :: Gen Tree
-- > treeGen = oneOf (leaf :| [branch])
-- >   where
-- >     leaf   = Leaf <$> (Gen.integral @Int & Gen.build)
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
enumerate (OneOf _ gs) = concat . NE.toList <$> traverse enumerate gs
enumerate _ = Nothing

-- | Choose one of the given generators, weighted by the accompanying 'Int'.
-- All weights must be positive.
frequency :: NonEmpty (Int, Gen a) -> Gen a
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
    prefixSelect :: Int -> NonEmpty (Int, Gen a) -> Gen a
    prefixSelect n ((w, g) :| rest)
      | n < w = g
      | otherwise = case rest of
          [] -> error "Gen.frequency: prefix-sum invariant violated (unreachable)"
          (x : xs) -> prefixSelect (n - w) (x :| xs)

-- | Generate either 'Nothing' or 'Just' a value from the given generator.
maybe :: Gen a -> Gen (Maybe a)
maybe g = oneOf (pure Nothing :| [Just <$> g])

-- | Generate a 'Left' value from the first generator or a 'Right' value
-- from the second.
either :: Gen a -> Gen b -> Gen (Either a b)
either ga gb = oneOf ((Left <$> ga) :| [Right <$> gb])

-- $exceptions
-- Used for control flow within the runner, not for surfacing test failures.

-- | Thrown when a test case is deliberately discarded, either via 'assume' or
-- 'discard', or by an exhausted 'filtered'\/'mapMaybe' retry budget.
data AssumeRejected = AssumeRejected
  deriving stock (Show)

instance Exception AssumeRejected

-- | Thrown when @hegel@ returns a value that cannot be parsed according
-- to the schema that was sent.
data UnexpectedResponse = UnexpectedResponse
  { -- | The CBOR schema sent to @hegel@.
    sentSchema :: !Value,
    -- | The raw value returned by the server.
    received :: !Value,
    -- | The parse failure.
    cause :: !ParseError
  }
  deriving stock (Show)

instance Exception UnexpectedResponse
