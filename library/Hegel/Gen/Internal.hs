{-# LANGUAGE GADTs #-}

-- | Core generator machinery.
module Hegel.Gen.Internal
  ( -- * Generator type
    Gen (..),

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
  )
where

import Control.Exception (throwIO)
import GHC.Stack (HasCallStack)
import Hegel.Internal.Control (AssumeRejected (..))
import Hegel.Internal.DataSource (Label (..), drawInteger, startSpan, stopSpan)
import Hegel.Internal.TestCase (TestCase)
import Prelude hiding (either, maybe)

-- | A generator that produces values of type @a@.
--
-- There is no server-side compound generation: every non-leaf constructor is
-- run by composing client-side draws over spans (see 'Hegel.Internal.DataSource'),
-- so all the work here is in how those spans nest, not in schema construction.
data Gen a where
  -- | A pre-computed constant. Consumes no entropy.
  Pure :: a -> Gen a
  -- | Arbitrary client-side action over a 'TestCase'; every leaf generator
  -- (bool, integer, float, …) and combinator (e.g. 'filtered', 'frequency')
  -- bottoms out here.
  Draw :: (TestCase -> IO a) -> Gen a
  -- | 'fmap' over a source generator.
  Map :: (b -> a) -> Gen b -> Gen a
  -- | Applicative composition: independent draws, wrapped in a @TUPLE@ span
  -- when there are two or more real (non-'Pure') leaves.
  Ap :: Gen (b -> a) -> Gen b -> Gen a
  -- | Monadic composition: dependent draws, wrapped in a @FLAT_MAP@ span.
  Bind :: Gen b -> (b -> Gen a) -> Gen a
  -- | Choice among generators, wrapped in a @ONE_OF@ span.
  OneOf :: [Gen a] -> Gen a

instance Functor Gen where
  fmap f (Pure a) = Pure (f a)
  fmap f (Map g x) = Map (f . g) x
  fmap f g = Map f g

-- | @('<*>')@ and @('>>=')@ have deliberately different semantics:
--
-- * @('<*>')@ treats draws as independent: @hegel@ gets to shrink each
--   component separately, grouped in a @TUPLE@ span.
-- * @('>>=')@ treats draws as dependent: the second draw may vary with the
--   first, so the two are grouped in a @FLAT_MAP@ span.
instance Applicative Gen where
  pure = Pure
  (<*>) = Ap

instance Monad Gen where
  (>>=) = Bind

-- Count non-'Pure' leaves in an 'Ap' spine, to decide whether a TUPLE span
-- is needed: fewer than 2 real draws don't require one.
apLeafCount :: Gen a -> Int
apLeafCount (Ap gf ga) =
  apLeafCount gf + case ga of
    Pure _ -> 0
    _ -> 1
apLeafCount (Pure _) = 0
apLeafCount _ = 1

-- Recursively run the leaves of an @Ap@ spine left-to-right, applying
-- the accumulated function.
runApSpine :: TestCase -> Gen a -> IO a
runApSpine tc (Ap gf ga) = do
  f <- runApSpine tc gf
  a <- runGenerator tc ga
  pure (f a)
runApSpine tc g = runGenerator tc g

runGenerator :: TestCase -> Gen a -> IO a
runGenerator _ (Pure a) = pure a
runGenerator tc g = runInteractive tc g

runInteractive :: TestCase -> Gen a -> IO a
runInteractive tc (Draw f) = f tc
runInteractive tc (Map f g) = do
  startSpan tc LabelMapped
  a <- runGenerator tc g
  stopSpan tc False
  pure (f a)
runInteractive tc node@(Ap _ _)
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
runInteractive tc (OneOf gens) = do
  let n = length gens
  startSpan tc LabelOneOf
  i <- drawInteger tc 0 (toInteger (n - 1))
  v <- runGenerator tc (gens !! fromInteger i)
  stopSpan tc False
  pure v
runInteractive _ (Pure a) = pure a

-- $combinators
-- Combinators for filtering and choosing between generators. Discarded test
-- cases are reported to @hegel@ as invalid rather than failing.

-- | Run a generator against a live test case, producing a value. May throw
-- 'AssumeRejected' (via 'assume', 'discard', or an exhausted 'filtered'
-- retry budget).
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

-- | Apply a function to values drawn from a generator, making up to 3 attempts
-- when the function returns 'Nothing'. Discards the test case when all attempts
-- are exhausted.
--
-- When the source generator is finite (i.e. 'enumerate' returns @Just xs@), the
-- function is applied statically and the result is drawn from the pre-mapped
-- list in a single round-trip — no retry loop, and no risk of discarding a
-- satisfiable case just because the retries happened to miss.
mapMaybe :: (a -> Prelude.Maybe b) -> Gen a -> Gen b
mapMaybe f g = case enumerate g of
  Just xs -> case [b | a <- xs, Prelude.Just b <- [f a]] of
    [] -> discard
    ys -> element ys
  Nothing -> Draw \tc -> go tc (3 :: Int)
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

-- | Filter values drawn from a generator, making up to 3 attempts before
-- discarding the test case. Exhaustion is treated as 'assume' 'False'.
--
-- Defined in terms of 'mapMaybe', so it inherits the static fast path: when
-- the source generator is finite (i.e. 'enumerate' returns @Just xs@), the
-- predicate is applied statically and the result is drawn from the
-- pre-filtered list in a single round-trip — no retry loop needed.
filtered :: (a -> Bool) -> Gen a -> Gen a
filtered p = mapMaybe \a -> if p a then Prelude.Just a else Prelude.Nothing

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
oneOf :: (HasCallStack) => [Gen a] -> Gen a
oneOf [] = error "Gen.oneOf: used with empty list"
oneOf gens = OneOf gens

-- | Generate one of the given values (not uniformly — see the distribution
-- note on 'oneOf'). The list must be non-empty; passing @[]@ raises an error
-- at the call site.
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
-- Useful as an optimization signal: 'filtered' uses this to pre-filter
-- finite generators instead of retrying at runtime.
enumerate :: Gen a -> Maybe [a]
enumerate (Pure a) = Just [a]
enumerate (Map f g) = fmap f <$> enumerate g
enumerate (Ap gf ga) = do
  fs <- enumerate gf
  as <- enumerate ga
  pure [f a | f <- fs, a <- as]
enumerate (OneOf gs) = concat <$> traverse enumerate gs
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
      startSpan tc LabelOneOf
      i <- drawInteger tc 0 (toInteger (total - 1))
      let chosen = prefixSelect (fromInteger i) pairs
      v <- runGenerator tc chosen
      stopSpan tc False
      pure v
  where
    total = sum (fmap fst pairs)

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
-- 'AssumeRejected' is re-exported from 'Hegel.Internal.Control', as it is used for
-- control flow within the runner rather than for surfacing test failures.
