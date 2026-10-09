{-# LANGUAGE GADTs #-}

-- | Core generator machinery.
module Hegel.Gen.Internal
  ( -- * Generator type
    Gen (..),
    labelOf,

    -- * Combinators
    -- $combinators
    draw,
    drawInline,
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
    prefixSelect,

    -- * Exceptions
    -- $exceptions
    AssumeRejected (..),
  )
where

import Control.Exception (throw, throwIO)
import Data.Text qualified as T
import Data.Vector qualified as Vector
import Data.Word (Word64)
import GHC.Stack (HasCallStack, callStack)
import Hegel.Exception (Diagnostic (..), InvariantViolation (..), ValidationError (..))
import Hegel.Internal.Control (AssumeRejected (..))
import Hegel.Internal.DataSource (Label (..), combineLabels, drawInteger, spanLabel, startSpan, stopSpan)
import Hegel.Internal.TestCase (TestCase)
import Prelude hiding (either, maybe)

-- | A generator that produces values of type @a@.
--
-- Every constructor but 'Pure' carries the span label 'draw' opens around
-- its value, so the engine can tell which generator produced a group of
-- choices.
data Gen a where
  -- | A pre-computed constant. Consumes no entropy.
  Pure :: a -> Gen a
  -- | Arbitrary client-side action over a 'TestCase'; every leaf generator
  -- and combinators such as 'filtered' and 'frequency' bottom out here.
  Draw :: Word64 -> (TestCase -> IO a) -> Gen a
  -- | 'fmap' over a source generator.
  Map :: Word64 -> (b -> a) -> Gen b -> Gen a
  -- | Applicative composition of independent draws. A spine of two or more
  -- non-'Pure' leaves gets a tuple span.
  Ap :: Word64 -> Gen (b -> a) -> Gen b -> Gen a
  -- | Monadic composition of dependent draws.
  Bind :: Word64 -> Gen b -> (b -> Gen a) -> Gen a
  -- | Choice among generators.
  OneOf :: Word64 -> [Gen a] -> Gen a

-- | The label of the span 'draw' opens around a generator's value.
--
-- Generators that draw the same shape of value share a label, and a
-- composite generator's label combines its components' labels, so a list of
-- integers and a list of text are labelled differently.
labelOf :: Gen a -> Word64
labelOf = \case
  Pure _ -> spanLabel LabelJust
  Draw l _ -> l
  Map l _ _ -> l
  Ap l _ _ -> l
  Bind l _ _ -> l
  OneOf l _ -> l

instance Functor Gen where
  fmap f (Pure a) = Pure (f a)
  fmap f (Map l g x) = Map l (f . g) x
  fmap f g = Map (combineLabels [spanLabel LabelMapped, labelOf g]) f g

-- | @('<*>')@ and @('>>=')@ have deliberately different semantics:
--
-- * @('<*>')@ treats draws as independent: @hegel@ gets to shrink each
--   component separately, grouped in a tuple span.
-- * @('>>=')@ treats draws as dependent: the second draw may vary with the
--   first, so the two are grouped in a flat-map span.
instance Applicative Gen where
  pure = Pure
  gf <*> ga = Ap (apLabel gf ga) gf ga

instance Monad Gen where
  g >>= k = Bind (combineLabels [spanLabel LabelFlatMap, labelOf g]) g k

-- | The label of an 'Ap' node: a tuple of its two sides, or the other side's
-- own label when one side is 'Pure' and so draws nothing.
apLabel :: Gen (b -> a) -> Gen b -> Word64
apLabel (Pure _) ga = labelOf ga
apLabel gf (Pure _) = labelOf gf
apLabel gf ga = combineLabels [spanLabel LabelTuple, labelOf gf, labelOf ga]

-- | The number of spans 'runApSpine' opens.
apLeafCount :: Gen a -> Int
apLeafCount (Ap _ gf ga) =
  apLeafCount gf + case ga of
    Pure _ -> 0
    _ -> 1
apLeafCount (Pure _) = 0
apLeafCount _ = 1

-- | Draw the leaves of an 'Ap' spine left to right, applying the accumulated
-- function. A 'Map' at the head of the spine applies its function to its
-- source directly, so it adds no span of its own inside the tuple.
runApSpine :: TestCase -> Gen a -> IO a
runApSpine tc (Ap _ gf ga) = do
  f <- runApSpine tc gf
  a <- draw tc ga
  pure (f a)
runApSpine tc (Map _ f g) = f <$> draw tc g
runApSpine tc g = draw tc g

-- $combinators
-- Combinators for filtering and choosing between generators. Discarded test
-- cases are reported to @hegel@ as invalid rather than failing.

-- | Run a generator against a live test case, producing a value inside a
-- span labelled with 'labelOf'. May throw 'AssumeRejected' via 'assume',
-- 'discard', or an exhausted 'filtered' retry budget.
--
-- The span is closed in plain sequence rather than under 'bracket'. An
-- unwind out of the draw leaves it open, which is what a discarded recursion
-- attempt needs, since the engine closes that attempt's spans itself. A
-- caller that recovers from an unwind and keeps drawing closes the leftover
-- spans with 'Hegel.Internal.DataSource.discardSpansTo'.
draw :: TestCase -> Gen a -> IO a
draw _ (Pure a) = pure a
draw tc (Bind _ (Pure a) k) = draw tc (k a)
draw tc g@Ap {} | apLeafCount g < 2 = runApSpine tc g
draw tc g = do
  startSpan tc (labelOf g)
  a <- drawInline tc g
  stopSpan tc False
  pure a

-- | Run a generator inside the caller's span, opening spans only for its
-- components.
--
-- A generator that forwards to another one, such as 'defer', uses this so
-- the forwarding layer does not add a second span around the same value.
drawInline :: TestCase -> Gen a -> IO a
drawInline _ (Pure a) = pure a
drawInline tc (Draw _ f) = f tc
drawInline tc (Map _ f g) = f <$> draw tc g
drawInline tc g@Ap {} = runApSpine tc g
drawInline tc (Bind _ g k) = draw tc g >>= draw tc . k
drawInline tc (OneOf _ gens) = do
  i <- drawInteger tc 0 (toInteger (length gens - 1))
  draw tc (gens !! fromInteger i)

-- | Discard the current test case when the condition is 'False'. Use this to
-- enforce preconditions on generated values without counting the case as a
-- failure.
assume :: Bool -> Gen ()
assume True = Pure ()
assume False = discard

-- | Discard the current test case unconditionally. Polymorphic in the result
-- type so it can appear anywhere in a monadic generator expression.
discard :: Gen a
discard = Draw (spanLabel LabelJust) \_ -> throwIO AssumeRejected

-- | Apply a function to values drawn from a generator, making up to 3 attempts
-- when the function returns 'Nothing'. Discards the test case when all attempts
-- are exhausted.
--
-- Each attempt runs in its own span, closed as discarded when the attempt
-- misses, so the shrinker can delete it.
mapMaybe :: (a -> Prelude.Maybe b) -> Gen a -> Gen b
mapMaybe f g = Draw (combineLabels [spanLabel LabelFilter, labelOf g]) \tc -> go tc (3 :: Int)
  where
    go tc n = do
      startSpan tc (spanLabel LabelFilterAttempt)
      v <- draw tc g
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
filtered :: (a -> Bool) -> Gen a -> Gen a
filtered p = mapMaybe \a -> if p a then Prelude.Just a else Prelude.Nothing

-- | Choose one of the given generators. The list must be non-empty;
-- passing @[]@ throws 'ValidationError' when evaluated.
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
oneOf [] = throw (ValidationError Diagnostic {context = "Gen.oneOf", detail = "used with empty list", values = [("choices", "0")], callStack = callStack})
oneOf gens = OneOf (combineLabels (spanLabel LabelOneOf : fmap labelOf gens)) gens

-- | Generate one of the given values (not uniformly — see the distribution
-- note on 'oneOf'). The list must be non-empty; passing @[]@ raises an error
-- at the call site.
element :: (HasCallStack) => [a] -> Gen a
element [] = throw (ValidationError Diagnostic {context = "Gen.element", detail = "used with empty list", values = [("choices", "0")], callStack = callStack})
element xs = Draw (spanLabel LabelSampledFrom) \tc -> do
  i <- drawInteger tc 0 (toInteger (Vector.length values - 1))
  pure (Vector.unsafeIndex values (fromInteger i))
  where
    -- Bound here, not inside the lambda, so the vector is built once per
    -- generator rather than on every draw.
    values = Vector.fromList xs

-- | Wrap a generator so that computing its span label terminates when it
-- appears on a recursive edge. Without 'defer', a self-referential generator
-- causes a @\<\<loop\>\>@ exception when it is constructed.
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
-- Every deferred value shares one span label, whatever it wraps, because
-- reading the wrapped generator's label would recurse forever on exactly
-- the edges 'defer' exists for.
defer :: Gen a -> Gen a
defer g = Draw (spanLabel LabelDeferred) \tc -> drawInline tc g

-- | Choose one of the given generators, weighted by the accompanying 'Int'.
--
-- The list must be non-empty and all weights must be positive; violations
-- throw 'ValidationError' when evaluated.
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
frequency [] = throw (ValidationError Diagnostic {context = "Gen.frequency", detail = "used with empty list", values = [("choices", "0")], callStack = callStack})
frequency pairs
  | any ((<= 0) . fst) pairs = throw (ValidationError Diagnostic {context = "Gen.frequency", detail = "all weights must be positive", values = [("weights", T.pack (show (map fst pairs)))], callStack = callStack})
  | otherwise = Draw (combineLabels (spanLabel LabelOneOf : fmap (labelOf . snd) pairs)) \tc -> do
      i <- drawInteger tc 0 (total - 1)
      draw tc (prefixSelect i weighted)
  where
    weighted = [(toInteger w, g) | (w, g) <- pairs]
    total = foldl' (\acc (w, _) -> acc + w) 0 weighted

-- | Select the branch containing an index in the total positive weight range.
prefixSelect :: Integer -> [(Integer, a)] -> a
prefixSelect _ [] = throw InvariantViolation {detail = "Gen.frequency: index exceeds the total weight"}
prefixSelect n ((w, g) : rest)
  | n < w = g
  | otherwise = prefixSelect (n - w) rest

-- | Generate either 'Nothing' or 'Just' a value from the given generator.
maybe :: Gen a -> Gen (Maybe a)
maybe g = OneOf (combineLabels [spanLabel LabelOptional, labelOf g]) [pure Nothing, Just <$> g]

-- | Generate a 'Left' value from the first generator or a 'Right' value
-- from the second.
either :: Gen a -> Gen b -> Gen (Either a b)
either ga gb = oneOf [Left <$> ga, Right <$> gb]

-- $exceptions
-- 'AssumeRejected' is re-exported from 'Hegel.Internal.Control', as it is used for
-- control flow within the runner rather than for surfacing test failures.
