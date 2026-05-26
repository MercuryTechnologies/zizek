{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Core generator machinery.
--
-- Enable @ApplicativeDo@ in modules that build generators to get better
-- shrinking for independent draws.
module Hegel.Gen.Internal
  ( -- * Generator type
    -- $generator
    Generator,
    BasicGenerator (..),
    pattern Schema,

    -- * Combinators
    -- $combinators
    draw,
    assume,
    filtered,
    oneOf,

    -- * Exceptions
    -- $exceptions
    InvalidTestCase (..),
    UnexpectedResponse (..),
  )
where

import CBOR.Value (Value (Array, NInt, Null, UInt))
import Control.Exception (Exception, throwIO)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Vector qualified as V
import Hegel.DataSource
  ( Label (..),
    Status (..),
    generate,
    markComplete,
    startSpan,
    stopSpan,
  )
import Hegel.Protocol.Cbor
  ( ParseError (..),
    buildMap,
    intVal,
    textVal,
  )
import Hegel.TestCase (TestCase (..))

-- $generator
-- A 'Generator' describes how to produce a value for a test case; generators
-- compose via the standard 'Functor', 'Applicative', and 'Monad' interfaces.

-- | A 'Generator' that can be expressed as a single schema request to
-- @hegel-core@. Construct one with the 'Schema' pattern synonym.
--
-- The @schema@ field is the CBOR schema sent to the server; @parse@ converts
-- the server's response back to @a@.
data BasicGenerator a = BasicGenerator
  { schema :: !Value,
    parse :: Value -> Either ParseError a
  }

instance Functor BasicGenerator where
  fmap f bg = bg {parse = fmap f . bg.parse}

data Generator a where
  Pure :: a -> Generator a
  Basic :: BasicGenerator a -> Generator a
  Draw :: (TestCase -> IO a) -> Generator a
  Map :: !(Maybe (BasicGenerator a)) -> (b -> a) -> Generator b -> Generator a
  Ap :: !(Maybe (BasicGenerator a)) -> Generator (b -> a) -> Generator b -> Generator a
  Bind :: Generator b -> (b -> Generator a) -> Generator a
  OneOf :: !(Maybe (BasicGenerator a)) -> NonEmpty (Generator a) -> Generator a

-- | Pattern synonym for constructing a 'BasicGenerator' directly from a CBOR
-- schema and a parse function. See 'Hegel.Gen.Integer.integer' for a
-- worked example.
pattern Schema :: Value -> (Value -> Either ParseError a) -> Generator a
pattern Schema s p = Basic (BasicGenerator s p)

unitSchema :: Value
unitSchema = buildMap [("type", textVal "constant"), ("value", Null)]

tupleSchema :: [Value] -> Value
tupleSchema elems =
  buildMap
    [ ("type", textVal "tuple"),
      ("elements", Array (V.fromList elems))
    ]

oneOfSchema :: NonEmpty Value -> Value
oneOfSchema gs =
  buildMap
    [ ("type", textVal "one_of"),
      ("generators", Array (V.fromList (NE.toList gs)))
    ]

-- Smart constructors that precompute the basic representation at build time.

mkMap :: (b -> a) -> Generator b -> Generator a
mkMap f (Pure a) = Pure (f a)
mkMap f (Basic bg) = Basic (fmap f bg)
mkMap f (Map _ g x) = mkMap (f . g) x
mkMap f g = Map (fmap f <$> toBasic g) f g

mkAp :: Generator (b -> a) -> Generator b -> Generator a
mkAp gf ga = Ap (basicAp gf ga) gf ga

-- | 'Pure' on either side is transparent: it contributes no schema element
-- and the combined generator reduces to a plain fmap of the other side.
basicAp :: Generator (b -> a) -> Generator b -> Maybe (BasicGenerator a)
basicAp (Pure f) ga = fmap f <$> toBasic ga
basicAp gf (Pure a) = fmap ($ a) <$> toBasic gf
basicAp gf ga = do
  bf <- toBasic gf
  ba <- toBasic ga
  let sch = tupleSchema [bf.schema, ba.schema]
      p (Array arr)
        | Just fv <- arr V.!? 0,
          Just av <- arr V.!? 1 = do
            f <- bf.parse fv
            a <- ba.parse av
            pure (f a)
      p v = Left ParseError {expected = "2-element array", got = v}
  pure (BasicGenerator sch p)

mkOneOf :: NonEmpty (Generator a) -> Generator a
mkOneOf gens = OneOf (basicOneOf gens) gens

basicOneOf :: NonEmpty (Generator a) -> Maybe (BasicGenerator a)
basicOneOf gens = do
  bs <- traverse toBasic gens
  let sch = oneOfSchema (fmap (.schema) bs)
      parsers = V.fromList (NE.toList (fmap (.parse) bs))
      p (Array arr)
        | Just idxV <- arr V.!? 0,
          Just val <- arr V.!? 1 = do
            i <- parseIndex idxV
            case parsers V.!? i of
              Just q -> q val
              Nothing ->
                Left
                  ParseError
                    { expected = "index < " <> T.pack (show (V.length parsers)),
                      got = idxV
                    }
      p v = Left ParseError {expected = "[index, value] array", got = v}
  pure (BasicGenerator sch p)

toBasic :: Generator a -> Maybe (BasicGenerator a)
toBasic (Basic bg) = Just bg
toBasic (Pure a) = Just (BasicGenerator unitSchema (\_ -> Right a))
toBasic (Map c _ _) = c
toBasic (Ap c _ _) = c
toBasic (OneOf c _) = c
toBasic _ = Nothing

instance Functor Generator where
  fmap = mkMap

-- | @('<*>')@ and @('>>=')@ have deliberately different semantics:
--
-- * @('<*>')@ treats draws as independent: @hegel-core@ gets to shrink each
--   component separately. When both sides are basic it collapses to a single
--   request; otherwise it wraps the draws in a @TUPLE@ span.
-- * @('>>=')@ treats draws as dependent: the second draw may vary with the
--   first, so the two are grouped in a @FLAT_MAP@ span.
instance Applicative Generator where
  pure = Pure
  (<*>) = mkAp

instance Monad Generator where
  (>>=) = Bind

runBasic :: TestCase -> BasicGenerator a -> IO a
runBasic tc bg = do
  raw <- generate tc.dataSource bg.schema
  case bg.parse raw of
    Right a -> pure a
    Left err -> throwIO UnexpectedResponse {sentSchema = bg.schema, received = raw, cause = err}

-- | Count non-'Pure' leaves in an 'Ap' spine, to decide whether a TUPLE span
-- is needed: fewer than 2 real draws don't require one.
apLeafCount :: Generator a -> Int
apLeafCount (Ap _ gf ga) =
  apLeafCount gf + case ga of
    Pure _ -> 0
    _ -> 1
apLeafCount (Pure _) = 0
apLeafCount _ = 1

-- | Recursively run the leaves of an @Ap@ spine left-to-right, applying
-- the accumulated function. Used for the non-basic fallback path.
runApSpine :: TestCase -> Generator a -> IO a
runApSpine tc (Ap _ gf ga) = do
  f <- runApSpine tc gf
  a <- runGenerator tc ga
  pure (f a)
runApSpine tc g = runGenerator tc g

runGenerator :: TestCase -> Generator a -> IO a
runGenerator _ (Pure a) = pure a
runGenerator tc g = case toBasic g of
  Just bg -> runBasic tc bg
  Nothing -> runInteractive tc g

runInteractive :: TestCase -> Generator a -> IO a
runInteractive tc (Draw f) = f tc
runInteractive tc (Map _ f g) = do
  startSpan tc.dataSource LabelMapped
  a <- runGenerator tc g
  stopSpan tc.dataSource False
  pure (f a)
runInteractive tc node@(Ap _ _ _)
  | apLeafCount node < 2 = runApSpine tc node
  | otherwise = do
      startSpan tc.dataSource LabelTuple
      a <- runApSpine tc node
      stopSpan tc.dataSource False
      pure a
runInteractive tc (Bind (Pure a) f) = runGenerator tc (f a)
runInteractive tc (Bind g f) = do
  startSpan tc.dataSource LabelFlatMap
  a <- runGenerator tc g
  b <- runGenerator tc (f a)
  stopSpan tc.dataSource False
  pure b
runInteractive tc (OneOf _ gens) = do
  let n = NE.length gens
      indexSchema =
        buildMap
          [ ("type", textVal "integer"),
            ("min_value", intVal (0 :: Int)),
            ("max_value", intVal (n - 1))
          ]
  startSpan tc.dataSource LabelOneOf
  raw <- generate tc.dataSource indexSchema
  i <- case parseIndex raw of
    Right k -> pure k
    Left err -> throwIO UnexpectedResponse {sentSchema = indexSchema, received = raw, cause = err}
  v <- runGenerator tc (NE.toList gens !! i)
  stopSpan tc.dataSource False
  pure v
runInteractive _ (Pure a) = pure a
runInteractive tc (Basic bg) = runBasic tc bg

parseIndex :: Value -> Either ParseError Int
parseIndex (UInt n) = Right (fromIntegral n)
parseIndex (NInt n) = Right (fromIntegral (negate (fromIntegral n :: Integer) - 1))
parseIndex v = Left ParseError {expected = "integer", got = v}

-- $combinators
-- Combinators for filtering and choosing between generators. Discarded test
-- cases are reported to @hegel-core@ as invalid rather than failing.

-- | Run a generator against a live test case, producing a value. May throw
-- 'InvalidTestCase' (via 'assume' or 'filtered') or 'UnexpectedResponse' if
-- the server returns a value that cannot be parsed.
draw :: TestCase -> Generator a -> IO a
draw = runGenerator

-- | Discard the current test case when the condition is 'False'. Use this to
-- enforce preconditions on generated values without counting the case as a
-- failure.
assume :: Bool -> Generator ()
assume True = Pure ()
assume False = Draw \tc -> do
  markComplete tc.dataSource Invalid
  throwIO InvalidTestCase

-- | Filter values drawn from a generator, retrying up to 3 times before
-- discarding the test case entirely. The retry cap prevents runaway rejection
-- sampling; exhaustion is treated the same as 'assume' 'False'.
filtered :: (a -> Bool) -> Generator a -> Generator a
filtered p g = Draw \tc -> go tc (3 :: Int)
  where
    go tc n = do
      startSpan tc.dataSource LabelFilter
      v <- runGenerator tc g
      if p v
        then stopSpan tc.dataSource False *> pure v
        else do
          stopSpan tc.dataSource True
          if n > 1
            then go tc (n - 1)
            else markComplete tc.dataSource Invalid *> throwIO InvalidTestCase

-- | Choose unconditionally among the given generators, with @hegel-core@
-- driving the selection. When all alternatives are basic, a single request
-- is made with a @one_of@ schema; otherwise a bounded-index draw picks the
-- branch at runtime.
oneOf :: NonEmpty (Generator a) -> Generator a
oneOf = mkOneOf

-- $exceptions
-- Thrown as control flow rather than error signalling; the server is notified
-- before they are thrown so the runner can handle them cleanly.

-- | Thrown when a test case is deliberately discarded, either via 'assume'
-- or by an exhausted 'filtered'.
data InvalidTestCase = InvalidTestCase
  deriving stock (Show)

instance Exception InvalidTestCase

-- | Thrown when @hegel-core@ returns a value that cannot be parsed according
-- to the schema that was sent.
data UnexpectedResponse = UnexpectedResponse
  { -- | The CBOR schema sent to @hegel-core@.
    sentSchema :: !Value,
    -- | The raw value returned by the server.
    received :: !Value,
    -- | The parse failure.
    cause :: !ParseError
  }
  deriving stock (Show)

instance Exception UnexpectedResponse
