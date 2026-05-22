{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module Hegel.Generators
  ( Generator,
    pattern Schema,
    draw,
    filtered,
  )
where

import CBOR.Value (Value)
import Control.Exception (throwIO)
import Hegel.DataSource (Label (..), generate, startSpan, stopSpan)
import Hegel.Protocol.Cbor (ParseError)
import Hegel.TestCase (TestCase (..))

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
  Map :: (b -> a) -> Generator b -> Generator a
  Bind :: Generator b -> (b -> Generator a) -> Generator a
  Filter :: (a -> Bool) -> Generator a -> Generator a

pattern Schema :: Value -> (Value -> Either ParseError a) -> Generator a
pattern Schema s p = Basic (BasicGenerator s p)

instance Functor Generator where
  fmap f (Basic bg) = Basic (fmap f bg)
  fmap f (Map g x) = Map (f . g) x
  fmap f g = Map f g

instance Applicative Generator where
  pure = Pure
  gf <*> ga = Bind gf (\f -> Bind ga (\a -> Pure (f a)))

instance Monad Generator where
  (>>=) = Bind

runGenerator :: TestCase -> Generator a -> IO a
runGenerator _ (Pure a) = pure a
runGenerator tc (Basic bg) = do
  raw <- generate tc.dataSource bg.schema
  case bg.parse raw of
    Right a -> pure a
    Left err -> throwIO err
runGenerator tc (Draw f) = f tc
runGenerator tc (Map f g) = f <$> runGenerator tc g
runGenerator tc (Bind (Pure a) f) = runGenerator tc (f a)
runGenerator tc (Bind g f) = do
  startSpan tc.dataSource LabelFlatMap
  a <- runGenerator tc g
  b <- runGenerator tc (f a)
  stopSpan tc.dataSource False
  pure b
runGenerator tc (Filter p g) = go (3 :: Int)
  where
    go 0 = error "Generator: filter exhausted all attempts (TODO: signal Invalid)"
    go n = do
      startSpan tc.dataSource LabelFilter
      v <- runGenerator tc g
      if p v
        then stopSpan tc.dataSource False *> pure v
        else stopSpan tc.dataSource True *> go (n - 1)

draw :: TestCase -> Generator a -> IO a
draw = runGenerator

filtered :: (a -> Bool) -> Generator a -> Generator a
filtered = Filter
