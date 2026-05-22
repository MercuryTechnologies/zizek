module Hegel.Generators
  ( Generator (..)
  , BasicGenerator (..)
  , draw
  ) where

import CBOR.Value (Value)
import Control.Exception (throwIO)
import Hegel.DataSource (generate)
import Hegel.Protocol.Cbor (ParseError)
import Hegel.TestCase (TestCase (..))

data BasicGenerator a = BasicGenerator
  { schema :: !Value
  , parse  :: Value -> Either ParseError a
  }

instance Functor BasicGenerator where
  fmap f bg = bg {parse = fmap f . bg.parse}

class Generator g where
  {-# MINIMAL asBasic | doDraw #-}
  type Output g
  doDraw  :: g -> TestCase -> IO (Output g)
  asBasic :: g -> Maybe (BasicGenerator (Output g))

  doDraw g tc =
    case asBasic g of
      Just bg -> do
        raw <- generate tc.dataSource bg.schema
        case bg.parse raw of
          Right a  -> pure a
          Left err -> throwIO err
      Nothing -> fail "Generator: no doDraw implementation and asBasic returned Nothing"

  asBasic _ = Nothing

draw :: (Generator g) => TestCase -> g -> IO (Output g)
draw = flip doDraw
