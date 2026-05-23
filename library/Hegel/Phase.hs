module Hegel.Phase
  ( Phase (..),
    toWire,
  )
where

import Data.Text (Text)

data Phase
  = Explicit
  | Reuse
  | Generate
  | Target
  | Shrink
  deriving stock (Show, Eq)

toWire :: Phase -> Text
toWire Explicit = "explicit"
toWire Reuse = "reuse"
toWire Generate = "generate"
toWire Target = "target"
toWire Shrink = "shrink"
