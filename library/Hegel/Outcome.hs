module Hegel.Outcome
  ( Outcome (..),
    Stats (..),
    PropertyFailed (..),
  )
where

import Control.Exception (Exception, SomeException)
import Data.Text (Text)

data Stats = Stats
  { testsRun :: !Int,
    invalid :: !Int
  }
  deriving stock (Show)

data Outcome a
  = Passed Stats
  | Failed {counterexample :: !a, message :: !Text, notes :: ![Text]}
  | Errored SomeException
  | Rejected Text
  | UnhealthyInput Text
  deriving stock (Show)

data PropertyFailed = forall a. (Show a) => PropertyFailed a Text [Text]

instance Show PropertyFailed where
  show (PropertyFailed cex msg _) =
    "property failed with counterexample " <> show cex <> ": " <> show msg

instance Exception PropertyFailed
