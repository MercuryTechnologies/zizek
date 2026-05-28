-- | Result of a property run.
module Hegel.Outcome
  ( Outcome (..),
    Stats (..),
    PropertyFailed (..),
  )
where

import Control.Exception (Exception, SomeException)
import Data.Text (Text)

-- | Summary statistics for a property run.
data Stats = Stats
  { -- | Number of test cases the runner attempted.
    testsRun :: !Int,
    -- | How many cases were rejected as invalid (via 'Hegel.Gen.assume',
    -- 'Hegel.Gen.filtered', 'Hegel.Gen.discard', or 'Hegel.Gen.mapMaybe'
    -- exhaustion).
    invalid :: !Int
  }
  deriving stock (Show)

-- | What happened when a property was run.
data Outcome a
  = -- | Every attempted test case passed.
    Passed Stats
  | -- | A counterexample was found.
    Failed {counterexample :: !a, message :: !Text, notes :: ![Text]}
  | -- | An exception other than a property failure escaped the test.
    Errored SomeException
  | -- | No valid examples were generated.
    Rejected Text
  | -- | A health check failed before the property ran.
    UnhealthyInput Text
  deriving stock (Show)

-- | Counterexample wrapped for throwing from 'Hegel.runProperty_'. Carries
-- the counterexample value, the failure message, and any accumulated notes.
data PropertyFailed = forall a. (Show a) => PropertyFailed a Text [Text]

instance Show PropertyFailed where
  show (PropertyFailed cex msg _) =
    "property failed with counterexample " <> show cex <> ": " <> show msg

instance Exception PropertyFailed
