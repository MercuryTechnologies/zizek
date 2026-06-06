-- | Local runner abstraction for multi-backend test parameterisation.
module TestRunner
  ( Runner (..),
    runWith,
    runWith_,
  )
where

import Control.Exception (throwIO)
import Hegel.Gen.Internal (Gen)
import Hegel.Outcome (Outcome (..), PropertyFailed (..))
import Hegel.Settings (Settings)

-- | A bundled property-run function, parameterised over the backend.
-- Use 'runWith' / 'runWith_' rather than unwrapping directly.
newtype Runner = Runner
  { runProperty :: forall a. Settings -> Gen a -> (a -> IO ()) -> IO (Outcome a)
  }

-- | Unwrap the runner and invoke it.
runWith :: Runner -> Settings -> Gen a -> (a -> IO ()) -> IO (Outcome a)
runWith (Runner rp) = rp

-- | Run a property and throw on anything other than success.
runWith_ :: (Show a) => Runner -> Settings -> Gen a -> (a -> IO ()) -> IO ()
runWith_ (Runner rp) settings gen body = do
  outcome <- rp settings gen body
  case outcome of
    Passed _ -> pure ()
    Failed {counterexample, message, notes} -> throwIO (PropertyFailed counterexample message notes)
    Errored exc -> throwIO exc
    Rejected msg -> fail ("Property rejected all inputs: " <> show msg)
    UnhealthyInput msg -> fail ("Health check failed: " <> show msg)
