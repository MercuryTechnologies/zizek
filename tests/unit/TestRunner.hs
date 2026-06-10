-- | Local runner abstraction for multi-backend test parameterisation.
module TestRunner
  ( Runner (..),
    runWith,
    runWith_,
  )
where

import Control.Exception (throwIO)
import Data.Text qualified as T
import Hegel.Gen.Internal (Gen)
import Hegel.Report (Abort (..), PropertyFailed (..), Report (..), Result (..))
import Hegel.Settings (Settings)

-- | A bundled property-run function, parameterised over the backend.
-- Use 'runWith' / 'runWith_' rather than unwrapping directly.
newtype Runner = Runner
  { runProperty :: forall a. Settings -> Gen a -> (a -> IO ()) -> IO (Report a)
  }

-- | Unwrap the runner and invoke it.
runWith :: Runner -> Settings -> Gen a -> (a -> IO ()) -> IO (Report a)
runWith (Runner rp) = rp

-- | Run a property and throw on anything other than success.
runWith_ :: (Show a) => Runner -> Settings -> Gen a -> (a -> IO ()) -> IO ()
runWith_ (Runner rp) settings gen body = do
  report <- rp settings gen body
  case report.result of
    Ok -> pure ()
    Counterexample {value, message, notes} ->
      throwIO PropertyFailed {counterexample = T.pack (show value), message, notes}
    GaveUp msg -> fail ("Property rejected all inputs: " <> show msg)
    Aborted (Errored exc) -> throwIO exc
    Aborted (UnhealthyInput msg) -> fail ("Health check failed: " <> show msg)
