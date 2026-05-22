module Hegel
  ( runProperty
  , runProperty_
  , module Hegel.Outcome
  , module Hegel.Runner
  ) where

import Control.Exception (throwIO)
import Hegel.Generators (Generator)
import Hegel.Outcome
import Hegel.Runner

runProperty
  :: Settings
  -> Generator a
  -> (a -> IO ())
  -> IO (Outcome a)
runProperty = runPropertyWith

runProperty_
  :: Show a
  => Settings
  -> Generator a
  -> (a -> IO ())
  -> IO ()
runProperty_ settings gen body = do
  outcome <- runPropertyWith settings gen body
  case outcome of
    Passed _             -> pure ()
    Failed cex msg notes -> throwIO (PropertyFailed cex msg notes)
    Errored exc          -> throwIO exc
    Rejected msg         -> fail ("Property rejected all inputs: " <> show msg)
    UnhealthyInput msg   -> fail ("Health check failed: " <> show msg)
