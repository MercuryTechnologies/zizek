module Hegel
  ( runProperty,
    runProperty_,
    closeSession,
    module Hegel.Outcome,
    module Hegel.Phase,
    module Hegel.Protocol.Error,
    module Hegel.Runner,
  )
where

import Control.Exception (throwIO)
import Hegel.Generators (Generator)
import Hegel.Outcome
import Hegel.Phase
import Hegel.Protocol.Error
import Hegel.Runner
import Hegel.Session (closeSession)

runProperty ::
  Settings ->
  Generator a ->
  (a -> IO ()) ->
  IO (Outcome a)
runProperty = runPropertyWith

runProperty_ ::
  (Show a) =>
  Settings ->
  Generator a ->
  (a -> IO ()) ->
  IO ()
runProperty_ settings gen body = do
  outcome <- runPropertyWith settings gen body
  case outcome of
    Passed _ -> pure ()
    Failed {counterexample, message, notes} -> throwIO (PropertyFailed counterexample message notes)
    Errored exc -> throwIO exc
    Rejected msg -> fail ("Property rejected all inputs: " <> show msg)
    UnhealthyInput msg -> fail ("Health check failed: " <> show msg)
