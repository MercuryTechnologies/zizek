module Hegel
  ( runProperty,
    runProperty_,
    module Hegel.Client,
    module Hegel.Outcome,
    module Hegel.Phase,
    module Hegel.Protocol.Error,
    module Hegel.Runner,
    module Hegel.Session,
  )
where

import Control.Exception (throwIO)
import Hegel.Client
import Hegel.Gen.Internal (Generator)
import Hegel.Outcome
import Hegel.Phase
import Hegel.Protocol.Error
import Hegel.Runner
import Hegel.Session

runProperty ::
  Settings ->
  Generator a ->
  (a -> IO ()) ->
  IO (Outcome a)
runProperty = runPropertyOn globalSession

runProperty_ ::
  (Show a) =>
  Settings ->
  Generator a ->
  (a -> IO ()) ->
  IO ()
runProperty_ settings gen body = do
  outcome <- runPropertyOn globalSession settings gen body
  case outcome of
    Passed _ -> pure ()
    Failed {counterexample, message, notes} -> throwIO (PropertyFailed counterexample message notes)
    Errored exc -> throwIO exc
    Rejected msg -> fail ("Property rejected all inputs: " <> show msg)
    UnhealthyInput msg -> fail ("Health check failed: " <> show msg)
