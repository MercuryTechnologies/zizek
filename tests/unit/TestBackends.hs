-- | The backends exercised by the unit suite.
-- Both native and server are always compiled; no flag or swap needed.
module TestBackends (backends) where

import Hegel.Native.Runner qualified as Native
import Hegel.Server.Runner qualified as Server
import TestRunner (Checker (..), Runner (..))

backends :: [(String, Runner, Checker)]
backends =
  [ ("native", Runner Native.runProperty, Checker Native.check),
    ("server", Runner Server.runProperty, Checker Server.check)
  ]
