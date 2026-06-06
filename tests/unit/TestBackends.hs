-- | The backends exercised by the unit suite.
-- Both native and server are always compiled; no flag or swap needed.
module TestBackends (backends) where

import Hegel.Native.Runner qualified as Native
import Hegel.Server.Runner qualified as Server
import TestRunner (Runner (..))

backends :: [(String, Runner)]
backends =
  [ ("native", Runner Native.runProperty),
    ("server", Runner Server.runProperty)
  ]
