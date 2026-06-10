-- | The backends exercised by the unit suite.
module TestBackends (backends) where

import Hegel.Native.Runner qualified as Native
import TestRunner (Checker (..), Runner (..))

backends :: [(String, Runner, Checker)]
backends =
  [ ("native", Runner Native.runProperty, Checker Native.check)
  ]
