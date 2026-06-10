-- | The backends exercised by the unit suite.
module TestBackends (backends) where

import Hegel.Property (forEach)
import Hegel.Runner qualified as Native
import TestRunner (Checker (..), Runner (..))

backends :: [(String, Runner, Checker)]
backends =
  [ ("native", Runner (\s g b -> Native.check s (forEach g b)), Checker Native.check)
  ]
