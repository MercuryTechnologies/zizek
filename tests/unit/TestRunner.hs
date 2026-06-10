-- | Local runner abstraction for multi-backend test parameterisation.
module TestRunner
  ( Runner (..),
    runWith,
    runWith_,
    Checker (..),
    checkWith,
  )
where

import Hegel.Gen.Internal (Gen)
import Hegel.Property (Property)
import Hegel.Report (Report, throwOnFailure)
import Hegel.Settings (Settings)

-- | A bundled property-run function, parameterised over the backend.
-- Use 'runWith' / 'runWith_' rather than unwrapping directly.
newtype Runner = Runner
  { runProperty :: forall a. (Show a) => Settings -> Gen a -> (a -> IO ()) -> IO Report
  }

-- | A bundled property-check function, parameterised over the backend.
newtype Checker = Checker
  { check :: Settings -> Property () -> IO Report
  }

-- | Unwrap the checker and invoke it.
checkWith :: Checker -> Settings -> Property () -> IO Report
checkWith (Checker c) = c

-- | Unwrap the runner and invoke it.
runWith :: (Show a) => Runner -> Settings -> Gen a -> (a -> IO ()) -> IO Report
runWith (Runner rp) = rp

-- | Run a property and throw on anything other than success.
runWith_ :: (Show a) => Runner -> Settings -> Gen a -> (a -> IO ()) -> IO ()
runWith_ (Runner rp) settings gen body = throwOnFailure =<< rp settings gen body
