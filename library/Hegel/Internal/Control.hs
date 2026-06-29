-- | Per-test-case control-flow signals.
--
-- __Internal module.__ Implementation substrate of @zizek@ itself, exposed so
-- you can reach past the public API when you must; it is not part of the
-- stable public interface and may change without notice.
--
-- These are /control signals/, not errors: they carry the runner's verdict for
-- the current test case out of the middle of a draw, up through the property
-- body, to 'Hegel.Runner.runTestCase', which classifies them. They are thrown
-- as /asynchronous/ exceptions so a catch-all in the test body cannot silently
-- swallow one and corrupt the run; 'isControlSignal' recognises them in
-- handlers that legitimately need to.
module Hegel.Internal.Control
  ( TestStopped (..),
    AssumeRejected (..),
    isControlSignal,
  )
where

import Control.Exception
  ( Exception (..),
    SomeException,
    asyncExceptionFromException,
    asyncExceptionToException,
  )
import Data.Maybe (isJust)

-- | Thrown when the engine signals that the current test case should be
-- abandoned (choice budget exhausted).
--
-- The runner treats this as a discard.
data TestStopped = TestStopped
  deriving stock (Show)

instance Exception TestStopped where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

-- | Thrown when a test case is deliberately discarded, either via
-- 'Hegel.Property.assume' or 'Hegel.Property.discard', or by an exhausted
-- 'Hegel.Gen.filtered'\/'Hegel.Gen.mapMaybe' retry budget.
data AssumeRejected = AssumeRejected
  deriving stock (Show)

instance Exception AssumeRejected where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

-- | Is this exception one of Hegel's control signals ('AssumeRejected' or
-- 'TestStopped')?
--
-- These signals are thrown as asynchronous exceptions, so handlers that need
-- to catch them may use this predicate to recognize them.
isControlSignal :: SomeException -> Bool
isControlSignal e =
  isJust (fromException @AssumeRejected e)
    || isJust (fromException @TestStopped e)
