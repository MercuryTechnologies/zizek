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
    MalformedTest (..),
    isControlSignal,
    ControlSignal (..),
    catchControl,
    isFailure,
    onFailure,
  )
where

import Control.Exception
  ( Exception (..),
    Handler (..),
    SomeException,
    asyncExceptionFromException,
    asyncExceptionToException,
    catch,
    catches,
    throwIO,
  )
import Control.Monad (when)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import UnliftIO.Exception (isSyncException)

-- | Thrown when the engine signals that the current test case should be
-- abandoned (choice budget exhausted).
--
-- The runner reports this as 'Hegel.Internal.TestCase.Overrun' — budget
-- exhaustion, distinct from a discard.
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

-- | Thrown when a test is structurally invalid — a precondition on the test
-- /definition/ rather than a property failure (for example, a stateful
-- 'Hegel.Stateful.Machine' with no rules).
--
-- The runner reports this as 'Hegel.Report.Aborted', keeping "the test was
-- built wrong" distinct from "the property found a counterexample". Unlike the
-- control signals above, it is an ordinary synchronous exception.
newtype MalformedTest = MalformedTest Text
  deriving stock (Show)

instance Exception MalformedTest where
  displayException (MalformedTest msg) = T.unpack msg

-- | Is this exception one of Hegel's control signals ('AssumeRejected' or
-- 'TestStopped')?
--
-- These signals are thrown as asynchronous exceptions, so handlers that need
-- to catch them may use this predicate to recognize them.
isControlSignal :: SomeException -> Bool
isControlSignal e =
  isJust (fromException @AssumeRejected e)
    || isJust (fromException @TestStopped e)

-- | Discriminated form of one of Hegel's two control signals.
--
-- Used with 'catchControl' when a handler needs to know /which/ signal fired,
-- not merely whether one did.
data ControlSignal
  = -- | Corresponds to 'AssumeRejected': the current test case should be
    -- discarded (assume\/filter failure, empty pool, etc.).
    Assume
  | -- | Corresponds to 'TestStopped': the engine's choice budget is exhausted.
    Stop
  deriving stock (Show, Eq)

-- | Catch only Hegel's async control signals ('AssumeRejected' \/ 'TestStopped'),
-- discriminate which one fired, and let every other exception propagate.
--
-- Uses base 'Control.Exception.catches' so the async tagging of the signals is
-- honoured: @unliftio@\/@safe-exceptions@ combinators rethrow async exceptions
-- and therefore cannot be used to catch these.
catchControl :: IO a -> (ControlSignal -> IO a) -> IO a
catchControl act h =
  act
    `catches` [ Handler \AssumeRejected -> h Assume,
                Handler \TestStopped -> h Stop
              ]

-- | Is this exception a /failure/: a synchronous exception that is not one of
-- Hegel's control signals?
--
-- These are exactly the exceptions the runner classifies as Interesting (a
-- counterexample).
--
-- Note that 'MalformedTest' also satisfies this predicate: callers adding
-- hooks to failures for reporting purposes may act on it harmlessly, since
-- aborted runs never render a journal.
isFailure :: SomeException -> Bool
isFailure e = isSyncException e && not (isControlSignal e)

-- | Like 'Control.Exception.onException', but the hook sees the exception and
-- fires only for failures (per 'isFailure').
--
-- The exception is /always/ rethrown, via 'throwIO' to preserve sync vs. async
-- classification, so control signals and genuine async exceptions propagate
-- untouched past the hook.
--
-- Compose 'onFailure' __inside__ 'catchControl':
--
-- > (act `onFailure` hook) `catchControl` handler
--
-- ...so that control signals pass through the hook untouched and are still
-- discriminated against within 'catchControl'.
--
-- __NOTE__: The hook must not throw; an exception escaping the hook would
-- replace the original failure.
onFailure :: IO a -> (SomeException -> IO ()) -> IO a
onFailure act hook =
  -- Base 'catch'/'throwIO': unliftio combinators rethrow async exceptions
  -- before a handler could run, and base 'throwIO' preserves the
  -- 'asyncExceptionToException' wrapping on rethrow.
  act `catch` \(e :: SomeException) -> do
    when (isFailure e) (hook e)
    throwIO e
