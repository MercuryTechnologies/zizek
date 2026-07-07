{-# LANGUAGE CPP #-}

-- | Per-test-case control-flow signals.
--
-- These are /control signals/, not errors: they carry the runner's verdict for
-- the current test case out of the middle of a draw, up through the property
-- body, to 'Hegel.Runner.runTestCase', which classifies them.
--
-- They are thrown as /asynchronous/ exceptions so a catch-all in the test body
-- cannot silently swallow one and corrupt the run; 'isControlSignal'
-- recognizes them in handlers that legitimately need to.
module Hegel.Internal.Control
  ( TestStopped (..),
    AssumeRejected (..),
    MalformedTest (..),
    FinalizerFailed (..),
    NoBacktrace (..),
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
#if __GLASGOW_HASKELL__ >= 912
import Control.Exception (NoBacktrace (..))
#endif
import Control.Monad (when)
import Data.List (intercalate)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import UnliftIO.Exception (isSyncException)

#if __GLASGOW_HASKELL__ < 912
-- | Compatibility shim for @base < 4.21@ (GHC < 9.12), which lacks the
-- 'Control.Exception.NoBacktrace' wrapper.
--
-- On GHC 9.12+, 'throwIO' attaches a backtrace annotation by default and
-- 'NoBacktrace' suppresses it.
--
-- On older GHCs there is no automatic backtrace collection, so this delegates
-- 'toException'\/'fromException' instances to the exception it wraps.
newtype NoBacktrace e = NoBacktrace e

instance (Show e) => Show (NoBacktrace e) where
  showsPrec p (NoBacktrace e) = showsPrec p e

instance (Exception e) => Exception (NoBacktrace e) where
  toException (NoBacktrace e) = toException e
  fromException = fmap NoBacktrace . fromException
  displayException (NoBacktrace e) = displayException e
#endif

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

#if __GLASGOW_HASKELL__ >= 912
  -- Suppress backtrace collection: thrown on every budget stop; nothing is
  -- ever rendered from it.
  backtraceDesired _ = False
#endif

-- | Thrown when a test case is deliberately discarded, either via
-- 'Hegel.Property.assume' or 'Hegel.Property.discard', or by an exhausted
-- 'Hegel.Gen.filtered'\/'Hegel.Gen.mapMaybe' retry budget.
data AssumeRejected = AssumeRejected
  deriving stock (Show)

instance Exception AssumeRejected where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

#if __GLASGOW_HASKELL__ >= 912
  -- Suppress backtrace collection: thrown on every discard; nothing is ever
  -- rendered from it.
  backtraceDesired _ = False
#endif

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

-- | Thrown when one or more registered finalizers
-- ('Hegel.Property.registerFinalizer') failed while draining at the case
-- boundary.
--
-- A finalizer restores the per-case isolation the run's soundness depends on,
-- so a failed teardown means later cases and shrink replays can no longer be
-- trusted.
--
-- The runner drains finalizers /outside/ the per-case classifier and lets this
-- escape to 'Hegel.Runner.check', which reports it as 'Hegel.Report.Aborted'.
--
-- This exception carries the origin text from a failed property run, if one
-- occurred at the time the finalizer was run, so the report doesn't hide that
-- a property failure was in-hand. When a database is enabled the engine has
-- already persisted that counterexample's reproduction blob (inside
-- @markComplete@, before the drain runs), so the next run replays it; under the
-- default settings (database disabled) the drawn values are not recoverable.
data FinalizerFailed = FinalizerFailed (Maybe Text) [SomeException]

instance Show FinalizerFailed where
  show (FinalizerFailed origin es) =
    "FinalizerFailed " <> show origin <> " " <> show (map displayException es)

instance Exception FinalizerFailed where
  displayException (FinalizerFailed origin es) =
    "finalizer(s) failed: "
      <> intercalate "; " (map displayException es)
      <> case origin of
        Nothing -> ""
        Just o -> " (the case had already failed at " <> T.unpack o <> ")"

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
-- honored: @unliftio@\/@safe-exceptions@ combinators rethrow async exceptions
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
  --
  -- Suppress backtrace collection ('NoBacktrace'): rethrown on every caught
  -- control signal; 'backtraceDesired' on the concrete signal types is
  -- bypassed here because the async 'SomeAsyncException' wrapper answers for
  -- them ('True') on a 'SomeException' rethrow.
  act `catch` \(e :: SomeException) -> do
    when (isFailure e) (hook e)
    throwIO (NoBacktrace e)
