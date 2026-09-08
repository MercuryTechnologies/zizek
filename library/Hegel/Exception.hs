{-# LANGUAGE CPP #-}

-- | Structured diagnostics for invalid generators, settings, and tests.
module Hegel.Exception
  ( Diagnostic (..),
    ValidationError (..),
    SettingsError (..),
    MalformedTest (..),
    HegelError (..),
    InvariantViolation (..),
  )
where

import Control.Exception (Exception (..))
import Data.Text (Text)
import Data.Text qualified as T
import Foreign.C.Types (CInt)
import GHC.Stack (CallStack, prettyCallStack)

-- | An operation's invalid configuration and the call site that supplied it.
data Diagnostic = Diagnostic
  { context :: !Text,
    detail :: !Text,
    values :: ![(Text, Text)],
    callStack :: !CallStack
  }
  deriving stock (Show)

-- | Invalid generator configuration, reported as a shrinkable counterexample.
newtype ValidationError = ValidationError Diagnostic
  deriving stock (Show)

-- | Invalid run settings, rejected before engine setup.
newtype SettingsError = SettingsError Diagnostic
  deriving stock (Show)

-- | Invalid test structure, which aborts exploration.
newtype MalformedTest = MalformedTest Diagnostic
  deriving stock (Show)

instance Exception ValidationError where
  displayException (ValidationError d) = renderDiagnostic "ValidationError" d

instance Exception SettingsError where
  displayException (SettingsError d) = renderDiagnostic "SettingsError" d

instance Exception MalformedTest where
  displayException (MalformedTest d) = renderDiagnostic "MalformedTest" d

renderDiagnostic :: Text -> Diagnostic -> String
renderDiagnostic category d =
  T.unpack $
    category
      <> ": "
      <> d.context
      <> ": "
      <> d.detail
      <> foldMap (\(name, value) -> "\n  " <> name <> " = " <> value) d.values
      <> "\n"
      <> T.pack (prettyCallStack d.callStack)

-- | An engine call failed with a raw error code and optional diagnostic.
data HegelError = HegelError
  { code :: !CInt,
    message :: !(Maybe Text)
  }
  deriving stock (Show)

#if __GLASGOW_HASKELL__ >= 912
instance Exception HegelError where
  backtraceDesired _ = False
#else
instance Exception HegelError
#endif

-- | The engine returned a value that violates its protocol.
newtype InvariantViolation = InvariantViolation {detail :: Text}
  deriving stock (Show)

instance Exception InvariantViolation
