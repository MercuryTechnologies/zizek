-- | Call-stack-aware assertion helpers for property bodies.
module Hegel.Assertion
  ( AssertionFailure (..),
    assert,
    failure,
    (===),
    (/==),
    originOf,
    callSite,
  )
where

import Control.Exception (Exception (displayException), SomeException (SomeException), fromException)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (typeOf)
import GHC.Stack
  ( CallStack,
    HasCallStack,
    SrcLoc (..),
    getCallStack,
    prettyCallStack,
    withFrozenCallStack,
  )
import GHC.Stack qualified as Stack
import Hegel.Diff (Diff, diffLines, diffShown, renderDiff)
import Hegel.Report (renderValue)
import UnliftIO.Exception (throwIO)

-- | Raised by 'assert' and 'failure'.
--
-- Carries a user-supplied message, a captured 'CallStack', and an optional 'Diff'.
data AssertionFailure = AssertionFailure
  { message :: !Text,
    callStack :: !CallStack,
    -- | Structural or line-level diff produced by '(===)', if applicable.
    diff :: !(Maybe Diff)
  }
  deriving stock (Show)

instance Exception AssertionFailure where
  displayException f =
    T.unpack $
      f.message
        <> maybe "" (\d -> "\n" <> renderDiff d) f.diff
        <> "\n"
        <> T.pack (prettyCallStack f.callStack)

-- | Fail the current property with a message, capturing the call site.
--
-- Sets 'diff' to 'Nothing'; use '(===)' or '(/==)' to capture a 'Diff'.
failure :: (HasCallStack, MonadIO m) => Text -> m a
failure msg =
  withFrozenCallStack $
    throwIO
      AssertionFailure
        { message = msg,
          callStack = Stack.callStack,
          diff = Nothing
        }

-- | Assert a condition; on 'False', fail with the given message and the
-- captured call site.
assert :: (HasCallStack, MonadIO m) => Bool -> Text -> m ()
assert cond msg = unless cond (withFrozenCallStack (failure msg))

infix 4 ===, /==

-- | Assert two values are equal; on failure, capture a 'Diff' — structural
-- when both rendered values parse as valid Haskell, line-level otherwise.
(===) :: (HasCallStack, MonadIO m, Eq a, Show a) => a -> a -> m ()
x === y
  | x == y = pure ()
  | otherwise =
      withFrozenCallStack $
        throwIO
          AssertionFailure
            { message = "=== failed, values are not equal",
              callStack = Stack.callStack,
              diff =
                Just
                  ( fromMaybe
                      (diffLines (renderValue x) (renderValue y))
                      (diffShown (renderValue x) (renderValue y))
                  )
            }

-- | Assert two values differ.
(/==) :: (HasCallStack, MonadIO m, Eq a, Show a) => a -> a -> m ()
x /== y
  | x /= y = pure ()
  | otherwise =
      withFrozenCallStack $
        failure (T.intercalate "\n" ["/== failed, values are equal", renderValue x])

-- | The innermost call site recorded in a 'CallStack', if any.
callSite :: CallStack -> Maybe SrcLoc
callSite cs = case getCallStack cs of
  (_, sl) : _ -> Just sl
  [] -> Nothing

-- | Format an exception as @\<ExcTypeName\> at \<file\>:\<line\>@ for use as
-- the @origin@ field in @mark_complete INTERESTING@.
--
-- @libhegel@ uses @origin@ as a deduplication key, so this string must NOT
-- contain the error message (which typically embeds the failing generated
-- value) or the full stack trace.
--
-- For 'AssertionFailure', the @file:line@ comes from the innermost
-- 'CallStack' frame.
--
-- For all other exceptions we don't have a Haskell traceback, so we emit
-- @\<unknown\>:0@ and rely on the exception type for deduplication.
originOf :: SomeException -> Text
originOf exc = case fromException exc of
  Just AssertionFailure {callStack = cs} -> formatWithStack (typeName exc) cs
  Nothing -> typeName exc <> " at <unknown>:0"
  where
    typeName :: SomeException -> Text
    typeName (SomeException e) = T.pack (show (typeOf e))

    formatWithStack :: Text -> CallStack -> Text
    formatWithStack tn cs = case callSite cs of
      Nothing -> tn <> " at <unknown>:0"
      Just sl ->
        tn <> " at " <> T.pack sl.srcLocFile <> ":" <> T.pack (show sl.srcLocStartLine)
