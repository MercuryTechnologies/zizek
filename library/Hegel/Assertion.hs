-- | Call-stack-aware assertion helpers for property bodies.
--
-- 'assert' and 'failure' are polymorphic in any 'MonadIO', so the same
-- functions work in a plain @IO@ property body and inside
-- 'Hegel.Property.PropertyT'.
module Hegel.Assertion
  ( AssertionFailure (..),
    assert,
    failure,
    originOf,
    callSite,
  )
where

import Control.Exception (Exception (displayException), SomeException (SomeException), fromException, throwIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
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

-- | Raised by 'assert' and 'failure'. Carries a user-supplied message and a
-- captured 'CallStack' so the runner can extract a stable @file:line@ origin
-- for failure deduplication on the server.
data AssertionFailure = AssertionFailure
  { message :: !Text,
    callStack :: !CallStack
  }
  deriving stock (Show)

instance Exception AssertionFailure where
  displayException f = T.unpack f.message <> "\n" <> prettyCallStack f.callStack

-- | Fail the current property with a message, capturing the call site.
failure :: (HasCallStack, MonadIO m) => Text -> m a
failure msg =
  withFrozenCallStack $
    liftIO (throwIO AssertionFailure {message = msg, callStack = Stack.callStack})

-- | Assert a condition; on 'False', fail with the given message and the
-- captured call site.
assert :: (HasCallStack, MonadIO m) => Bool -> Text -> m ()
assert cond msg = unless cond (withFrozenCallStack (failure msg))

-- | The innermost call site recorded in a 'CallStack', if any.
callSite :: CallStack -> Maybe SrcLoc
callSite cs = case getCallStack cs of
  (_, sl) : _ -> Just sl
  [] -> Nothing

-- | Format an exception as @\<ExcTypeName\> at \<file\>:\<line\>@ for use as
-- the @origin@ field in @mark_complete INTERESTING@.
--
-- The server uses @origin@ as a deduplication key, so this string must NOT
-- contain the error message (which typically embeds the failing generated
-- value) or the full stack trace. Mirrors the contract enforced by
-- @OriginDeduplicationConformance@ in @hegel-core@.
--
-- For 'AssertionFailure', the @file:line@ comes from the innermost
-- 'CallStack' frame. For all other exceptions we don't have a Haskell
-- traceback, so we emit @\<unknown\>:0@ — dedup remains correct by exception
-- type.
originOf :: SomeException -> Text
originOf exc = case fromException exc of
  Just AssertionFailure {callStack = cs} -> formatWithStack (typeName exc) cs
  Nothing -> typeName exc <> " at <unknown>:0"
  where
    typeName (SomeException e) = T.pack (show (typeOf e))

    formatWithStack tn cs = case callSite cs of
      Nothing -> tn <> " at <unknown>:0"
      Just sl ->
        tn <> " at " <> T.pack sl.srcLocFile <> ":" <> T.pack (show sl.srcLocStartLine)
