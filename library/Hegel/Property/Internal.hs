module Hegel.Property.Internal
  ( -- * Property monad
    PropertyT (..),
    Property,
    PropertyEnv (..),
    hoist,

    -- * Draws
    forAll,
    forAllWith,
    forAllSilent,

    -- * Notes
    annotate,
    annotateShow,
    footnote,

    -- * Discards
    assume,
    discard,

    -- * Runner hooks
    runPropertyT,
    propertyAction,
    observeProperty,
    failureDetails,
  )
where

import Control.Exception (SomeException, fromException)
import Control.Exception qualified as E
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Data.Foldable (toList)
import Data.Sequence ((|>))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Stack (HasCallStack, SrcLoc, callStack, withFrozenCallStack)
import Hegel.Assertion (AssertionFailure (..), callSite)
import Hegel.Diff (Diff)
import Hegel.Gen.Internal (AssumeRejected (..), Gen, draw)
import Hegel.Internal.Control (isControlSignal)
import Hegel.Internal.TestCase (TestCase)
import Hegel.Report (Note (..), NoteKind (..), renderValue)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (isSyncException)
import UnliftIO.IORef (modifyIORef', newIORef, readIORef)

-- | The per-test-case environment a property runs against:
--
-- * the 'TestCase'
-- * a journal that collects 'Note's
data PropertyEnv = PropertyEnv
  { testCase :: !TestCase,
    journal :: !(NoteKind -> Maybe SrcLoc -> Text -> IO ())
  }

-- | A property: test logic interleaved with generator draws against a live
-- test case.
--
-- An environment that allows for test logic to be interleaved with values
-- drawn from a 'TestCase'.
--
-- Unlike @'Hegel.Property.forEach' gen body@, where all draws happen up
-- front, a 'PropertyT' may draw ('forAll'), perform effects, and make
-- assertions in any order.
--
-- Failure is exception-based ('Hegel.Assertion.AssertionFailure' from
-- 'Hegel.Assertion.assert'\/'Hegel.Assertion.failure', or any other
-- exception), so assertions work unchanged under any transformer stack
-- layered on top.
--
-- __NOTE__: The entire body of a 'Property' is re-run on every shrink attempt,
-- and once more to reconstruct the failure report; effects must tolerate
-- repetition.
newtype PropertyT m a = PropertyT (ReaderT PropertyEnv m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFail, MonadUnliftIO)

-- | A property over 'IO', as consumed by the runners' @check@ entry points.
type Property = PropertyT IO

instance MonadTrans PropertyT where
  lift = PropertyT . lift

-- | Run a property in a different base monad.
--
-- > check settings (hoist (runAppM env) myProp)
hoist :: (forall x. m x -> n x) -> PropertyT m a -> PropertyT n a
hoist f (PropertyT (ReaderT g)) = PropertyT (ReaderT (f . g))

-- | Send a note to the journal.
note :: (MonadIO m) => NoteKind -> Maybe SrcLoc -> Text -> PropertyT m ()
note kind loc text = PropertyT do
  env <- ask
  liftIO (env.journal kind loc text)

-- | Draw a value from a generator mid-test.
--
-- The drawn value is rendered to the journal so it can show up in the failure
-- report.
forAll :: (HasCallStack, MonadIO m, Show a) => Gen a -> PropertyT m a
forAll = withFrozenCallStack (forAllWith renderValue)

-- | 'forAll' with an explicit renderer, for values without a 'Show'
-- instance (or with an unhelpful one).
forAllWith :: (HasCallStack, MonadIO m) => (a -> Text) -> Gen a -> PropertyT m a
forAllWith render gen = do
  a <- forAllSilent gen
  note Drawn (callSite callStack) (render a)
  pure a

-- | Draw a value without journaling it.
--
-- For bookkeeping draws that would only add noise to the report.
forAllSilent :: (MonadIO m) => Gen a -> PropertyT m a
forAllSilent gen = PropertyT do
  env <- ask
  liftIO (draw env.testCase gen)

-- | Attach context to the failure report, rendered at the point it was
-- recorded.
annotate :: (HasCallStack, MonadIO m) => Text -> PropertyT m ()
annotate = note Annotation (callSite callStack)

-- | 'annotate' a value via its 'Show' instance.
annotateShow :: (HasCallStack, MonadIO m, Show a) => a -> PropertyT m ()
annotateShow = withFrozenCallStack (annotate . renderValue)

-- | Attach context rendered after the report body.
footnote :: (MonadIO m) => Text -> PropertyT m ()
footnote = note Footnote Nothing

-- | Discard the current test case when the condition is 'False'.
--
-- Use this to enforce preconditions discovered mid-test; the case is reported
-- to the engine as invalid rather than failed.
assume :: (MonadIO m) => Bool -> m ()
assume cond = if cond then pure () else discard

-- NOTE: This function _needs_ to use 'Control.Exception.throwIO' so that
-- 'AssumeRejected' can be thrown as as a proper async exception.

-- | Discard the current test case unconditionally.
--
-- The discard signal is delivered as an asynchronous exception
-- ('Hegel.Internal.TestCase.AssumeRejected') so that catch-all handlers in the
-- property body may pass it through to the runner instead of silently ignoring
-- them.
--
-- __NOTE__: A bare 'Control.Exception.try' @\@SomeException@ will catch
-- asynchronous exceptions, which will produce undefined behavior from this
-- library.
discard :: (MonadIO m) => m a
discard = liftIO (E.throwIO AssumeRejected)

-- * Runner hooks

-- | Run a property against the given 'PropertyEnv'.
runPropertyT :: PropertyEnv -> PropertyT m a -> m a
runPropertyT env (PropertyT r) = runReaderT r env

-- | Lower a property to a per-case run loop.
--
-- Ordinary cases run with a no-op journal; failing cases are journaled later
-- via 'observeProperty' on the engine's minimal counterexample.
propertyAction :: Property () -> TestCase -> IO ()
propertyAction prop tc =
  runPropertyT PropertyEnv {testCase = tc, journal = \_ _ _ -> pure ()} prop

-- | Run a property against a test case with a recording journal, returning
-- how the run ended together with the journal contents.
observeProperty :: TestCase -> Property () -> IO (Either SomeException (), [Note])
observeProperty tc prop = do
  j <- newIORef Seq.empty
  let record kind loc text = modifyIORef' j (|> Note {kind, text, loc})
  eRes <- tryProperty (runPropertyT PropertyEnv {testCase = tc, journal = record} prop)
  notes <- toList <$> readIORef j
  pure (eRes, notes)

-- NOTE: This function _needs_ to use 'Control.Exception.throwIO' so that
-- all non-Hegel async exceptions are rethrown _as_ async exceptions (and not
-- re-wrapped in a synchronous exception wrapper by safe-exceptions).

-- | Like 'UnliftIO.Exception.tryAny', but additionally catches Hegel's
-- control signals ('Hegel.Internal.TestCase.AssumeRejected',
-- 'Hegel.Internal.TestCase.TestStopped'), which are async exceptions precisely so that
-- user catch-alls pass them through.
--
-- All other async exceptions should be passed through unmodified.
tryProperty :: IO a -> IO (Either SomeException a)
tryProperty act =
  E.try act >>= \res -> case res of
    Right a -> pure (Right a)
    Left e
      | isControlSignal e || isSyncException e -> pure (Left e)
      -- Base 'E.throwIO' to preserve the exception's async flavor on rethrow.
      | otherwise -> E.throwIO e

-- | Attempt to recover an 'AssertionFailure' from the given exception, and (if
-- present) extract the message, callsite, and diff associated with it.
--
-- If the given exception is /not/ an 'AssertionFailure', render it with
-- 'displayException' and return that on its own.
failureDetails :: SomeException -> (Text, Maybe SrcLoc, Maybe Diff)
failureDetails e = case fromException e of
  Just (af :: AssertionFailure) -> (af.message, callSite af.callStack, af.diff)
  Nothing -> (T.pack (E.displayException e), Nothing, Nothing)
