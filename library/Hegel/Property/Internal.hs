-- | Internals of the property monad. Most users should import
-- "Hegel.Property" instead; this module additionally exposes the
-- environment and runner hooks that backends build on.
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
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Data.Foldable (toList)
import Data.Sequence ((|>))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import GHC.Stack (HasCallStack, SrcLoc, callStack, withFrozenCallStack)
import Hegel.Assertion (AssertionFailure (..), callSite)
import Hegel.Diff (Diff)
import Hegel.Gen.Internal (AssumeRejected (..), Gen, draw)
import Hegel.Report (Note (..), NoteKind (..), renderValue)
import Hegel.TestCase (TestCase)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (throwIO, tryAny)
import UnliftIO.IORef (modifyIORef', newIORef, readIORef)

-- | The per-test-case environment a property runs against: the live test
-- case (the engine's oracle for draws) and a journaling sink for 'Note's
-- describing the case.
--
-- The sink is lazy in the note text, and ordinary cases run with a no-op
-- sink ('propertyAction'), so rendering drawn values costs nothing on the
-- hot path; notes are only materialised when a failing case is re-executed
-- with a recording sink ('observeProperty').
data PropertyEnv = PropertyEnv
  { testCase :: !TestCase,
    journal :: !(NoteKind -> Maybe SrcLoc -> Text -> IO ())
  }

-- | A property: test logic interleaved with generator draws against a live
-- test case.
--
-- Unlike @'Hegel.Property.forEach' gen body@ — where all draws happen up
-- front — a 'PropertyT' may draw ('forAll'), perform effects, and assert in
-- any order. The engine shrinks across the whole interleaving.
--
-- Failure is exception-based ('Hegel.Assertion.AssertionFailure' from
-- 'Hegel.Assertion.assert'\/'Hegel.Assertion.failure', or any other
-- exception), so assertions work unchanged under any transformer stack
-- layered on top.
--
-- __Note__: the engine re-runs the whole property on every shrink attempt
-- and once more to reconstruct the failure report, so effects must tolerate
-- repetition ('Hegel.Settings.perCaseFinalizer' runs after every case).
newtype PropertyT m a = PropertyT (ReaderT PropertyEnv m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFail, MonadUnliftIO)

-- | A property over 'IO', as consumed by the runners' @check@ entry points.
type Property = PropertyT IO

instance MonadTrans PropertyT where
  lift = PropertyT . lift

-- | Run a property in a different base monad, e.g. to collapse an
-- application stack before handing the property to a runner:
--
-- > check settings (hoist (runAppM env) myProp)
hoist :: (forall x. m x -> n x) -> PropertyT m a -> PropertyT n a
hoist f (PropertyT (ReaderT g)) = PropertyT (ReaderT (f . g))

-- | Send a note to the journaling sink.
note :: (MonadIO m) => NoteKind -> Maybe SrcLoc -> Text -> PropertyT m ()
note kind loc text = PropertyT do
  env <- ask
  liftIO (env.journal kind loc text)

-- | Draw a value from a generator mid-test. The drawn value is journaled
-- (via 'renderValue') so it appears in the failure report.
forAll :: (HasCallStack, MonadIO m, Show a) => Gen a -> PropertyT m a
forAll = withFrozenCallStack (forAllWith renderValue)

-- | 'forAll' with an explicit renderer, for values without a 'Show'
-- instance (or with an unhelpful one).
forAllWith :: (HasCallStack, MonadIO m) => (a -> Text) -> Gen a -> PropertyT m a
forAllWith render gen = do
  a <- forAllSilent gen
  note Drawn (callSite callStack) (render a)
  pure a

-- | Draw a value without journaling it. For bookkeeping draws (e.g. a
-- stateful driver's step budget) that would only add noise to the report.
forAllSilent :: (MonadIO m) => Gen a -> PropertyT m a
forAllSilent gen = PropertyT do
  env <- ask
  liftIO (draw env.testCase gen)

-- | Attach context to the failure report, rendered at the point it was
-- recorded.
annotate :: (HasCallStack, MonadIO m) => Text -> PropertyT m ()
annotate = note Annotation (callSite callStack)

-- | 'annotate' a value, rendered via 'renderValue'.
annotateShow :: (HasCallStack, MonadIO m, Show a) => a -> PropertyT m ()
annotateShow = withFrozenCallStack (annotate . renderValue)

-- | Attach context rendered after the report body.
footnote :: (MonadIO m) => Text -> PropertyT m ()
footnote = note Footnote Nothing

-- | Discard the current test case when the condition is 'False'. Use this
-- to enforce preconditions discovered mid-test; the case is reported to the
-- engine as invalid rather than failed.
assume :: (MonadIO m) => Bool -> m ()
assume cond = if cond then pure () else discard

-- | Discard the current test case unconditionally.
discard :: (MonadIO m) => m a
discard = throwIO AssumeRejected

-- * Runner hooks

-- | Run a property against an explicit environment.
runPropertyT :: PropertyEnv -> PropertyT m a -> m a
runPropertyT env (PropertyT r) = runReaderT r env

-- | Lower a property to a per-case action for a drive loop. Ordinary cases
-- run with a no-op journaling sink (their notes are never read); failing
-- cases are journaled later via 'observeProperty' on the engine's minimal
-- counterexample.
propertyAction :: Property () -> TestCase -> IO ()
propertyAction prop tc =
  runPropertyT PropertyEnv {testCase = tc, journal = \_ _ _ -> pure ()} prop

-- | Run a property against a test case with a recording journal, returning
-- how the run ended together with the journal contents. Synchronous
-- exceptions are captured, not rethrown.
observeProperty :: TestCase -> Property () -> IO (Either SomeException (), [Note])
observeProperty tc prop = do
  j <- newIORef Seq.empty
  let record kind loc text = modifyIORef' j (|> Note {kind, text, loc})
  eRes <- tryAny (runPropertyT PropertyEnv {testCase = tc, journal = record} prop)
  notes <- toList <$> readIORef j
  pure (eRes, notes)

-- | Failure presentation details from the exception that reproduced a
-- failure: prefer 'AssertionFailure''s message, call site, and diff over the
-- engine's stable origin string, which is a dedup key rather than prose.
failureDetails :: Text -> SomeException -> (Text, Maybe SrcLoc, Maybe Diff)
failureDetails engineMsg e = case fromException e of
  Just (af :: AssertionFailure) -> (af.message, callSite af.callStack, af.diff)
  Nothing -> (engineMsg, Nothing, Nothing)
