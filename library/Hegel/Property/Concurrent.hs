-- | Concurrent branches of a property, each drawing from its own cloned
-- choice stream.
--
-- 'concurrently' and its list-shaped siblings run N property bodies at once
-- against a shared system under test, joining before the combinator returns.
-- Every branch's generated values replay and shrink deterministically.
--
-- Every concurrent-property tool carries the same caveat: the real-time
-- interleaving of a branch's effects against a shared system does not replay
-- deterministically.
--
-- A branch failure surfaces as an ordinary shrinkable counterexample rather
-- than an aborted run. The engine shrinks toward the lowest-indexed failing
-- branch's exception after every branch has run to completion, and every
-- failing branch renders its own message, location, and diff in the report,
-- not only the one that wins the shrink target.
--
-- Each branch's journaled notes are folded into the report one level deeper,
-- under a @Branch N@ header. A citation crossing a branch boundary, a value
-- born in one branch and consumed in another, does not resolve the way a
-- same-branch citation does.
module Hegel.Property.Concurrent
  ( concurrently,
    concurrently_,
    mapConcurrently,
    mapConcurrently_,
    forConcurrently,
    forConcurrently_,
    replicateConcurrently,
    replicateConcurrently_,
    replicateConcurrentlyBounded,
  )
where

import Control.Exception qualified as E
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Data.Either (partitionEithers)
import Data.Foldable (for_, toList)
import Data.Sequence ((|>))
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Hegel.Internal.Control (isFailure)
import Hegel.Internal.TestCase (TestCase)
import Hegel.Internal.TestCase qualified as TestCase
import Hegel.Internal.Tick qualified as Tick
import Hegel.Property.Internal
  ( Env (..),
    Journal (..),
    PropertyT,
    askEnv,
    drainFinalizers,
    failureDetails,
    newFinalizers,
    runPropertyT,
    tryProperty,
  )
import Hegel.Report.Note (Note (..), NoteKind (BranchFailure, BranchHeader))
import UnliftIO (MonadUnliftIO, withRunInIO)
import UnliftIO.Async qualified as Async
import UnliftIO.IORef (modifyIORef', newIORef, readIORef)

-- | Run two property bodies concurrently, each against its own cloned choice
-- stream, and combine their results.
--
-- @
-- (readResult, writeResult) <- concurrently
--   (do h <- forAll (Pool.reuse handles); liftIO (readHandle h))
--   (do h <- forAll (Pool.reuse handles); liftIO (writeHandle h "x"))
-- @
concurrently :: (MonadUnliftIO m) => PropertyT m a -> PropertyT m b -> PropertyT m (a, b)
concurrently pa pb = do
  env <- askEnv
  withBaseRunInIO \runBase ->
    withClonePair env.testCase \ta tb -> do
      (ra, rb) <- Async.concurrently (runBranch runBase env ta pa) (runBranch runBase env tb pb)
      foldBranchNotes env [snd ra, snd rb]
      case (fst ra, fst rb) of
        (Left e, _) -> E.throwIO e
        (Right _, Left e) -> E.throwIO e
        (Right a, Right b) -> pure (a, b)

-- | 'concurrently', discarding both results.
concurrently_ :: (MonadUnliftIO m) => PropertyT m a -> PropertyT m b -> PropertyT m ()
concurrently_ pa pb = void (concurrently pa pb)

-- | Apply a property-valued function to every element of a list concurrently,
-- each against its own cloned choice stream, preserving input order in the
-- result.
mapConcurrently :: (MonadUnliftIO m) => (a -> PropertyT m b) -> [a] -> PropertyT m [b]
mapConcurrently f xs = runBranches Async.mapConcurrently (map f xs)

-- | 'mapConcurrently', discarding the results.
mapConcurrently_ :: (MonadUnliftIO m) => (a -> PropertyT m b) -> [a] -> PropertyT m ()
mapConcurrently_ f = void . mapConcurrently f

-- | 'mapConcurrently' with its arguments flipped.
forConcurrently :: (MonadUnliftIO m) => [a] -> (a -> PropertyT m b) -> PropertyT m [b]
forConcurrently = flip mapConcurrently

-- | 'mapConcurrently_' with its arguments flipped.
forConcurrently_ :: (MonadUnliftIO m) => [a] -> (a -> PropertyT m b) -> PropertyT m ()
forConcurrently_ = flip mapConcurrently_

-- | Run @n@ copies of a property body concurrently, each against its own
-- cloned choice stream. The N-client shape most concurrent-SUT properties
-- want: @replicateConcurrently 5 clientSession@ runs five independent
-- sessions against one shared server.
replicateConcurrently :: (MonadUnliftIO m) => Int -> PropertyT m a -> PropertyT m [a]
replicateConcurrently n act = runBranches Async.mapConcurrently (replicate n act)

-- | 'replicateConcurrently', discarding the results.
replicateConcurrently_ :: (MonadUnliftIO m) => Int -> PropertyT m a -> PropertyT m ()
replicateConcurrently_ n = void . replicateConcurrently n

-- | 'replicateConcurrently', capping how many branches drive at once rather
-- than acquiring all @n@ clones and threads live simultaneously. @cap@ must
-- be at least 1.
replicateConcurrentlyBounded :: (MonadUnliftIO m) => Int -> Int -> PropertyT m a -> PropertyT m [a]
replicateConcurrentlyBounded cap n act = runBranches (Async.pooledMapConcurrentlyN cap) (replicate n act)

-- * Mechanics

-- | Obtain a way to run the base monad's actions in 'IO', independent of the
-- ambient 'Env'. The N-branch drivers below use this to step outside the
-- parent's environment before assembling each branch's own.
withBaseRunInIO :: (MonadUnliftIO m) => ((forall x. m x -> IO x) -> IO b) -> PropertyT m b
withBaseRunInIO inner = withRunInIO \run -> inner (run . lift)

-- | Acquire two clones of @tc@ in a fixed order, against @tc@ itself rather
-- than each other, so both fork positions are direct children at clone depth
-- one and consume their choice positions in the same order on every replay.
withClonePair :: TestCase -> (TestCase -> TestCase -> IO r) -> IO r
withClonePair tc k = TestCase.withClone tc \c1 -> TestCase.withClone tc \c2 -> k c1 c2

-- | Acquire @n@ clones of @tc@ sequentially, in a fixed order, for the same
-- reason 'withClonePair' does.
withClones :: Int -> TestCase -> ([TestCase] -> IO r) -> IO r
withClones n0 tc k = go n0 []
  where
    go 0 acc = k (reverse acc)
    go n acc = TestCase.withClone tc \c -> go (n - 1) (c : acc)

-- | Run one branch of a concurrent combinator against its own clone, in a
-- fresh 'Env' nested one level deeper than the parent's.
--
-- Neither a control signal nor an 'Hegel.Assertion.AssertionFailure' escapes
-- as an exception: each comes back as 'Left' so every branch runs to
-- completion regardless of a sibling's fate, and the caller decides
-- deterministically which failure, if any, to report as the case's shrink
-- target.
--
-- A real failure additionally gets journaled in-band as a 'BranchFailure', so
-- a branch that does not win the shrink target still shows its own message,
-- location, and diff in the report. A discard or budget stop does not,
-- since neither is a failure to explain.
--
-- A finalizer registered inside the branch is drained at the branch's own
-- exit. A teardown failure there propagates immediately, the same severity a
-- top-level 'Hegel.Property.registerFinalizer' failure carries.
runBranch ::
  (forall x. m x -> IO x) ->
  Env ->
  TestCase ->
  PropertyT m a ->
  IO (Either E.SomeException a, [Note])
runBranch runBase parentEnv tc body = do
  fin <- newFinalizers
  notesRef <- newIORef Seq.empty
  let branchJournal = case parentEnv.journal of
        Silent -> Silent
        Recording _ -> Recording \n -> modifyIORef' notesRef (|> n)
      branchEnv =
        Env
          { testCase = tc,
            journal = branchJournal,
            noteDepth = parentEnv.noteDepth + 1,
            finalizers = fin
          }
  eRes <- tryProperty (runBase (runPropertyT branchEnv body)) `E.onException` drainFinalizers fin
  failures <- drainFinalizers fin
  case failures of
    [] -> pure ()
    e : _ -> E.throwIO e
  notes <- toList <$> readIORef notesRef
  failureNotes <- case (branchJournal, eRes) of
    (Recording _, Left e) | isFailure e -> do
      clock <- Tick.next tc.recording
      let (msg, mloc, diff) = failureDetails e
      pure [Note {kind = BranchFailure diff, text = msg, loc = mloc, depth = branchEnv.noteDepth, clock}]
    _ -> pure []
  pure (eRes, notes <> failureNotes)

-- | Run every branch of a homogeneous fan-out, given the concurrency strategy
-- ('UnliftIO.Async.mapConcurrently' for unbounded fan-out, or a pooled
-- variant for bounded), then fold notes and report the lowest-indexed failure
-- deterministically, mirroring 'concurrently'.
runBranches ::
  (MonadUnliftIO m) =>
  (forall x y. (x -> IO y) -> [x] -> IO [y]) ->
  [PropertyT m a] ->
  PropertyT m [a]
runBranches runMany actions = do
  env <- askEnv
  withBaseRunInIO \runBase ->
    withClones (length actions) env.testCase \clones -> do
      outcomes <- runMany (\(tc, act) -> runBranch runBase env tc act) (zip clones actions)
      foldBranchNotes env (map snd outcomes)
      case partitionEithers (map fst outcomes) of
        (e : _, _) -> E.throwIO e
        ([], results) -> pure results

-- | Fold each branch's journaled notes into the parent journal, one level
-- deeper than the parent's ambient depth, under a @BranchHeader@. A no-op
-- under a 'Silent' journal, matching every other journaling primitive's cost
-- discipline.
foldBranchNotes :: Env -> [[Note]] -> IO ()
foldBranchNotes env branchNotes = case env.journal of
  Silent -> pure ()
  Recording sink ->
    for_ (zip [1 :: Int ..] branchNotes) \(i, notes) -> do
      clock <- Tick.next env.testCase.recording
      sink
        Note
          { kind = BranchHeader i,
            text = "Branch " <> T.pack (show i),
            loc = Nothing,
            depth = env.noteDepth,
            clock
          }
      for_ notes sink
