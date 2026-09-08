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
module Hegel.Property.Branch
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
import Control.Monad.IO.Class (liftIO)
import Data.Either (partitionEithers)
import Data.List (find)
import Data.Text qualified as T
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hegel.Internal.Control (isAborting, malformedTest)
import Hegel.Internal.TestCase (withClonePair, withClones)
import Hegel.Property.Internal
  ( Env (testCase),
    PropertyT,
    askEnv,
    checkCloneDepth,
    foldBranchNotes,
    runBranch,
    withBaseRunInIO,
  )
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async qualified as Async

-- | Run two property bodies concurrently, each against its own cloned choice
-- stream, and combine their results.
--
-- @
-- (readResult, writeResult) <- concurrently
--   (do h <- forAll (Pool.reuse handles); liftIO (readHandle h))
--   (do h <- forAll (Pool.reuse handles); liftIO (writeHandle h "x"))
-- @
concurrently :: (HasCallStack, MonadUnliftIO m) => PropertyT m a -> PropertyT m b -> PropertyT m (a, b)
concurrently pa pb = withFrozenCallStack $ do
  env <- askEnv
  liftIO (checkCloneDepth env)
  withBaseRunInIO \runBase ->
    withClonePair env.testCase \ta tb -> do
      (ra, rb) <- Async.concurrently (runBranch runBase env ta pa) (runBranch runBase env tb pb)
      foldBranchNotes env [snd ra, snd rb]
      case find isAborting [e | Left e <- [void (fst ra), void (fst rb)]] of
        Just e -> E.throwIO e
        Nothing -> pure ()
      case (fst ra, fst rb) of
        (Left e, _) -> E.throwIO e
        (Right _, Left e) -> E.throwIO e
        (Right a, Right b) -> pure (a, b)

-- | 'concurrently', discarding both results.
concurrently_ :: (HasCallStack, MonadUnliftIO m) => PropertyT m a -> PropertyT m b -> PropertyT m ()
concurrently_ pa pb = withFrozenCallStack $ void (concurrently pa pb)

-- | Apply a property-valued function to every element of a list concurrently,
-- each against its own cloned choice stream, preserving input order in the
-- result.
mapConcurrently :: (HasCallStack, MonadUnliftIO m) => (a -> PropertyT m b) -> [a] -> PropertyT m [b]
mapConcurrently f xs = withFrozenCallStack $ runBranches Async.mapConcurrently (map f xs)

-- | 'mapConcurrently', discarding the results.
mapConcurrently_ :: (HasCallStack, MonadUnliftIO m) => (a -> PropertyT m b) -> [a] -> PropertyT m ()
mapConcurrently_ f = withFrozenCallStack $ void . mapConcurrently f

-- | 'mapConcurrently' with its arguments flipped.
forConcurrently :: (HasCallStack, MonadUnliftIO m) => [a] -> (a -> PropertyT m b) -> PropertyT m [b]
forConcurrently = withFrozenCallStack $ flip mapConcurrently

-- | 'mapConcurrently_' with its arguments flipped.
forConcurrently_ :: (HasCallStack, MonadUnliftIO m) => [a] -> (a -> PropertyT m b) -> PropertyT m ()
forConcurrently_ = withFrozenCallStack $ flip mapConcurrently_

-- | Run @n@ copies of a property body concurrently, each against its own
-- cloned choice stream. This is the N-client shape most concurrent-SUT
-- properties want: @replicateConcurrently 5 clientSession@ runs five
-- independent sessions against one shared server.
replicateConcurrently :: (HasCallStack, MonadUnliftIO m) => Int -> PropertyT m a -> PropertyT m [a]
replicateConcurrently n act = withFrozenCallStack $ runBranches Async.mapConcurrently (replicate n act)

-- | 'replicateConcurrently', discarding the results.
replicateConcurrently_ :: (HasCallStack, MonadUnliftIO m) => Int -> PropertyT m a -> PropertyT m ()
replicateConcurrently_ n = withFrozenCallStack $ void . replicateConcurrently n

-- | 'replicateConcurrently', capping how many branches drive at once rather
-- than acquiring all @n@ clones and threads live simultaneously. @cap@ must
-- be at least 1.
replicateConcurrentlyBounded :: (HasCallStack, MonadUnliftIO m) => Int -> Int -> PropertyT m a -> PropertyT m [a]
replicateConcurrentlyBounded cap n act =
  withFrozenCallStack $
    if cap < 1
      then liftIO (E.throwIO (malformedTest "Hegel.Property.Branch.replicateConcurrentlyBounded" "cap must be at least 1" [("cap", T.pack (show cap))]))
      else runBranches (Async.pooledMapConcurrentlyN cap) (replicate n act)

-- * Mechanics

-- | Run every branch of a homogeneous fan-out, given the concurrency strategy
-- ('UnliftIO.Async.mapConcurrently' for unbounded fan-out, or a pooled
-- variant for bounded), then fold notes and report the lowest-indexed failure
-- deterministically, mirroring 'concurrently'.
runBranches ::
  (HasCallStack, MonadUnliftIO m) =>
  (forall x y. (x -> IO y) -> [x] -> IO [y]) ->
  [PropertyT m a] ->
  PropertyT m [a]
runBranches _ [] = pure []
runBranches runMany actions = withFrozenCallStack $ do
  env <- askEnv
  liftIO (checkCloneDepth env)
  withBaseRunInIO \runBase ->
    withClones (length actions) env.testCase \clones -> do
      outcomes <- runMany (\(tc, act) -> runBranch runBase env tc act) (zip clones actions)
      foldBranchNotes env (map snd outcomes)
      case partitionEithers (map fst outcomes) of
        (e : es, _) -> E.throwIO (maybe e id (find isAborting (e : es)))
        ([], results) -> pure results
