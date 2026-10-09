-- | Unit tests for 'Hegel.Supply'.
module SupplyProperties (spec) where

import Control.Exception (SomeException, fromException, try)
import Control.Exception qualified as E
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (def)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust)
import Data.Text qualified as T
import Data.Traversable (for)
import Hegel (Gen)
import Hegel.Gen qualified as Gen
import Hegel.Internal.Control (AssumeRejected)
import Hegel.Property (Property, assert, check, (===))
import Hegel.Report (Abort (..), FailureEvidence (..), FailureOutcome (..), Note (..), Report (..), Result (..), isBranchHeader, isDrawn)
import Hegel.Runner (replay)
import Hegel.Settings (Settings (..), defaultSettings)
import Hegel.Supply (Supply)
import Hegel.Supply qualified as Supply
import Test.Hspec
import TestSupport (expectCaptured, expectToken, forRenderers)
import UnliftIO.Async qualified as Async
import UnliftIO.IORef (newIORef, readIORef, writeIORef)
import UnliftIO.STM (TBQueue, atomically, newTBQueueIO, readTBQueue, writeTBQueue)

intR :: (Int, Int) -> Gen Int
intR (lo, hi) = Gen.int & Gen.min lo & Gen.max hi & Gen.build

-- | A generator that discards the test case whenever its coin lands 'False'.
sometimesDiscards :: Gen Int
sometimesDiscards = do
  b <- Gen.bool & Gen.build
  Gen.assume b
  pure 1

-- | Swallow every exception, as an HTTP framework's catch-all handler does.
swallow :: a -> IO a -> IO a
swallow fallback act = act `E.catch` \(_ :: SomeException) -> pure fallback

drawnTexts :: [Note] -> [T.Text]
drawnTexts notes = [n.text | n <- notes, isDrawn n.kind]

spec :: Spec
spec = describe "Hegel.Supply" do
  coreSpec
  splitSpec

coreSpec :: Spec
coreSpec = describe "withSupply and draw" do
  it "journals each draw under its label beneath a Supply header" do
    report <- check def do
      _ <- Supply.withSupply \supply -> Supply.draw supply "latency" (intR (0, 100))
      assert False "force a counterexample so the journal renders"
    FailureEvidence {notes} <- expectCaptured report.result
    [n.text | n <- notes, isBranchHeader n] `shouldBe` ["Supply 1"]
    [(n.text, n.depth) | n <- notes, isDrawn n.kind] `shouldBe` [("latency=0", 1)]
    forRenderers report (`shouldSatisfy` T.isInfixOf "latency=0")

  it "shrinks a supply draw like any other draw" do
    report <- check def do
      n <- Supply.withSupply \supply -> Supply.draw supply "n" (intR (0, 1000))
      assert (n < 100) "n is too big"
    FailureEvidence {notes} <- expectCaptured report.result
    drawnTexts notes `shouldBe` ["n=100"]

  it "rethrows a swallowed discard when withSupply returns" do
    report <- check def do
      n <- Supply.withSupply \supply -> swallow 0 (Supply.draw supply "x" sometimesDiscards)
      assert (n /= 0) "the stub's fallback leaked into the property"
    report.result `shouldSatisfy` \case
      Ok -> True
      _ -> False

  it "rethrows a swallowed discard from every later draw" do
    sawPoison <- newIORef False
    report <- check def do
      Supply.withSupply \supply -> do
        _ <- swallow 0 (Supply.draw supply "x" sometimesDiscards)
        try (Supply.draw supply "y" (intR (0, 10))) >>= \case
          Left (e :: SomeException) | isJust (fromException e :: Maybe AssumeRejected) -> writeIORef sawPoison True
          _ -> pure ()
    report.result `shouldSatisfy` \case
      Failures _ -> False
      _ -> True
    readIORef sawPoison `shouldReturn` True

  it "rejects a draw from a supply that outlived its scope" do
    leaked <- newIORef (Nothing :: Maybe Supply)
    report <- check def do
      Supply.withSupply \supply -> writeIORef leaked (Just supply)
      liftIO (readIORef leaked) >>= \case
        Just supply -> liftIO (() <$ Supply.draw supply "late" (intR (0, 10)))
        Nothing -> pure ()
    case report.result of
      Aborted _ -> T.pack (show report.result) `shouldSatisfy` T.isInfixOf "after its withSupply scope ended"
      other -> expectationFailure ("expected Aborted, got: " <> show other)

  it "passes quietly when nothing fails" do
    report <- check def do
      n <- Supply.withSupply \supply -> Supply.draw supply "n" (intR (0, 10))
      assert (n <= 10) "within bounds"
    report.result `shouldSatisfy` \case
      Ok -> True
      _ -> False

-- * Split: a multiple-producer, single-consumer queue

type Item = (Int, Int, Int)

-- | Each producer pushes a drawn number of drawn payloads, tagged with its id
-- and sequence number, then a 'Nothing' to say it's done.
producer :: TBQueue (Maybe Item) -> (Int, Supply) -> IO [Item]
producer queue (pid, supply) = do
  count <- Supply.draw supply ("p" <> T.pack (show pid) <> "#count") (intR (0, 4))
  items <- for [1 .. count] \sq -> do
    payload <- Supply.draw supply ("p" <> T.pack (show pid)) (intR (0, 2000))
    let item = (pid, sq, payload)
    atomically (writeTBQueue queue (Just item))
    pure item
  atomically (writeTBQueue queue Nothing)
  pure items

-- | Drain the queue until every producer is done, keeping what the processor
-- passes on.
consumer :: (Maybe Item -> Item -> Bool) -> Int -> TBQueue (Maybe Item) -> IO [Item]
consumer keep producers queue = go producers Nothing []
  where
    go 0 _ acc = pure (reverse acc)
    go n prev acc =
      atomically (readTBQueue queue) >>= \case
        Nothing -> go (n - 1 :: Int) prev acc
        Just item -> go n (Just item) (if keep prev item then item : acc else acc)

-- | Run three split producers against one consumer and check that every
-- producer's items come out complete and in order.
queueProperty :: (Maybe Item -> Item -> Bool) -> Property ()
queueProperty keep = do
  (produced, consumed) <- Supply.withSupply \root -> do
    supplies <- replicateM 3 (Supply.split root)
    queue <- newTBQueueIO 4
    Async.concurrently
      (Async.mapConcurrently (producer queue) (zip [1 ..] supplies))
      (consumer keep 3 queue)
  for_ (zip [1 :: Int ..] produced) \(pid, items) ->
    [i | i@(p, _, _) <- consumed, p == pid] === items

payloadTexts :: [Note] -> [T.Text]
payloadTexts notes = [t | t <- drawnTexts notes, not ("#count" `T.isInfixOf` t)]

splitSpec :: Spec
splitSpec = describe "Supply.split" do
  it "keeps every producer's items in order through a correct queue" do
    report <- check def (queueProperty \_ _ -> True)
    report.result `shouldSatisfy` \case
      Ok -> True
      _ -> False

  it "shrinks a value-dependent drop to one minimal payload and replays it" do
    let prop = queueProperty \_ (_, _, payload) -> payload < 1000
    report <- check def prop
    FailureEvidence {notes} <- expectCaptured report.result
    map (T.dropWhile (/= '=')) (payloadTexts notes) `shouldBe` ["=1000"]
    [n.text | n <- notes, isBranchHeader n] `shouldBe` ["Supply 1", "Supply 2", "Supply 3", "Supply 4"]
    case report.result of
      Failures (outcome :| _) -> do
        outcome.failureCaveat `shouldBe` Nothing
        token <- expectToken outcome
        replayed <- replay def token prop
        FailureEvidence {notes = replayedNotes} <- expectCaptured replayed.result
        payloadTexts replayedNotes `shouldBe` payloadTexts notes
      other -> expectationFailure ("expected a failure, got: " <> show other)

  it "survives a drop that depends on how producers interleave" do
    -- Drops an item whenever it lands right behind another producer's item,
    -- which depends on scheduling rather than on any drawn value.
    let keep :: Maybe Item -> Item -> Bool
        keep prev (pid, _, _) = maybe True (\(p, _, _) -> p == pid) prev
    report <- check def (queueProperty keep)
    report.result `shouldSatisfy` \case
      Aborted _ -> False
      _ -> True

  it "rethrows a discard swallowed on a split" do
    report <- check def do
      n <- Supply.withSupply \root -> do
        child <- Supply.split root
        swallow 0 (Supply.draw child "x" sometimesDiscards)
      assert (n /= 0) "the stub's fallback leaked into the property"
    report.result `shouldSatisfy` \case
      Ok -> True
      _ -> False

  it "fails the case when splits nest past maxCloneDepth" do
    let settings = defaultSettings {maxCloneDepth = Just 3}
        nest :: Int -> Supply -> IO ()
        nest 0 _ = pure ()
        nest n s = Supply.split s >>= nest (n - 1)
    report <- check settings (Supply.withSupply (nest 5))
    case report.result of
      Aborted (Errored e) -> T.pack (E.displayException e) `shouldSatisfy` T.isInfixOf "maxCloneDepth"
      other -> expectationFailure ("expected Aborted Errored, got: " <> show other)
