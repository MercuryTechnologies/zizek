-- | Unit tests for 'Hegel.Supply'.
module SupplyProperties (spec) where

import Control.Exception (SomeException, fromException, try)
import Control.Exception qualified as E
import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (def)
import Data.Function ((&))
import Data.Maybe (isJust)
import Data.Text qualified as T
import Hegel (Gen)
import Hegel.Gen qualified as Gen
import Hegel.Internal.Control (AssumeRejected)
import Hegel.Property (assert, check)
import Hegel.Report (FailureEvidence (..), Note (..), Report (..), Result (..), isBranchHeader, isDrawn)
import Hegel.Supply (Supply)
import Hegel.Supply qualified as Supply
import Test.Hspec
import TestSupport (expectCaptured, forRenderers)
import UnliftIO.IORef (newIORef, readIORef, writeIORef)

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
