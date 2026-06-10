-- | Smoke tests for the property monad ('check').
module PropertyChecks (spec) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Data.Function ((&))
import Data.Maybe (isJust)
import Data.Text qualified as T
import Hegel (Gen)
import Hegel.Diff (LineDiff (..))
import Hegel.Gen qualified as Gen
import Hegel.Property
  ( annotate,
    assert,
    assume,
    failure,
    footnote,
    forAll,
    forAllSilent,
    forAllWith,
    hoist,
    (===),
  )
import Hegel.Report (Note (..), NoteKind (..), Report (..), Result (..), Stats (..), renderReport)
import Hegel.Runner (check)
import Hegel.Settings (defaultSettings)
import Test.Hspec
import UnliftIO.IORef (newIORef, readIORef, writeIORef)

intR :: (Int, Int) -> Gen Int
intR (lo, hi) = Gen.integral & Gen.min lo & Gen.max hi & Gen.build

spec :: Spec
spec = do
  it "interleaves draws, notes, and assertions" $ do
    report <- check defaultSettings do
      x <- forAll (intR (0, 100))
      annotate "first draw done"
      y <- forAll (intR (0, 100))
      assert (x + y == y + x) "addition commutes"
    report.result `shouldSatisfy` \case
      Ok -> True
      _ -> False

  it "reports counterexamples through the journal" $ do
    report <- check defaultSettings do
      x <- forAll (intR (0, 100))
      annotate "drew the first addend"
      y <- forAll (intR (0, 100))
      assert (x + y < 150) "sum stays small"
    case report.result of
      Counterexample {message, notes, loc} -> do
        message `shouldBe` "sum stays small"
        length [n | n <- notes, n.kind == Drawn] `shouldBe` 2
        length [n | n <- notes, n.kind == Annotation] `shouldBe` 1
        loc `shouldSatisfy` isJust
      other -> expectationFailure ("expected a counterexample, got: " <> show other)

  it "discards mid-body via assume" $ do
    report <- check defaultSettings do
      x <- forAll (intR (0, 100))
      assume (x < 50)
      assert (x < 50) "assumed bound holds"
    case report.result of
      Ok -> report.stats.invalid `shouldSatisfy` (> 0)
      other -> expectationFailure ("expected Ok, got: " <> show other)

  it "shrinks dependent draws to a minimal counterexample" $ do
    -- The second range depends on the first draw; minimal failing case is
    -- x = y = 50. The capture is written by the reconstruction replay, so
    -- it doubles as a check that reconstruction re-executes the body.
    capture <- newIORef (0, 0)
    report <- check defaultSettings do
      x <- forAll (intR (0, 1000))
      y <- forAll (intR (0, x))
      writeIORef capture (x, y)
      assert (x + y < 100) "sum stays under threshold"
    case report.result of
      Counterexample {notes} -> do
        (x, y) <- readIORef capture
        (x + y) `shouldBe` 100
        y `shouldSatisfy` (<= x)
        fmap (.text) (filter (\n -> n.kind == Drawn) notes)
          `shouldBe` [T.pack (show x), T.pack (show y)]
      other -> expectationFailure ("expected a counterexample, got: " <> show other)

  it "journals forAllWith renderings but not silent draws" $ do
    report <- check defaultSettings do
      _x <- forAllWith (\n -> "custom:" <> T.pack (show n)) (intR (0, 10))
      _y <- forAllSilent (intR (0, 10))
      footnote "from the footer"
      failure "always fails"
    case report.result of
      Counterexample {message, notes} -> do
        message `shouldBe` "always fails"
        case notes of
          [drawn, foot] -> do
            drawn.kind `shouldBe` Drawn
            drawn.text `shouldSatisfy` T.isPrefixOf "custom:"
            foot.kind `shouldBe` Footnote
            foot.text `shouldBe` "from the footer"
          other -> expectationFailure ("expected two notes, got: " <> show other)
      other -> expectationFailure ("expected a counterexample, got: " <> show other)

  it "carries (===) diffs into the counterexample diff field" $ do
    report <- check defaultSettings do
      x <- forAll (intR (0, 100))
      x === x + 1
    case report.result of
      Counterexample {message, diff} -> do
        message `shouldBe` "=== failed"
        -- Structural diff: two integers are one-liners so they diff as
        -- a removed/added pair, not a structural field diff.
        diff `shouldBe` Just [LineRemoved "0", LineAdded "1"]
        -- The rendered report carries the diff lines with -/+ prefixes.
        T.lines (renderReport report)
          `shouldSatisfy` any ("  - 0" ==)
        T.lines (renderReport report)
          `shouldSatisfy` any ("  + 1" ==)
      other -> expectationFailure ("expected a counterexample, got: " <> show other)

  it "hoists application monads and lifts base actions" $ do
    report <- check defaultSettings $ hoist (`runReaderT` 25) do
      bound <- lift ask
      x <- forAll (intR (0, bound))
      assert (x <= bound) "stays within the environment bound"
    report.result `shouldSatisfy` \case
      Ok -> True
      _ -> False
