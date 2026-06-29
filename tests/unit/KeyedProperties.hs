-- | Path-derived example-database keys for the hspec and tasty integrations.
module KeyedProperties (spec, tastyTree) where

import Data.Default.Class (def)
import Data.Function ((&))
import Data.List (isInfixOf)
import Data.Text (Text)
import GHC.Stack (callStack, emptyCallStack)
import Hegel (Gen)
import Hegel.Database (Database (..))
import Hegel.Gen qualified as Gen
import Hegel.Hspec (prop, propWith)
import Hegel.Internal.DatabaseKey (joinPath, moduleFromCallStack, propKey)
import Hegel.Phase (Phase (..))
import Hegel.Property (Property, assert, forAll)
import Hegel.Settings (Settings (..), defaultSettings, withDatabaseKey)
import Hegel.Tasty qualified
import Test.Hspec
import Test.Hspec.Core.Spec qualified as Core
import Test.Tasty (TestTree, testGroup)
import UnliftIO.Directory (withCurrentDirectory)
import UnliftIO.Temporary (withSystemTempDirectory)

spec :: Spec
spec = do
  describe "propKey" $ do
    it "salts with the call-site module and joins the describe path" $
      deriveKey ["reverse"] "is involutive"
        `shouldBe` "KeyedProperties:reverse/is involutive"

    it "handles an empty describe path" $
      deriveKey [] "standalone" `shouldBe` "KeyedProperties:standalone"

  describe "joinPath" $ do
    it "joins segments with slashes" $
      joinPath ["a", "b", "c"] `shouldBe` "a/b/c"

    it "is empty for no segments" $
      joinPath [] `shouldBe` ""

  describe "moduleFromCallStack" $
    it "falls back to a sentinel when the stack is empty" $
      moduleFromCallStack emptyCallStack `shouldBe` "<unknown-module>"

  describe "withDatabaseKey" $ do
    it "sets the key without changing the store" $ do
      let s = withDatabaseKey "k" defaultSettings
      s.databaseKey `shouldBe` Just "k"
      s.database `shouldSatisfy` isDisabled

    it "leaves an explicitly chosen store untouched" $ do
      let s = withDatabaseKey "k" defaultSettings {database = DatabaseDirectory "/tmp/x"}
      s.database `shouldSatisfy` isDirectory

  describe "hspec prop" $ do
    it "persists a failure and replays it under the path-derived key" $
      withSystemTempDirectory "zizek-keyed-hspec" \dbDir -> do
        let mk ph label =
              describe "group" $
                propWith
                  defaultSettings {database = DatabaseDirectory dbDir, phases = ph}
                  label
                  failing
            replayOnly = [Explicit, Reuse, Shrink]
        -- Record a counterexample into the store under "group/label".
        r1 <- evalOnly (mk defaultSettings.phases "label")
        r1 `shouldSatisfy` reproduced
        -- With generation disabled, only the stored example can fail it again;
        -- it does, so the path-derived key filed and refetched it.
        r2 <- evalOnly (mk replayOnly "label")
        r2 `shouldSatisfy` reproduced
        -- A different label keys elsewhere: nothing stored, nothing generated,
        -- so the engine gives up rather than reproducing the counterexample.
        r3 <- evalOnly (mk replayOnly "other")
        r3 `shouldSatisfy` gaveUp

    it "runs under the default store with an auto-derived key" $
      -- prop turns on the default .hegel/ store; run in a temp cwd so nothing
      -- lands in the repo.
      withSystemTempDirectory "zizek-keyed-cwd" \tmp ->
        withCurrentDirectory tmp $ do
          r <- evalOnly (describe "group" $ prop "passes" passing)
          r `shouldSatisfy` isSuccess

    it "propWith def runs without persisting" $ do
      -- Generation finds the counterexample but never stores it, so a
      -- replay-only rerun has nothing to reproduce and gives up.
      r1 <- evalOnly (describe "group" $ propWith def "label" failing)
      r1 `shouldSatisfy` reproduced
      r2 <-
        evalOnly
          (describe "group" $ propWith defaultSettings {phases = [Explicit, Reuse, Shrink]} "label" failing)
      r2 `shouldSatisfy` gaveUp

-- | A property whose stored counterexample is reused on replay.
failing :: Property ()
failing = do
  x <- forAll (intR (0, 1000))
  assert (x < 100) "stays small"

-- | A property that always succeeds.
passing :: Property ()
passing = do
  x <- forAll (intR (0, 10))
  assert (x >= 0) "non-negative"

intR :: (Int, Int) -> Gen Int
intR (lo, hi) = Gen.integral & Gen.min lo & Gen.max hi & Gen.build

-- | Derive a key as if called from this module (for the module salt).
deriveKey :: (HasCallStack) => [String] -> String -> Text
deriveKey = propKey callStack

-- | Evaluate the single example in @s@ directly, returning its hspec result.
evalOnly :: Spec -> IO Core.Result
evalOnly s = do
  (_, trees) <- Core.runSpecM s
  case concatMap leaves trees of
    item : _ -> Core.itemExample item Core.defaultParams (\f -> f ()) (\_ -> pure ())
    [] -> error "evalOnly: no example found"

-- | All leaf items of a spec tree, in order.
leaves :: Core.Tree c (Core.Item a) -> [Core.Item a]
leaves = \case
  Core.Leaf i -> [i]
  Core.Node _ ts -> concatMap leaves ts
  Core.NodeWithCleanup _ _ ts -> concatMap leaves ts

isSuccess :: Core.Result -> Bool
isSuccess (Core.Result _ Core.Success) = True
isSuccess _ = False

-- | The failure reason text, if the result is a reasoned failure.
failureReason :: Core.Result -> Maybe String
failureReason (Core.Result _ (Core.Failure _ (Core.Reason msg))) = Just msg
failureReason _ = Nothing

-- | A reproduced counterexample: a failure that is not the engine giving up.
reproduced :: Core.Result -> Bool
reproduced r = maybe False (not . isInfixOf "gave up") (failureReason r)

-- | The engine gave up without reproducing a counterexample.
gaveUp :: Core.Result -> Bool
gaveUp r = maybe False (isInfixOf "gave up") (failureReason r)

isDisabled :: Database -> Bool
isDisabled DatabaseDisabled = True
isDisabled _ = False

isDirectory :: Database -> Bool
isDirectory (DatabaseDirectory _) = True
isDirectory _ = False

-- | tasty leaves exercising auto-keying and an explicit-key override.
tastyTree :: TestTree
tastyTree =
  testGroup
    "keyed properties (tasty)"
    [ Hegel.Tasty.testProperty "auto-keyed leaf" passing,
      Hegel.Tasty.testPropertyWith
        defaultSettings {databaseKey = Just "explicit-tasty-key"}
        "explicit key respected"
        passing
    ]
