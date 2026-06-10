-- | Pure tests for 'Hegel.Report' rendering and assertion diffs; no engine
-- involved.
module ReportRendering (spec) where

import Control.Exception (displayException)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Stack (SrcLoc (..))
import Hegel.Assertion (AssertionFailure (..), (/==), (===))
import Hegel.Report
import Test.Hspec

aLoc :: SrcLoc
aLoc =
  SrcLoc
    { srcLocPackage = "zizek",
      srcLocModule = "Spec",
      srcLocFile = "tests/Spec.hs",
      srcLocStartLine = 42,
      srcLocStartCol = 3,
      srcLocEndLine = 42,
      srcLocEndCol = 10
    }

drawn, annotation, footer :: Text -> Note
drawn t = Note {kind = Drawn, text = t, loc = Nothing}
annotation t = Note {kind = Annotation, text = t, loc = Nothing}
footer t = Note {kind = Footnote, text = t, loc = Nothing}

spec :: Spec
spec = do
  describe "renderReport" $ do
    it "renders a passing run" $ do
      renderReport Report {result = Ok, stats = Stats {valid = 100, invalid = 0}}
        `shouldBe` "OK, passed 100 tests"

    it "renders discard counts" $ do
      renderReport Report {result = Ok, stats = Stats {valid = 100, invalid = 3}}
        `shouldBe` "OK, passed 100 tests (3 discarded)"

    it "renders a counterexample with numbered draws, footnotes last" $ do
      let result =
            Counterexample
              { message = "sum stays small",
                notes =
                  [ footer "seen at the end",
                    drawn "50",
                    annotation "drew the first addend",
                    drawn "51"
                  ],
                loc = Just aLoc
              }
          report = Report {result, stats = Stats {valid = 12, invalid = 1}}
      T.lines (renderReport report)
        `shouldBe` [ "failed after 12 tests (1 discarded)",
                     "sum stays small",
                     "  at tests/Spec.hs:42",
                     "  forAll 1 = 50",
                     "  drew the first addend",
                     "  forAll 2 = 51",
                     "  seen at the end"
                   ]

    it "renders gave-up and aborted verdicts" $ do
      renderReport Report {result = GaveUp "no valid examples", stats = Stats {valid = 0, invalid = 7}}
        `shouldBe` "gave up after 0 tests (7 discarded): no valid examples"
      renderReport (aborted (UnhealthyInput "filter too much"))
        `shouldBe` "aborted: health check failed: filter too much"

  describe "renderValue" $ do
    it "pretty-prints parseable Show output" $ do
      renderValue (Just (3 :: Int)) `shouldBe` "Just 3"

    it "falls back to the raw string for unparseable Show output" $ do
      renderValue Opaque `shouldBe` "<<opaque>>"

  describe "PropertyFailed" $ do
    it "displayException agrees with renderFailure" $ do
      let notes = [drawn "50", footer "ft"]
          exc = PropertyFailed {message = "boom", notes, loc = Just aLoc}
      T.pack (displayException exc)
        `shouldBe` renderFailure "property failed: boom" notes (Just aLoc)

  describe "throwOnFailure" $ do
    it "is silent on Ok" $ do
      throwOnFailure Report {result = Ok, stats = Stats {valid = 1, invalid = 0}}

    it "throws PropertyFailed on a counterexample" $ do
      let report =
            Report
              { result = Counterexample {message = "boom", notes = [], loc = Nothing},
                stats = Stats {valid = 1, invalid = 0}
              }
      throwOnFailure report `shouldThrow` \PropertyFailed {message} -> message == "boom"

  describe "equality assertions" $ do
    it "(===) passes on equal values" $ do
      (42 :: Int) === 42 :: IO ()

    it "(===) renders a line diff on failure" $ do
      (Lines ["a", "b", "c"] === Lines ["a", "X", "c"])
        `shouldThrow` \AssertionFailure {message} ->
          T.lines message
            == [ "=== failed",
                 "  <<a>>",
                 "- <<b>>",
                 "+ <<X>>",
                 "  <<c>>"
               ]

    it "(/==) throws when values are equal" $ do
      ((7 :: Int) /== 7)
        `shouldThrow` \AssertionFailure {message} ->
          "/== failed" `T.isPrefixOf` message

-- | A 'Show' instance whose output cannot parse as a value AST.
data Opaque = Opaque

instance Show Opaque where
  show _ = "<<opaque>>"

-- | Multi-line 'Show' output that cannot parse as a value AST (so
-- 'renderValue' preserves it verbatim), for exercising the line diff.
newtype Lines = Lines [Text]
  deriving stock (Eq)

instance Show Lines where
  show (Lines ls) = T.unpack (T.intercalate "\n" (fmap (\l -> "<<" <> l <> ">>") ls))
