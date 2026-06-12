-- | Pure tests for 'Hegel.Report' rendering and assertion diffs; no engine
-- involved.
module ReportRendering (spec) where

import Control.Exception (displayException)
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Stack (HasCallStack, SrcLoc (..), callStack, getCallStack)
import Hegel.Assertion (AssertionFailure (..), (/==), (===))
import Hegel.Diff (Diff, LineDiff (..), diffShown)
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

-- | The 'SrcLoc' of the call site, so tests can point a note at a line that
-- really exists in this file without hardcoding line numbers.
hereLoc :: (HasCallStack) => SrcLoc
hereLoc = case getCallStack callStack of
  (_, loc) : _ -> loc
  [] -> error "hereLoc: empty call stack"

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
                loc = Just aLoc,
                diff = Nothing
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

    it "renders a diff block before the journal" $ do
      let result =
            Counterexample
              { message = "=== failed",
                notes = [drawn "some value"],
                loc = Just aLoc,
                diff = Just [LineRemoved "old", LineAdded "new"]
              }
          report = Report {result, stats = Stats {valid = 5, invalid = 0}}
      T.lines (renderReport report)
        `shouldBe` [ "failed after 5 tests",
                     "=== failed",
                     "  - old",
                     "  + new",
                     "  at tests/Spec.hs:42",
                     "  forAll 1 = some value"
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

  describe "renderReportAnsi" $ do
    it "emits ANSI escape codes for a diff-bearing counterexample" $ do
      let result =
            Counterexample
              { message = "=== failed",
                notes = [],
                loc = Nothing,
                diff = Just [LineRemoved "old", LineAdded "new"]
              }
          report = Report {result, stats = Stats {valid = 1, invalid = 0}}
      -- ANSI output contains ESC codes; plain output does not.
      let plain = T.unpack (renderReport report)
          ansi = T.unpack (renderReportAnsi report)
      ansi `shouldNotBe` plain
      "\ESC[" `isInfixOf` ansi `shouldBe` True

  describe "renderReportRich" $ do
    it "splices the enclosing declaration for a Drawn note with a known location" $ do
      let loc' = hereLoc -- splice-marker: this line should appear in the rich report
          result =
            Counterexample
              { message = "boom",
                notes = [Note {kind = Drawn, text = "42", loc = Just loc'}],
                loc = Nothing,
                diff = Nothing
              }
          report = Report {result, stats = Stats {valid = 3, invalid = 0}}
      rich <- renderReportRich report
      -- The note's location sits inside the `spec` declaration of this very
      -- file, so the source listing should include the marked line.
      "splice-marker" `T.isInfixOf` rich `shouldBe` True

    it "degrades to plain renderReport when the source file is missing" $ do
      let loc' =
            SrcLoc
              { srcLocPackage = "zizek",
                srcLocModule = "ReportRendering",
                srcLocFile = "no/such/file.hs",
                srcLocStartLine = 1,
                srcLocStartCol = 1,
                srcLocEndLine = 1,
                srcLocEndCol = 1
              }
          result =
            Counterexample
              { message = "boom",
                notes = [Note {kind = Drawn, text = "42", loc = Just loc'}],
                loc = Nothing,
                diff = Nothing
              }
          report = Report {result, stats = Stats {valid = 3, invalid = 0}}
      rich <- renderReportRich report
      rich `shouldBe` renderReport report

  describe "PropertyFailed" $ do
    it "displayException agrees with renderFailure" $ do
      let notes = [drawn "50", footer "ft"]
          exc = PropertyFailed {message = "boom", notes, loc = Just aLoc, diff = Nothing}
      T.pack (displayException exc)
        `shouldBe` renderFailure "property failed: boom" notes (Just aLoc) Nothing

    it "displayException includes the diff when present" $ do
      let exc =
            PropertyFailed
              { message = "=== failed",
                notes = [],
                loc = Nothing,
                diff = Just [LineRemoved "1", LineAdded "2"]
              }
      "- 1" `isInfixOf` displayException exc `shouldBe` True
      "+ 2" `isInfixOf` displayException exc `shouldBe` True

  describe "throwOnFailure" $ do
    it "is silent on Ok" $ do
      throwOnFailure Report {result = Ok, stats = Stats {valid = 1, invalid = 0}}

    it "throws PropertyFailed on a counterexample" $ do
      let report =
            Report
              { result = Counterexample {message = "boom", notes = [], loc = Nothing, diff = Nothing},
                stats = Stats {valid = 1, invalid = 0}
              }
      throwOnFailure report `shouldThrow` \PropertyFailed {message} -> message == "boom"

  describe "equality assertions" $ do
    it "(===) passes on equal values" $ do
      (42 :: Int) === 42 :: IO ()

    it "(===) puts the diff in the diff field, not the message" $ do
      (Lines ["a", "b", "c"] === Lines ["a", "X", "c"])
        `shouldThrow` \AssertionFailure {message, diff} ->
          message == "=== failed"
            && diff
              == Just
                [ LineSame "<<a>>",
                  LineRemoved "<<b>>",
                  LineAdded "<<X>>",
                  LineSame "<<c>>"
                ]

    it "(===) produces a structural diff for parseable Show output" $ do
      (TwoField {p = 1, q = 2} === TwoField {p = 1, q = 3})
        `shouldThrow` \AssertionFailure {diff} ->
          isJust diff
            && any isRemoved (concat diff)
            && any isAdded (concat diff)

    it "(/==) throws when values are equal" $ do
      ((7 :: Int) /== 7)
        `shouldThrow` \AssertionFailure {message} ->
          "/== failed" `T.isPrefixOf` message

  describe "Hegel.Diff.diffShown" $ do
    it "returns Nothing for unparseable Show output" $ do
      diffShown "<<opaque>>" "<<other>>" `shouldBe` Nothing

    it "returns Just for parseable output" $ do
      diffShown (renderValue (Just (1 :: Int))) (renderValue (Just (2 :: Int)))
        `shouldSatisfy` isJust

-- * Helper predicates

isRemoved :: LineDiff -> Bool
isRemoved (LineRemoved _) = True
isRemoved _ = False

isAdded :: LineDiff -> Bool
isAdded (LineAdded _) = True
isAdded _ = False

-- * Test fixtures

-- | A 'Show' instance whose output cannot parse as a value AST.
data Opaque = Opaque

instance Show Opaque where
  show _ = "<<opaque>>"

-- | Multi-line 'Show' output that cannot parse as a value AST (so
-- 'renderValue' preserves it verbatim), for exercising the line-level diff
-- fallback path.
newtype Lines = Lines [Text]
  deriving stock (Eq)

instance Show Lines where
  show (Lines ls) = T.unpack (T.intercalate "\n" (fmap (\l -> "<<" <> l <> ">>") ls))

-- | A two-field record with a parseable 'Show' instance, for exercising the
-- structural diff path in '(===)'.
data TwoField = TwoField {p :: Int, q :: Int}
  deriving stock (Eq, Show)

-- | Convenience alias for type inference in 'diffShown' tests.
type Diff' = Diff
