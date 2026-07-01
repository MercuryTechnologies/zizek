-- | Pure tests for 'Hegel.Report' rendering and assertion diffs; no engine
-- involved.
module ReportRendering (spec) where

import Control.Exception (displayException)
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Stack (SrcLoc (..), callStack, getCallStack)
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
drawn t = Note {kind = Drawn, text = t, loc = Nothing, diff = Nothing, depth = 0}
annotation t = Note {kind = Annotation, text = t, loc = Nothing, diff = Nothing, depth = 0}
footer t = Note {kind = Footnote, text = t, loc = Nothing, diff = Nothing, depth = 0}

-- | A step header (loc-less annotation) at top level, as emitted by
-- 'Hegel.Stateful'.
step :: Text -> Note
step t = Note {kind = Annotation, text = t, loc = Nothing, diff = Nothing, depth = 0}

-- | A draw nested under a step (depth 1).
nestedDrawn :: Text -> Note
nestedDrawn t = Note {kind = Drawn, text = t, loc = Nothing, diff = Nothing, depth = 1}

-- | An in-band failure nested under a step (depth 1).
failureAt :: Text -> Maybe Diff -> Maybe SrcLoc -> Note
failureAt t d l = Note {kind = Failure, text = t, loc = l, diff = d, depth = 1}

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

    it "nests a stateful journal and renders the failure in-band (=== diff)" $ do
      -- The top-level message\/loc\/diff are populated (as they are for every
      -- counterexample) but must be *suppressed* in favour of the in-band
      -- 'Failure' note, so they do not appear twice.
      let result =
            Counterexample
              { message = "=== failed",
                notes =
                  [ step "Initial invariant check.",
                    step "Step 1: push",
                    nestedDrawn "0",
                    step "Step 2: push",
                    nestedDrawn "1",
                    step "Step 3: check_palindrome",
                    failureAt
                      "=== failed"
                      (Just [LineRemoved "Stack [ 1 , 0 ]", LineAdded "Stack [ 0 , 1 ]"])
                      (Just aLoc)
                  ],
                loc = Just aLoc,
                diff = Just [LineRemoved "Stack [ 1 , 0 ]", LineAdded "Stack [ 0 , 1 ]"]
              }
          report = Report {result, stats = Stats {valid = 5038, invalid = 0}}
      T.lines (renderReport report)
        `shouldBe` [ "failed after 5038 tests",
                     "  Initial invariant check.",
                     "  Step 1: push",
                     "    forAll 1 = 0",
                     "  Step 2: push",
                     "    forAll 2 = 1",
                     "  Step 3: check_palindrome",
                     "    ✗ === failed",
                     "        - Stack [ 1 , 0 ]",
                     "        + Stack [ 0 , 1 ]",
                     "      at tests/Spec.hs:42"
                   ]

    it "renders a stateful assert failure in-band (no diff)" $ do
      let result =
            Counterexample
              { message = "counter stays small",
                notes =
                  [ step "Initial invariant check.",
                    step "Step 1: increment",
                    failureAt "counter stays small" Nothing (Just aLoc)
                  ],
                loc = Just aLoc,
                diff = Nothing
              }
          report = Report {result, stats = Stats {valid = 11, invalid = 0}}
      T.lines (renderReport report)
        `shouldBe` [ "failed after 11 tests",
                     "  Initial invariant check.",
                     "  Step 1: increment",
                     "    ✗ counter stays small",
                     "      at tests/Spec.hs:42"
                   ]

    it "hoists a nested footnote to a fixed indent, dropping its depth" $ do
      -- 'footnote' is reachable inside a stateful rule body, so it can carry a
      -- nonzero depth. Hoisting already discards its position; it must discard
      -- its depth too, rather than render "nested" under the last step.
      let result =
            Counterexample
              { message = "boom",
                notes =
                  [ drawn "1",
                    Note {kind = Footnote, text = "nested footer", loc = Nothing, diff = Nothing, depth = 1}
                  ],
                loc = Nothing,
                diff = Nothing
              }
          report = Report {result, stats = Stats {valid = 1, invalid = 0}}
      T.lines (renderReport report)
        `shouldBe` [ "failed after 1 tests",
                     "boom",
                     "  forAll 1 = 1",
                     "  nested footer"
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
                notes = [Note {kind = Drawn, text = "42", loc = Just loc', diff = Nothing, depth = 0}],
                loc = Nothing,
                diff = Nothing
              }
          report = Report {result, stats = Stats {valid = 3, invalid = 0}}
      rich <- renderReportRich report
      -- The note's location sits inside the `spec` declaration of this very
      -- file, so the source listing should include the marked line.
      "splice-marker" `T.isInfixOf` rich `shouldBe` True

    it "degrades to the structured layout for a Failure-bearing journal" $ do
      -- Stateful reports carry an in-band 'Failure' note; the rich renderer
      -- must reuse the plain structured layout rather than attempt to splice
      -- source, so rich and plain agree byte-for-byte.
      let result =
            Counterexample
              { message = "counter stays small",
                notes =
                  [ step "Step 1: increment",
                    failureAt "counter stays small" Nothing (Just aLoc)
                  ],
                loc = Just aLoc,
                diff = Nothing
              }
          report = Report {result, stats = Stats {valid = 4, invalid = 0}}
      rich <- renderReportRich report
      rich `shouldBe` renderReport report

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
                notes = [Note {kind = Drawn, text = "42", loc = Just loc', diff = Nothing, depth = 0}],
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

    it "displayException keeps the headline for a Failure-bearing journal" $ do
      -- The in-band layout suppresses the top-level message; the report
      -- renderers replace it with "failed after N tests", but
      -- 'displayException' has no such summary, so it must supply the
      -- "property failed: …" headline itself.
      let notes =
            [ step "Step 1: increment",
              failureAt "counter stays small" Nothing (Just aLoc)
            ]
          exc = PropertyFailed {message = "counter stays small", notes, loc = Just aLoc, diff = Nothing}
      T.lines (T.pack (displayException exc))
        `shouldBe` [ "property failed: counter stays small",
                     "  Step 1: increment",
                     "    ✗ counter stays small",
                     "      at tests/Spec.hs:42"
                   ]

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
