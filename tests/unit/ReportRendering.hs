-- | Pure tests for 'Hegel.Report' rendering and assertion diffs; no engine
-- involved.
module ReportRendering (spec) where

import Control.Exception (displayException)
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (Forest, Tree (..), flatten)
import GHC.Stack (SrcLoc (..), callStack, getCallStack)
import Hegel.Assertion (AssertionFailure (..), (/==), (===))
import Hegel.Diff (Diff, LineDiff (..), diffShown)
import Hegel.Report
import Hegel.Report.Journal (groupByDepth, numberDraws)
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
drawn t = Note {kind = Drawn [], text = t, loc = Nothing, depth = 0, clock = Tick 0}
annotation t = Note {kind = Annotation, text = t, loc = Nothing, depth = 0, clock = Tick 0}
footer t = Note {kind = Footnote, text = t, loc = Nothing, depth = 0, clock = Tick 0}

-- | A step header (loc-less annotation) at top level, as emitted by
-- 'Hegel.Stateful'.
step :: Text -> Note
step t = Note {kind = Annotation, text = t, loc = Nothing, depth = 0, clock = Tick 0}

-- | A draw nested under a step (depth 1).
nestedDrawn :: Text -> Note
nestedDrawn t = Note {kind = Drawn [], text = t, loc = Nothing, depth = 1, clock = Tick 0}

-- | An in-band failure nested under a step (depth 1).
failureAt :: Text -> Maybe Diff -> Maybe SrcLoc -> Note
failureAt t d l = Note {kind = Failure d, text = t, loc = l, depth = 1, clock = Tick 0}

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
      renderReport Report {result = Ok, stats = Stats {valid = 100, invalid = 0}, databaseKey = Nothing}
        `shouldBe` "OK, passed 100 tests"

    it "renders discard counts" $ do
      renderReport Report {result = Ok, stats = Stats {valid = 100, invalid = 3}, databaseKey = Nothing}
        `shouldBe` "OK, passed 100 tests (3 discarded)"

    it "renders a counterexample with numbered draws, footnotes last" $ do
      let result =
            Counterexample
              { events = [],
                message = "sum stays small",
                notes =
                  [ footer "seen at the end",
                    drawn "50",
                    annotation "drew the first addend",
                    drawn "51"
                  ],
                loc = Just aLoc,
                diff = Nothing
              }
          report = Report {result, stats = Stats {valid = 12, invalid = 1}, databaseKey = Nothing}
      T.lines (renderReport report)
        `shouldBe` [ "failed after 12 tests (1 discarded)",
                     "sum stays small",
                     "  at tests/Spec.hs:42",
                     "  Draw 1: 50",
                     "  drew the first addend",
                     "  Draw 2: 51",
                     "  seen at the end"
                   ]

    it "renders a diff block, led by its legend, before the journal" $ do
      let result =
            Counterexample
              { events = [],
                message = "=== failed, values are not equal",
                notes = [drawn "some value"],
                loc = Just aLoc,
                diff = Just [LineRemoved "old", LineAdded "new"]
              }
          report = Report {result, stats = Stats {valid = 5, invalid = 0}, databaseKey = Nothing}
      T.lines (renderReport report)
        `shouldBe` [ "failed after 5 tests",
                     "=== failed, values are not equal",
                     "  (- lhs) (+ rhs)",
                     "  - old",
                     "  + new",
                     "  at tests/Spec.hs:42",
                     "  Draw 1: some value"
                   ]

    it "nests a stateful journal and renders the failure in-band (=== diff)" $ do
      -- The top-level message\/loc\/diff are populated (as they are for every
      -- counterexample) but must be *suppressed* in favour of the in-band
      -- 'Failure' note, so they do not appear twice.
      let result =
            Counterexample
              { events = [],
                message = "=== failed, values are not equal",
                notes =
                  [ step "Initial invariant check.",
                    step "Step 1: push",
                    nestedDrawn "0",
                    step "Step 2: push",
                    nestedDrawn "1",
                    step "Step 3: check_palindrome",
                    failureAt
                      "=== failed, values are not equal"
                      (Just [LineRemoved "Stack [ 1 , 0 ]", LineAdded "Stack [ 0 , 1 ]"])
                      (Just aLoc)
                  ],
                loc = Just aLoc,
                diff = Just [LineRemoved "Stack [ 1 , 0 ]", LineAdded "Stack [ 0 , 1 ]"]
              }
          report = Report {result, stats = Stats {valid = 5038, invalid = 0}, databaseKey = Nothing}
      T.lines (renderReport report)
        `shouldBe` [ "failed after 5038 tests",
                     "  Initial invariant check.",
                     "  Step 1: push",
                     "    Draw 1: 0",
                     "  Step 2: push",
                     "    Draw 1: 1",
                     "  Step 3: check_palindrome",
                     "    ✗ === failed, values are not equal",
                     "        (- lhs) (+ rhs)",
                     "        - Stack [ 1 , 0 ]",
                     "        + Stack [ 0 , 1 ]",
                     "      at tests/Spec.hs:42"
                   ]

    it "renders a stateful assert failure in-band (no diff)" $ do
      let result =
            Counterexample
              { events = [],
                message = "counter stays small",
                notes =
                  [ step "Initial invariant check.",
                    step "Step 1: increment",
                    failureAt "counter stays small" Nothing (Just aLoc)
                  ],
                loc = Just aLoc,
                diff = Nothing
              }
          report = Report {result, stats = Stats {valid = 11, invalid = 0}, databaseKey = Nothing}
      T.lines (renderReport report)
        `shouldBe` [ "failed after 11 tests",
                     "  Initial invariant check.",
                     "  Step 1: increment",
                     "    ✗ counter stays small",
                     "      at tests/Spec.hs:42"
                   ]

    it "hangs a multi-line drawn value under its own start column" $ do
      -- Pins the PP.align behaviour inside a nested step indent ahead of the
      -- tree-renderer refactor: continuation lines hang under the value's
      -- first column, not the note's indent column.
      let result =
            Counterexample
              { events = [],
                message = "boom",
                notes =
                  [ step "Step 1: push",
                    Note {kind = Drawn [], text = "Stack\n[ 1 ]", loc = Nothing, depth = 1, clock = Tick 0}
                  ],
                loc = Nothing,
                diff = Nothing
              }
          report = Report {result, stats = Stats {valid = 1, invalid = 0}, databaseKey = Nothing}
      T.lines (renderReport report)
        `shouldBe` [ "failed after 1 tests",
                     "boom",
                     "  Step 1: push",
                     "    Draw 1: Stack",
                     "            [ 1 ]"
                   ]

    it "renders orphan depth jumps at their stamped depth" $ do
      -- Nothing guarantees contiguous depths in a journal; pin the absolute
      -- (depth + 1) * 2 indent for a 0→2 jump ahead of the tree-renderer
      -- refactor, whose relative indents must telescope to the same columns.
      let result =
            Counterexample
              { events = [],
                message = "boom",
                notes =
                  [ step "Step 1: push",
                    Note {kind = Annotation, text = "deep note", loc = Nothing, depth = 2, clock = Tick 0},
                    Note {kind = Failure Nothing, text = "boom", loc = Just aLoc, depth = 2, clock = Tick 0}
                  ],
                loc = Just aLoc,
                diff = Nothing
              }
          report = Report {result, stats = Stats {valid = 1, invalid = 0}, databaseKey = Nothing}
      T.lines (renderReport report)
        `shouldBe` [ "failed after 1 tests",
                     "  Step 1: push",
                     "      deep note",
                     "      ✗ boom",
                     "        at tests/Spec.hs:42"
                   ]

    it "hoists a nested footnote to a fixed indent, dropping its depth" $ do
      -- 'footnote' is reachable inside a stateful rule body, so it can carry a
      -- nonzero depth. Hoisting already discards its position; it must discard
      -- its depth too, rather than render "nested" under the last step.
      let result =
            Counterexample
              { events = [],
                message = "boom",
                notes =
                  [ drawn "1",
                    Note {kind = Footnote, text = "nested footer", loc = Nothing, depth = 1, clock = Tick 0}
                  ],
                loc = Nothing,
                diff = Nothing
              }
          report = Report {result, stats = Stats {valid = 1, invalid = 0}, databaseKey = Nothing}
      T.lines (renderReport report)
        `shouldBe` [ "failed after 1 tests",
                     "boom",
                     "  Draw 1: 1",
                     "  nested footer"
                   ]

    it "renders gave-up and aborted verdicts" $ do
      renderReport Report {result = GaveUp "no valid examples", stats = Stats {valid = 0, invalid = 7}, databaseKey = Nothing}
        `shouldBe` "gave up after 0 tests (7 discarded): no valid examples"
      renderReport (aborted (UnhealthyInput "filter too much"))
        `shouldBe` "aborted: health check failed: filter too much"

  describe "Hegel.Report.Journal" $ do
    -- Compare on note text: 'Note' has no 'Eq', and text is unique per test.
    let shape :: Forest Note -> Forest Text
        shape = fmap (fmap (.text))
        at :: Int -> Text -> Note
        at d t = Note {kind = Annotation, text = t, loc = Nothing, depth = d, clock = Tick 0}

    it "keeps a flat depth-0 journal as sibling roots" $ do
      shape (groupByDepth [drawn "a", annotation "b", drawn "c"])
        `shouldBe` [Node "a" [], Node "b" [], Node "c" []]

    it "nests notes under the nearest preceding shallower note" $ do
      let ns = [step "s1", nestedDrawn "a", nestedDrawn "b", step "s2", nestedDrawn "c"]
      shape (groupByDepth ns)
        `shouldBe` [ Node "s1" [Node "a" [], Node "b" []],
                     Node "s2" [Node "c" []]
                   ]

    it "pops back out to a shallower sibling after a nested run" $ do
      let ns = [at 0 "root", at 1 "a", at 2 "a.1", at 1 "b"]
      shape (groupByDepth ns)
        `shouldBe` [Node "root" [Node "a" [Node "a.1" []], Node "b" []]]

    it "attaches an orphan depth jump under the nearest shallower note" $ do
      shape (groupByDepth [at 0 "s1", at 2 "deep"])
        `shouldBe` [Node "s1" [Node "deep" []]]

    it "preserves journal order under pre-order flattening" $ do
      let ns = [at 0 "a", at 1 "b", at 2 "c", at 1 "d", at 0 "e", at 2 "f"]
      concatMap flatten (shape (groupByDepth ns)) `shouldBe` fmap (.text) ns

    it "numbers draws among their siblings, 1-based, skipping non-draws" $ do
      -- The counter restarts per step (so the same forAll keeps its index
      -- across firings); top-level draws — the flat non-stateful journal —
      -- share one counter.
      let ns =
            [ step "s1",
              nestedDrawn "x",
              nestedDrawn "x2",
              step "s2",
              nestedDrawn "y",
              drawn "z",
              drawn "z2"
            ]
      concatMap (flatten . fmap fst) (numberDraws (groupByDepth ns))
        `shouldBe` [Nothing, Just 1, Just 2, Nothing, Just 1, Just 1, Just 2]

  describe "renderValue" $ do
    it "pretty-prints parseable Show output" $ do
      renderValue (Just (3 :: Int)) `shouldBe` "Just 3"

    it "falls back to the raw string for unparseable Show output" $ do
      renderValue Opaque `shouldBe` "<<opaque>>"

  describe "renderReportAnsi" $ do
    it "emits ANSI escape codes for a diff-bearing counterexample" $ do
      let result =
            Counterexample
              { events = [],
                message = "=== failed",
                notes = [],
                loc = Nothing,
                diff = Just [LineRemoved "old", LineAdded "new"]
              }
          report = Report {result, stats = Stats {valid = 1, invalid = 0}, databaseKey = Nothing}
      let plain = T.unpack (renderReport report)
          ansi = T.unpack (renderReportAnsi report)
      ansi `shouldNotBe` plain
      "\ESC[" `isInfixOf` ansi `shouldBe` True

  describe "renderReportRich" $ do
    it "splices the enclosing declaration for a Drawn note with a known location" $ do
      let loc' = hereLoc -- splice-marker: this line should appear in the rich report
          result =
            Counterexample
              { events = [],
                message = "boom",
                notes = [Note {kind = Drawn [], text = "42", loc = Just loc', depth = 0, clock = Tick 0}],
                loc = Nothing,
                diff = Nothing
              }
          report = Report {result, stats = Stats {valid = 3, invalid = 0}, databaseKey = Nothing}
      rich <- renderReportRich report
      -- The note's location sits inside the `spec` declaration of this very
      -- file, so the source listing should include the marked line.
      "splice-marker" `T.isInfixOf` rich `shouldBe` True

    it "degrades the failing step to structured lines when nothing splices" $ do
      -- The rich event-log path splices per-note; with every location
      -- unreadable (aLoc names a file that doesn't exist) the failing step
      -- falls back to its structured journal lines (header, ✗ message, loc),
      -- appended after the compact log — no source-splice chrome, but the
      -- reason still reads (the rich log is no longer byte-equal to plain).
      let result =
            Counterexample
              { events = [],
                message = "counter stays small",
                notes =
                  [ step "Step 1: increment",
                    failureAt "counter stays small" Nothing (Just aLoc)
                  ],
                loc = Just aLoc,
                diff = Nothing
              }
          report = Report {result, stats = Stats {valid = 4, invalid = 0}, databaseKey = Nothing}
      rich <- renderReportRich report
      rich `shouldSatisfy` T.isInfixOf "Step 1: increment"
      rich `shouldSatisfy` T.isInfixOf "✗ counter stays small"
      rich `shouldSatisfy` T.isInfixOf "at tests/Spec.hs:42"
      -- Nothing spliced, so no source-listing chrome.
      T.count "┏━━" rich `shouldBe` 0

    it "splices the failing step's notes into their source" $ do
      let drawLoc = hereLoc -- stateful-splice-marker-draw
          failLoc = hereLoc -- stateful-splice-marker-fail
          result =
            Counterexample
              { events = [],
                message = "boom",
                notes =
                  [ step "Step 1: rule_a",
                    Note {kind = Drawn [], text = "42", loc = Just drawLoc, depth = 1, clock = Tick 0},
                    Note {kind = Failure Nothing, text = "boom", loc = Just failLoc, depth = 1, clock = Tick 0}
                  ],
                loc = Just failLoc,
                diff = Nothing
              }
          report = Report {result, stats = Stats {valid = 3, invalid = 0}, databaseKey = Nothing}
      rich <- renderReportRich report
      -- The step header stays on the spine; the draw and the failure splice
      -- into this very declaration, under one listing header.
      ("Step 1: rule_a" `T.isInfixOf` rich) `shouldBe` True
      ("stateful-splice-marker-draw" `T.isInfixOf` rich) `shouldBe` True
      ("stateful-splice-marker-fail" `T.isInfixOf` rich) `shouldBe` True
      T.count "┏━━" rich `shouldBe` 1

    it "mixes spliced and structured notes in the failing step (per-note fallback)" $ do
      let goodLoc = hereLoc -- stateful-mix-marker
          badLoc = aLoc -- names a file that does not exist
          result =
            Counterexample
              { events = [],
                message = "boom",
                notes =
                  [ step "Step 1: mixed",
                    Note {kind = Drawn [], text = "7", loc = Just badLoc, depth = 1, clock = Tick 0},
                    Note {kind = Failure Nothing, text = "boom", loc = Just goodLoc, depth = 1, clock = Tick 0}
                  ],
                loc = Just goodLoc,
                diff = Nothing
              }
          report = Report {result, stats = Stats {valid = 3, invalid = 0}, databaseKey = Nothing}
      rich <- renderReportRich report
      -- The failure splices; the unreadable draw keeps its structured line.
      ("stateful-mix-marker" `T.isInfixOf` rich) `shouldBe` True
      ("Draw 1: 7" `T.isInfixOf` rich) `shouldBe` True

    it "keeps the top-level headline for a Failure-less step journal" $ do
      -- An exception mid-loop (or a failure in machine.initial) leaves a step
      -- journal with no in-band Failure note: the log has no ✗ row and nothing
      -- splices, so the top-level message/location must lead — else nothing
      -- states why the run failed.
      let result =
            Counterexample
              { events = [],
                message = "boom: exception in rule",
                notes =
                  [ step "Step 1: rule_a",
                    Note {kind = Drawn [], text = "42", loc = Nothing, depth = 1, clock = Tick 0}
                  ],
                loc = Just aLoc,
                diff = Nothing
              }
          report = Report {result, stats = Stats {valid = 2, invalid = 0}, databaseKey = Nothing}
      rich <- renderReportRich report
      -- The headline and location are kept (no in-band ✗ carries the reason)…
      rich `shouldSatisfy` T.isInfixOf "boom: exception in rule"
      rich `shouldSatisfy` T.isInfixOf "at tests/Spec.hs:42"
      -- …the step still shows in the log, but there is no failure mark.
      rich `shouldSatisfy` T.isInfixOf "rule_a"
      rich `shouldNotSatisfy` T.isInfixOf "✗"

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
              { events = [],
                message = "boom",
                notes = [Note {kind = Drawn [], text = "42", loc = Just loc', depth = 0, clock = Tick 0}],
                loc = Nothing,
                diff = Nothing
              }
          report = Report {result, stats = Stats {valid = 3, invalid = 0}, databaseKey = Nothing}
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
      throwOnFailure Report {result = Ok, stats = Stats {valid = 1, invalid = 0}, databaseKey = Nothing}

    it "throws PropertyFailed on a counterexample" $ do
      let report =
            Report
              { result = Counterexample {message = "boom", notes = [], events = [], loc = Nothing, diff = Nothing},
                stats = Stats {valid = 1, invalid = 0},
                databaseKey = Nothing
              }
      throwOnFailure report `shouldThrow` \PropertyFailed {message} -> message == "boom"

  describe "equality assertions" $ do
    it "(===) passes on equal values" $ do
      (42 :: Int) === 42 :: IO ()

    it "(===) puts the diff in the diff field, not the message" $ do
      (Lines ["a", "b", "c"] === Lines ["a", "X", "c"])
        `shouldThrow` \AssertionFailure {message, diff} ->
          message == "=== failed, values are not equal"
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
