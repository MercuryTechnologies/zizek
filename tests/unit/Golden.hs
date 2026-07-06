-- | A readable matcher for multi-line rendered output (the event log, full
-- reports — anything whose test is a 2-D character grid).
--
-- 'shouldBe' on 'Text' reports a mismatch through 'show', which escapes every
-- box-drawing glyph to @\\9474@-style codepoints: unreadable, and impossible to
-- paste back. 'shouldRenderAs' instead prints the expected and actual blocks
-- with their glyphs intact, marks the lines that differ, and emits a
-- ready-to-paste list literal, so an intentional layout change is eyeballed and
-- pasted rather than hand-decoded.
--
-- Goldens stay inline in the test source (reviewable in the diff, no golden-file
-- or working-directory coupling); only the /failure ergonomics/ change.
module Golden (shouldRenderAs) where

import Control.Monad (unless)
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec (Expectation, expectationFailure)

-- | @actual \`shouldRenderAs\` expectedLines@ asserts that @actual@, split on
-- newlines, equals @expectedLines@ (the line-list form the goldens are written
-- in). On mismatch, fails with a readable, paste-ready report.
shouldRenderAs :: Text -> [Text] -> Expectation
shouldRenderAs actual expectedLines =
  unless (actualLines == expectedLines) do
    expectationFailure . T.unpack . T.intercalate "\n" $
      ["rendered layout did not match the golden (‹ = expected, › = actual):", ""]
        <> diffBlock
        <> ["", "paste-ready actual:"]
        <> fmap pasteLine actualLines
  where
    actualLines = T.lines actual

    -- Interleave the two blocks, showing matching lines once and flagging each
    -- differing pair, so the offending row is obvious in a tall grid.
    diffBlock =
      concat
        [ if e == a then ["  " <> e] else ["‹ " <> e, "› " <> a]
        | (e, a) <- zipPad expectedLines actualLines
        ]

    -- One element of the expected list literal, glyphs intact, ready to paste
    -- over the old list. Only @\\@ and @"@ need escaping; layout glyphs do not.
    pasteLine l = "    " <> "\"" <> T.concatMap esc l <> "\","
    esc '\\' = "\\\\"
    esc '"' = "\\\""
    esc c = T.singleton c

-- | Zip to the length of the longer list, padding the short one with @""@.
zipPad :: [Text] -> [Text] -> [(Text, Text)]
zipPad [] [] = []
zipPad (x : xs) (y : ys) = (x, y) : zipPad xs ys
zipPad (x : xs) [] = (x, "") : zipPad xs []
zipPad [] (y : ys) = ("", y) : zipPad [] ys
