-- | A plain property with two distinct failures that share one cause.
--
-- A compact run-length codec writes a run's count only when it exceeds one,
-- so @"aab"@ encodes as @"2ab"@. That saves space until the input itself
-- contains a digit, which the decoder cannot tell apart from a count. The
-- property checks the encoding decodes at all, then that it round-trips, and
-- the run reports both failures: @"0"@ decodes to nothing, and @"0a"@
-- decodes to the empty string. Each annotation shows the encoder left its
-- input untouched, so the reader sees two symptoms of the same ambiguity.
module Gallery.Codec (scenario) where

import Data.Char (isDigit)
import Data.Function ((&))
import Data.List (group, sort)
import Data.Text (Text)
import Gallery.Scenario
import Hegel.Gen qualified as Gen
import Hegel.Property (Property, annotate, failure, forAll, (===))
import Hegel.Report (FailureEvidence (..), Note (..), NoteKind (Drawn))
import Hegel.Report qualified
import Hegel.Settings (Settings (..))

scenario :: Scenario
scenario =
  Scenario
    { name = "codec",
      title = "plain property: two failures, one ambiguous codec",
      settings = seeded {reportMultipleFailures = True},
      property = roundTrip,
      ascii = False,
      attempts = 1,
      expect = pure . check
    }

-- | Run-length encode a string, writing a run's count only when it exceeds
-- one.
encode :: String -> String
encode = concatMap run . group
  where
    run r = (if length r > 1 then show (length r) else "") <> take 1 r

-- | Invert 'encode', or 'Nothing' when a count has no character after it.
decode :: String -> Maybe String
decode [] = Just []
decode s = case span isDigit s of
  (_, []) -> Nothing
  (digits, c : rest) -> (replicate (if null digits then 1 else read digits) c <>) <$> decode rest

roundTrip :: Property ()
roundTrip = do
  s <- forAll (Gen.list (Gen.element "ab01") & Gen.maxSize 6 & Gen.build)
  let e = encode s
  annotate ("encoded as " <> Hegel.Report.renderValue e)
  case decode e of
    Nothing -> failure "the encoding decodes"
    Just s' -> s' === s

check :: Hegel.Report.Report -> [Text]
check report = case captured report of
  Left mismatch -> [mismatch]
  Right evidence ->
    ensureEqual "failure messages" ["=== failed, values are not equal", "the encoding decodes"] (sort [e.message | e <- evidence])
      <> ensureEqual "counterexamples" ["\"0\"", "\"0a\""] (sort (concatMap drawn evidence))
  where
    drawn :: FailureEvidence -> [Text]
    drawn e = take 1 [n.text | n <- e.notes, Drawn {} <- [n.kind]]
