-- | A forked worker failing beside top-level activity.
--
-- An uploader hands its payload to a background worker that checksums it in
-- fixed-size chunks while the upload proceeds. The worker only sums whole
-- chunks, so a payload whose length isn't a multiple of the chunk size loses
-- its tail from the checksum. The smallest case is a single nonzero byte
-- checksummed two at a time.
--
-- The payload draw and the upload annotation belong to the property itself
-- and render unlabeled. The worker's chunk-size draw and its failure carry the
-- fork's label.
module Gallery.Upload (scenario) where

import Data.Function ((&))
import Data.Text (Text)
import Gallery.Scenario
import Hegel.Gen qualified as Gen
import Hegel.Property (Property, annotate, forAllWithLabel, (===))
import Hegel.Property.Fork qualified as Fork
import Hegel.Report (FailureEvidence (..), Note (..), NoteKind (Drawn), Report, isBranchFailure, renderValue)

scenario :: Scenario
scenario =
  Scenario
    { name = "upload",
      title = "fork: a background checksum drops the last chunk",
      settings = seeded,
      property = upload,
      ascii = False,
      attempts = 1,
      expect = pure . check
    }

-- | Sum a payload chunk by chunk.
--
-- BUG: a final chunk shorter than @n@ is never summed.
chunkedSum :: Int -> [Int] -> Int
chunkedSum n xs
  | length chunk < n = 0
  | otherwise = sum chunk + chunkedSum n rest
  where
    (chunk, rest) = splitAt n xs

upload :: Property ()
upload = do
  payload <- forAllWithLabel "payload" (Gen.list (Gen.int & Gen.min 0 & Gen.max 255 & Gen.build) & Gen.minSize 1 & Gen.maxSize 16 & Gen.build)
  checksum <- Fork.spawn do
    chunk <- forAllWithLabel "chunk" (Gen.int & Gen.min 2 & Gen.max 8 & Gen.build)
    chunkedSum chunk payload === sum payload
  annotate ("uploading " <> renderValue (length payload) <> (if length payload == 1 then " byte" else " bytes"))
  Fork.join checksum

check :: Report -> [Text]
check report = case captured report of
  Right [evidence] ->
    ensureEqual "counterexample" ["payload=[ 1 ]", "chunk=2"] [n.text | n <- evidence.notes, Drawn {} <- [n.kind]]
      <> ensureEqual "failing forks" 1 (length (filter isBranchFailure evidence.notes))
  Right evidence -> ["expected one failure, got " <> renderValue (length evidence)]
  Left mismatch -> [mismatch]
