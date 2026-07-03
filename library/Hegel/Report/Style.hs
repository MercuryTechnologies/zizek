-- | The composed report's style record: glyphs, words, and the trace
-- layout's knobs, in one place.
--
-- The renderers this styles — the citation ledger, the verdict paragraph,
-- the phenomenon chip, the reproduction footer — are sections of one
-- report, so they share one record (the ledger-specific fields ride along;
-- they are the only layout with knobs so far). Grew out of
-- @Ledger.Options@ once the composed report made that name a lie.
module Hegel.Report.Style
  ( Style (..),
    Direction (..),
    defaultStyle,
  )
where

import Hegel.Report.Glyph (GlyphTable)
import Hegel.Report.Phrase (PhraseTable)
import Hegel.Report.Phrase qualified as Phrase

-- | The trace ledger's reading order. 'FailureFirst' puts the failure at
-- eye level (the decided default); 'Chronological' reads as a story, and is
-- the only possible order for anything streamed.
data Direction = FailureFirst | Chronological
  deriving stock (Show, Eq)

data Style = Style
  { glyphs :: !GlyphTable,
    -- | The words for the verdict, annotations, elisions, chip, and footer
    -- — one table, so every section agrees by construction.
    phrases :: !PhraseTable,
    direction :: !Direction,
    -- | Maximum drawn rail columns; more citations than this fall back to
    -- the numeric citation list on the failing row.
    railBudget :: !Int,
    -- | Call-column clip budget (@rule args → response@), in characters.
    callWidth :: !Int
  }

-- | Failure-first, English, rail budget 3, call width 40 — over the given
-- glyph table.
defaultStyle :: GlyphTable -> Style
defaultStyle table =
  Style
    { glyphs = table,
      phrases = Phrase.english,
      direction = FailureFirst,
      railBudget = 3,
      callWidth = 40
    }
