-- | The composed report's style record: glyphs, words, and the trace
-- layout's knobs, in one place.
--
-- The renderers this styles — the citation ledger, the verdict list,
-- the phenomenon chip, the reproduction footer — are sections of one
-- report, so they share one record (the ledger-specific fields ride along;
-- they are the only layout with knobs so far). Grew out of
-- @Ledger.Options@ once the composed report made that name a lie.
--
-- The ledger reads failure-first (the failure at eye level); the verdict
-- list reads chronologically. Neither order is a knob — they are fixed
-- editorial choices in their respective renderers.
module Hegel.Report.Style
  ( Style (..),
    defaultStyle,
  )
where

import Hegel.Report.Glyph (GlyphTable)
import Hegel.Report.Phrase (PhraseTable)
import Hegel.Report.Phrase qualified as Phrase

data Style = Style
  { glyphs :: !GlyphTable,
    -- | The words for the verdict, annotations, elisions, chip, and footer
    -- — one table, so every section agrees by construction.
    phrases :: !PhraseTable,
    -- | Maximum drawn link columns; more citations than this fall back to
    -- the numeric citation list on the failing row.
    linkBudget :: !Int,
    -- | Call-column clip budget (@rule args → response@), in characters.
    callWidth :: !Int
  }

-- | English, link budget 3, call width 40 — over the given glyph table.
defaultStyle :: GlyphTable -> Style
defaultStyle table =
  Style
    { glyphs = table,
      phrases = Phrase.english,
      linkBudget = 3,
      callWidth = 40
    }
