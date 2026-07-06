-- | The composed report's style record; applies to glyphs, words, and the
-- trace layout.
--
-- The spine and the timeline both read chronologically (oldest first, the
-- failing step last).
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
    -- | The words for the headline, annotations, elisions, and footer
    -- — one table, so every section agrees by construction.
    phrases :: !PhraseTable,
    -- | Call-column clip budget (@rule args → response@), in characters.
    callWidth :: !Int
  }

-- | English wording, call width 40 — over the given glyph table.
defaultStyle :: GlyphTable -> Style
defaultStyle table =
  Style
    { glyphs = table,
      phrases = Phrase.english,
      callWidth = 40
    }
