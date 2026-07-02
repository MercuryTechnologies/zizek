-- | The composed report's style record; applies to glyphs, words, and the
-- trace layout.
--
-- The spine reads failure-first, while the lead reads chronologically.
module Hegel.Report.Style
  ( Style (..),
    LinkMode (..),
    defaultStyle,
  )
where

import Hegel.Report.Glyph (GlyphTable)
import Hegel.Report.Phrase (PhraseTable)
import Hegel.Report.Phrase qualified as Phrase

-- | How the citation spine renders a failure's citations: as the mid-line
-- link connectors (@●─┬─┬─╮@ \/ @◀─╯@ drawn to each cited step) or as the
-- numeric @← cites …@ list.
data LinkMode
  = -- | Always draw the link connectors, up to the link budget.
    Links
  | -- | Never draw connectors, always report the numeric citation list.
    Numeric
  deriving stock (Show, Eq)

data Style = Style
  { glyphs :: !GlyphTable,
    -- | The words for the headline, annotations, elisions, and footer
    -- — one table, so every section agrees by construction.
    phrases :: !PhraseTable,
    -- | How the citation spine renders citations.
    linkMode :: !LinkMode,
    -- | Maximum drawn link columns; more citations than this fall back to
    -- the numeric citation list on the failing row.
    linkBudget :: !Int,
    -- | Call-column clip budget (@rule args → response@), in characters.
    callWidth :: !Int
  }

-- | English, 'Numeric' link mode, link budget 3, call width 40 — over the given
-- glyph table.
defaultStyle :: GlyphTable -> Style
defaultStyle table =
  Style
    { glyphs = table,
      phrases = Phrase.english,
      linkMode = Numeric,
      linkBudget = 3,
      callWidth = 40
    }
