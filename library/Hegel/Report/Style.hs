-- | The composed report's style record: glyphs, wording, and layout budgets.
--
-- Designed for qualified import:
--
-- > import Hegel.Report.Style (Cell (..), Style (..))
-- > import Hegel.Report.Style qualified as Style
module Hegel.Report.Style
  ( -- * Style
    Style (..),
    defaultStyle,

    -- * Glyphs
    Cell (..),
    GlyphTable (..),
    unicode,
    ascii,

    -- * Phrases
    PhraseTable (..),
    english,
    firstLine,

    -- * Output-driven selection
    Preference (..),
    preference,
    table,
    cleanFor,
    sevenBitClean,
  )
where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Report.Encoding (Preference (..), preference)
import Hegel.Report.Encoding qualified as Encoding

-- * Glyphs

-- | One abstract log cell. The ascii table must render these injectively so
-- the transliteration ('cellTransliterations') is unambiguous.
data Cell
  = NodeFail
  | ElidedMark
  | Ellipsis
  | ResponseArrow
  | Blank
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | A rendering vocabulary: cell glyphs plus value naming.
data GlyphTable = GlyphTable
  { cell :: Cell -> Text,
    valueName :: Maybe Text -> Int -> Int -> Text
  }

-- | The default glyph table for anything that supports UTF-8 display.
unicode :: GlyphTable
unicode =
  GlyphTable
    { cell = \case
        NodeFail -> "✗"
        ElidedMark -> "▸"
        Ellipsis -> "⋯"
        ResponseArrow -> "→"
        Blank -> " ",
      valueName = \label poolOrd valOrd ->
        maybe (poolLetter poolOrd) id label <> subscript valOrd
    }

-- | A fallback glyph table for anything that doesn't support UTF-8 display.
ascii :: GlyphTable
ascii =
  GlyphTable
    { cell = \case
        NodeFail -> "x"
        ElidedMark -> ">"
        Ellipsis -> "..."
        ResponseArrow -> "->"
        Blank -> " ",
      valueName = \label poolOrd valOrd ->
        maybe (poolLetter poolOrd) id label <> T.pack (show valOrd)
    }

-- | Unlabeled pools are lettered @v, w, x, y, z@ in birth order, doubling
-- past five (@vv, ww, ...@) so names never collide across pools.
poolLetter :: Int -> Text
poolLetter n = T.replicate (n `div` 5 + 1) (T.singleton letter)
  where
    letter = "vwxyz" !! (n `mod` 5)

-- | @₁₂₃@-style subscripts
subscript :: Int -> Text
subscript = T.map sub . T.pack . show
  where
    sub :: Char -> Char
    sub c = toEnum (fromEnum '₀' + fromEnum c - fromEnum '0')

-- * Phrases

-- | The fixed wording for every sentence the event log emits.
--
-- The log composes its sentences exclusively from these fields plus
-- /quoted/ user data.
data PhraseTable = PhraseTable
  { -- | An elision row's label: @\"2 steps elided\"@, or
    -- @\"2 steps elided (h₂)\"@ naming the value(s) the elided run touched.
    elidedSteps :: Int -> Maybe Text -> Text,
    -- | The reproduction footer, given the database key:
    -- @\"stored: k — replays automatically next run\"@.
    stored :: Text -> Text
  }

-- | The default phrase table (English wording).
english :: PhraseTable
english =
  PhraseTable
    { elidedSteps = \n mConcerns ->
        counted n "step" <> " elided" <> maybe "" (\c -> " (" <> c <> ")") mConcerns,
      stored = \key -> "stored: " <> key <> " — replays automatically next run"
    }
  where
    counted :: Int -> Text -> Text
    counted n noun =
      T.pack (show n) <> " " <> noun <> (if n == 1 then "" else "s")

-- | Quote only the first physical line of user text where a single line is
-- structurally required.
firstLine :: Text -> Text
firstLine = T.takeWhile (/= '\n')

-- * Style

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
defaultStyle glyphTable =
  Style
    { glyphs = glyphTable,
      phrases = english,
      callWidth = 40
    }

-- * Output-driven selection

table :: Preference -> GlyphTable
table = \case
  PreferUnicode -> unicode
  PreferAscii -> ascii

-- | The text-cleaning pass a preference implies: 'sevenBitClean' for ascii
-- (the 7-bit guarantee covers user text too), identity otherwise.
cleanFor :: Preference -> Text -> Text
cleanFor = \case
  PreferAscii -> sevenBitClean
  PreferUnicode -> id

-- | Make a rendered report 7-bit clean, covering both the base chrome
-- ('Encoding.baseTransliterations') and this module's log cell glyphs.
sevenBitClean :: Text -> Text
sevenBitClean =
  Encoding.sevenBitCleanWith (Encoding.baseTransliterations <> cellTransliterations)

-- | Every single-glyph unicode cell maps to its ascii cell.
cellTransliterations :: Map.Map Char Text
cellTransliterations =
  Map.fromList
    [ (u, ascii.cell c)
    | c <- [minBound .. maxBound],
      [u] <- [T.unpack (unicode.cell c)]
    ]
