-- | Abstract log cells and the glyph tables that render them.
--
-- Designed for qualified import:
--
-- > import Hegel.Report.Glyph (Cell (..), GlyphTable)
-- > import Hegel.Report.Glyph qualified as Glyph
module Hegel.Report.Glyph
  ( Cell (..),
    GlyphTable (..),
    unicode,
    ascii,
    displayName,

    -- * Output-driven selection
    Preference (..),
    preference,
    table,
    cleanFor,
    sevenBitClean,
  )
where

import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Internal.Event (Var (..))
import Hegel.Report.Encoding (Preference (..), preference)
import Hegel.Report.Encoding qualified as Encoding
import Hegel.Report.Trace (Lifeline (..), Trace)
import Hegel.Report.Trace qualified as Trace

-- | One abstract log cell. The ascii table must render these injectively so
-- the transliteration ('cellTransliterations') is unambiguous.
data Cell
  = -- Gutter (strand) cells
    NodeBorn
  | NodeTouch
  | NodeTransfer
  | NodeDeath
  | NodeFail
  | EdgeAlive
  | EdgeElided
  | HistoryEnd
  | -- Text-region sigils
    ElidedMark
  | Ellipsis
  | NumericCite
  | CiteLead
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
        NodeBorn -> "●"
        NodeTouch -> "○"
        NodeTransfer -> "◉"
        NodeDeath -> "◌"
        NodeFail -> "✗"
        EdgeAlive -> "│"
        EdgeElided -> "┆"
        HistoryEnd -> "~"
        ElidedMark -> "▸"
        Ellipsis -> "⋯"
        NumericCite -> "←"
        CiteLead -> "↳"
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
        NodeBorn -> "*"
        NodeTouch -> "o"
        NodeTransfer -> "#"
        NodeDeath -> "%"
        NodeFail -> "x"
        EdgeAlive -> "|"
        EdgeElided -> ":"
        HistoryEnd -> "~"
        ElidedMark -> ">"
        Ellipsis -> "..."
        NumericCite -> "<-"
        CiteLead -> "\\_"
        ResponseArrow -> "->"
        Blank -> " ",
      valueName = \label poolOrd valOrd ->
        maybe (poolLetter poolOrd) id label <> T.pack (show valOrd)
    }

-- | A value's display name, resolved through its lineage root:
--
-- The pool's 'Hegel.Pool.named' label (or an automatic assignment) plus a
-- numeric identifier in order of a pooled variable's introduction.
--
-- Defined as @\\tbl trace -> \\v -> …@ so callers that bind
-- @nameOf = displayName tbl trace@ share the precomputed per-trace name
-- table across every lookup.
displayName :: GlyphTable -> Trace -> Var -> Text
displayName tbl trace =
  \v -> Map.findWithDefault (compute (Trace.root trace v)) (Trace.root trace v) precomputed
  where
    poolOrds :: Map.Map Int Int
    poolOrds = Map.fromList (zip (nub [l.var.pool | l <- trace.lifelines]) [0 ..])
    compute :: Var -> Text
    compute r =
      let life = Trace.lifeline trace r
       in tbl.valueName
            (life >>= (.label))
            (Map.findWithDefault 0 r.pool poolOrds)
            (maybe 0 (.ordinal) life)
    precomputed :: Map.Map Var Text
    precomputed = Map.fromList [(Trace.root trace l.var, compute (Trace.root trace l.var)) | l <- trace.lifelines]

-- | Unlabeled pools are lettered @v, w, x, y, z@ in birth order, doubling
-- past five (@vv, ww, …@) so names never collide across pools.
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
