-- | Abstract spine cells and the glyph tables that render them.
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

-- | One abstract spine cell.
--
-- Gutter cells and link cells occupy disjoint regions of a row, so the ascii
-- table only needs injectivity within each family; e.g. a corner @'@ and a
-- dead strand @'@ could never be confused as they cannot share a column.
data Cell
  = -- Gutter (strand) cells
    NodeBorn
  | NodeTouch
  | NodeDeath
  | NodeFail
  | EdgeAlive
  | EdgeDead
  | EdgeElided
  | HistoryEnd
  | -- Link cells
    LinkOrigin
  | LinkHorizontal
  | LinkVertical
  | LinkElided
  | -- | Origin-row junction: an inner column continues down to its cited row.
    LinkTeeDown
  | -- | Unused since the spine became failure-first only; retained so the
    -- link-cell family stays complete (transliteration, injectivity pins).
    LinkTeeUp
  | -- | Origin-row corner: the outermost column turns down.
    LinkCornerDown
  | -- | Cited-row corner: the column turns up-left into the arrowhead.
    LinkCornerUp
  | LinkArrow
  | -- Text-region sigils
    ElidedMark
  | Ellipsis
  | NumericCite
  | ResponseArrow
  | -- | The trajectory lead: the prefix on a degraded report's value-history
    -- breadcrumb (@↳ p₁: open \@1 · …@).
    TrajectoryLead
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
        NodeDeath -> "◌"
        NodeFail -> "✗"
        EdgeAlive -> "│"
        EdgeDead -> "┊"
        EdgeElided -> "┆"
        HistoryEnd -> "~"
        LinkOrigin -> "●"
        LinkHorizontal -> "─"
        LinkVertical -> "│"
        LinkElided -> "┆"
        LinkTeeDown -> "┬"
        LinkTeeUp -> "┴"
        LinkCornerDown -> "╮"
        LinkCornerUp -> "╯"
        LinkArrow -> "◀"
        ElidedMark -> "▸"
        Ellipsis -> "⋯"
        NumericCite -> "←"
        ResponseArrow -> "→"
        TrajectoryLead -> "↳"
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
        NodeDeath -> "%"
        NodeFail -> "x"
        EdgeAlive -> "|"
        EdgeDead -> "."
        EdgeElided -> ":"
        HistoryEnd -> "~"
        LinkOrigin -> "*"
        LinkHorizontal -> "-"
        LinkVertical -> "|"
        LinkElided -> ":"
        LinkTeeDown -> "+"
        LinkTeeUp -> "+"
        LinkCornerDown -> "."
        LinkCornerUp -> "'"
        LinkArrow -> "<"
        ElidedMark -> ">"
        Ellipsis -> "..."
        NumericCite -> "<-"
        ResponseArrow -> "->"
        TrajectoryLead -> "\\->"
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

-- | Unlabelled pools are lettered @v, w, x, y, z@ in birth order, doubling
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
-- ('Encoding.baseTransliterations') and this module's spine cell glyphs.
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
