-- | Abstract ledger cells and the glyph tables that render them.
--
-- Tenet 3 of the trace-rendering design
-- (@notes\/roadmap\/01-stateful-trace-rendering.md@): layout emits abstract
-- cell kinds; glyphs and colours are tables applied last. One layout engine
-- yields (unicode | ascii) for free, and losing /semantics/ (rather than
-- aesthetics) in the ascii table is a bug — pinned by the per-region
-- injectivity test in @tests\/unit\/LedgerRendering.hs@.
--
-- Every unicode pick stays in the note's /bulletproof/ (box drawing, @●@,
-- @○@) or /solid/ (@✗ ◌ ▸ ⋯@) coverage tiers; the provisional risky glyphs
-- from the early sketches (@◂@, @⇠@) were replaced by @◀@ and @←@ as decided
-- there.
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
  )
where

import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Internal.Event (Var (..))
import Hegel.Report.Trace (Lifeline (..), Trace)
import Hegel.Report.Trace qualified as Trace

-- | One abstract ledger cell. Gutter cells and rail cells occupy disjoint
-- regions of a row, so the ascii table only needs injectivity within each
-- family (a corner @'@ and a dead lane @'@ could never be confused; they
-- cannot share a column).
data Cell
  = -- Gutter (lane) cells
    NodeBorn
  | NodeTouch
  | NodeDeath
  | NodeFail
  | EdgeAlive
  | EdgeDead
  | EdgeElided
  | HistoryEnd
  | -- Rail cells
    RailOrigin
  | RailHoriz
  | RailVert
  | RailElided
  | -- | Junction continuing downward (failure-first origin row).
    RailTeeDown
  | -- | Junction continuing upward (chronological origin row).
    RailTeeUp
  | -- | Corner entering from above (chronological cited row).
    RailCornerDown
  | -- | Corner entering from below (failure-first cited row).
    RailCornerUp
  | RailArrow
  | -- Text-region sigils
    ElidedMark
  | Ellipsis
  | NumericCite
  | ResponseArrow
  | Blank
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | A rendering vocabulary: cell glyphs plus value naming.
data GlyphTable = GlyphTable
  { cell :: Cell -> Text,
    -- | @valueName label poolOrdinal valueOrdinal@ — the display name of a
    -- pool value: the pool's 'Hegel.Pool.named' label when present (@h₁@),
    -- otherwise a letter per pool in birth order (@v₁@, @w₂@, …), plus a
    -- subscript per value (birth order within the pool).
    valueName :: Maybe Text -> Int -> Int -> Text
  }

-- | The default table. Everything bulletproof or solid tier.
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
        RailOrigin -> "●"
        RailHoriz -> "─"
        RailVert -> "│"
        RailElided -> "┆"
        RailTeeDown -> "┬"
        RailTeeUp -> "┴"
        RailCornerDown -> "╮"
        RailCornerUp -> "╯"
        RailArrow -> "◀"
        ElidedMark -> "▸"
        Ellipsis -> "⋯"
        NumericCite -> "←"
        ResponseArrow -> "→"
        Blank -> " ",
      valueName = \label poolOrd valOrd ->
        maybe (poolLetter poolOrd) id label <> subscript valOrd
    }

-- | The escape hatch for windows-1252 pipelines, exotic log processors, and
-- @LANG=C@ consumers. Checkpoint-3 picks: @◌ → %@ (a digit-like @0@ next to
-- the step-number column read as part of the number) and @┊ → .@ (completing
-- the density gradient @| : .@ that mirrors @│ ┆ ┊@; the rail corner also
-- maps to @.@ but gutter and rail are disjoint families).
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
        RailOrigin -> "*"
        RailHoriz -> "-"
        RailVert -> "|"
        RailElided -> ":"
        RailTeeDown -> "+"
        RailTeeUp -> "+"
        RailCornerDown -> "."
        RailCornerUp -> "'"
        RailArrow -> "<"
        ElidedMark -> ">"
        Ellipsis -> "..."
        NumericCite -> "<-"
        ResponseArrow -> "->"
        Blank -> " ",
      valueName = \label poolOrd valOrd ->
        maybe (poolLetter poolOrd) id label <> T.pack (show valOrd)
    }

-- | A value's display name, resolved through its lineage root: the pool's
-- 'Hegel.Pool.named' label (or its birth-order letter) plus the value's
-- birth-order ordinal — one name for one logical value, across transfers.
-- Shared by the ledger and the verdict paragraph.
displayName :: GlyphTable -> Trace -> Var -> Text
displayName table trace v =
  let r = Trace.root trace v
      life = Trace.lifeline trace r
      poolOrdinals = nub [l.var.pool | l <- trace.lifelines]
      poolOrd = fromMaybe 0 (lookup r.pool (zip poolOrdinals [0 ..]))
   in table.valueName (life >>= (.label)) poolOrd (maybe 0 (.ordinal) life)

-- | Unlabelled pools are lettered @v, w, x, y, z@ in birth order, doubling
-- past five (@vv, ww, …@) so names never collide across pools.
poolLetter :: Int -> Text
poolLetter n = T.replicate (n `div` 5 + 1) (T.singleton letter)
  where
    letter = "vwxyz" !! (n `mod` 5)

-- | @₁₂₃@-style subscripts (borderline coverage tier: kept for now, plain
-- digits in the ascii table regardless).
subscript :: Int -> Text
subscript = T.map sub . T.pack . show
  where
    sub :: Char -> Char
    sub c = toEnum (fromEnum '₀' + fromEnum c - fromEnum '0')
