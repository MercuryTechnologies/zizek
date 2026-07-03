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

    -- * Output-driven selection
    Preference (..),
    preference,
    table,
    cleanFor,
    sevenBitClean,
  )
where

import Data.Char qualified as Char
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Internal.Event (Var (..))
import Hegel.Report.Trace (Lifeline (..), Trace)
import Hegel.Report.Trace qualified as Trace
import Numeric (showHex)
import System.Environment (lookupEnv)
import System.IO (Handle, hGetEncoding)

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
-- Defined as @\\tbl trace -> \\v -> …@ so callers that bind
-- @nameOf = displayName tbl trace@ share the precomputed per-trace name
-- table across every lookup (the renderers call this per touch, per row).
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

-- | @₁₂₃@-style subscripts (borderline coverage tier: kept for now, plain
-- digits in the ascii table regardless).
subscript :: Int -> Text
subscript = T.map sub . T.pack . show
  where
    sub :: Char -> Char
    sub c = toEnum (fromEnum '₀' + fromEnum c - fromEnum '0')

-- * Output-driven selection

-- | Which table the current output should get. Unicode is the default
-- everywhere, including CI (modern log viewers render box drawing fine);
-- ascii is the escape hatch for genuinely legacy consumers.
data Preference = PreferUnicode | PreferAscii
  deriving stock (Show, Eq)

-- | Choose a table for output destined for the given handle. @HEGEL_GLYPHS@
-- (@ascii@ \/ @unicode@) overrides in both directions; otherwise a
-- non-UTF-capable encoding (e.g. @LANG=C@, where writing @✗@ would /throw/,
-- not mojibake) selects ascii — the never-crash requirement, answered by
-- detection rather than by forcing the handle's encoding. The caller names
-- the handle it is protecting (the integrations pass 'System.IO.stdout' as
-- their best knowledge of where the framework writes).
preference :: Handle -> IO Preference
preference h =
  lookupEnv "HEGEL_GLYPHS" >>= \case
    Just "ascii" -> pure PreferAscii
    Just "unicode" -> pure PreferUnicode
    _ -> do
      enc <- hGetEncoding h
      pure case enc of
        Just e | "UTF" `T.isInfixOf` T.pack (show e) -> PreferUnicode
        _ -> PreferAscii

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

-- | Make a rendered report 7-bit clean: /transliterate/ every glyph the
-- renderers are known to emit (derived from the cell tables, so it cannot
-- drift, plus the splice chrome and prose typography that predate the table
-- architecture — recorded debt, A2 in the roadmap note), and @\\xNNNN@-escape
-- only what remains — genuinely unknown user text. Applied by the
-- integrations to the whole rendered report when ascii is selected, so the
-- guarantee is total without turning the chrome into escape soup.
sevenBitClean :: Text -> Text
sevenBitClean = T.concatMap \c ->
  if Char.isAscii c
    then T.singleton c
    else case Map.lookup c transliterations of
      Just t -> t
      Nothing -> "\\x" <> T.pack (showHex (Char.ord c) "")

-- | Every single-glyph unicode cell maps to its ascii cell, plus the
-- not-yet-tabled chrome (source-splice borders, in-band failure marks,
-- headline rules) and prose typography.
transliterations :: Map.Map Char Text
transliterations =
  Map.fromList
    ( [ (u, ascii.cell c)
      | c <- [minBound .. maxBound],
        [u] <- [T.unpack (unicode.cell c)]
      ]
        <> [ ('┏', "+"),
             ('━', "-"),
             ('┃', "|"),
             ('⋮', ":"),
             ('·', "."),
             ('—', "--"),
             ('–', "-")
           ]
        <> [(toEnum (fromEnum '₀' + d), T.pack (show d)) | d <- [0 .. 9]]
    )
