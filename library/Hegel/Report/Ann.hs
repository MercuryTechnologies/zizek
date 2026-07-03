-- | Semantic annotations on report fragments and their ANSI rendering.
--
-- Factored out of "Hegel.Report" so that "Hegel.Report.Source" can import
-- them without creating a module cycle.
module Hegel.Report.Ann
  ( -- * Semantic annotations
    Ann (..),
    Style (..),

    -- * Document rendering
    docToText,
    docToAnsi,

    -- * Shared layout helpers
    diffDocs,
    lineDiffText,
    lineDiffAnn,
  )
where

import Data.Text (Text)
import Hegel.Diff (LineDiff (..))
import Prettyprinter (Doc)
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal (AnsiStyle)
import Prettyprinter.Render.Terminal qualified as PP.Terminal
import Prettyprinter.Render.Text qualified as PP.Text

-- | Source-line styling levels, mirroring hedgehog's @Style@.
-- 'StyleFailure' takes priority over 'StyleAnnotation', which takes priority
-- over 'StyleDefault'.
data Style = StyleDefault | StyleAnnotation | StyleFailure
  deriving stock (Eq, Ord, Show)

instance Semigroup Style where
  StyleFailure <> _ = StyleFailure
  _ <> StyleFailure = StyleFailure
  StyleAnnotation <> _ = StyleAnnotation
  _ <> StyleAnnotation = StyleAnnotation
  StyleDefault <> StyleDefault = StyleDefault

instance Monoid Style where
  mempty = StyleDefault

-- | Semantic annotations on report fragments.  The plain-text renderer strips
-- them; the ANSI renderer maps them to colours.
data Ann
  = -- | The failure headline message.
    MessageAnn
  | -- | Source location (@at file:line@).
    LocAnn
  | -- | A @forAll@-style drawn value.
    DrawnAnn
  | -- | An inline annotation or footnote.
    NoteAnn
  | -- | Diff context line.
    DiffSame
  | -- | Diff removed line.
    DiffRemoved
  | -- | Diff added line.
    DiffAdded
  | -- | File path in the @┏━━ file ━━━@ header.
    DeclLocation
  | -- | Line-number gutter, coloured by 'Style'.
    StyledLineNo !Style
  | -- | @┏━━@ \/ @┃@ border, coloured by 'Style'.
    StyledBorder !Style
  | -- | Source text, coloured by 'Style'.
    StyledSource !Style
  | -- | @│ @ gutter for inlined annotation values.
    AnnotationGutter
  | -- | Inlined annotation value text.
    AnnotationValue
  | -- | A marker pointing at the failure: the @^^^@ arrows under a failing
    -- expression in spliced source, or the @✗@ on an in-band failure note.
    FailureMark
  | -- | @│ @ gutter for inlined failure message \/ diff.
    FailureGutter
  | -- | Inlined failure message text.
    FailureMessage
  | -- | A trace-ledger lane glyph (and any value name in text), coloured by
    -- the lane's index — glyphs carry state, columns carry identity, and the
    -- colour binds a value's name in prose to its lane with zero geometry.
    LaneAnn !Int
  | -- | A citation-rail cell, coloured as the lane of the value the edge
    -- concerns.
    RailAnn !Int
  | -- | A ledger step number (dim).
    StepNoAnn
  | -- | A rule's @→ response@ segment on a ledger row.
    ResponseAnn
  | -- | Elision rows and other droppable ledger detail (dim).
    ElidedAnn

-- | Render a 'Doc Ann' as plain text, stripping all annotations.
docToText :: Doc Ann -> Text
docToText = PP.Text.renderStrict . PP.layoutPretty PP.defaultLayoutOptions

-- | Render a 'Doc Ann' with ANSI colour codes.
docToAnsi :: Doc Ann -> Text
docToAnsi =
  PP.Terminal.renderStrict
    . PP.layoutPretty PP.defaultLayoutOptions
    . PP.reAnnotate annToAnsi

-- | Render a diff, one 'Doc' per line: the legend first, then each diff line
-- with its @  @\/@- @\/@+ @ prefix and semantic annotation.  Shared by the
-- plain failure layout in "Hegel.Report" and the source-inlined diff in
-- "Hegel.Report.Source".
diffDocs :: [LineDiff] -> [Doc Ann]
diffDocs d = diffLegend : fmap lineDiffDoc d

lineDiffDoc :: LineDiff -> Doc Ann
lineDiffDoc d = PP.annotate (lineDiffAnn d) (PP.pretty (lineDiffText d))

-- | A diff line as prefixed text — the one home of the @  @\/@- @\/@+ @
-- prefix vocabulary (the ledger's detail rows render from this too).
lineDiffText :: LineDiff -> Text
lineDiffText = \case
  LineSame t -> "  " <> t
  LineRemoved t -> "- " <> t
  LineAdded t -> "+ " <> t

-- | The annotation matching 'lineDiffText'.
lineDiffAnn :: LineDiff -> Ann
lineDiffAnn = \case
  LineSame _ -> DiffSame
  LineRemoved _ -> DiffRemoved
  LineAdded _ -> DiffAdded

-- | Legend tying the diff markers to the @(===)@ operands: @(- lhs) (+ rhs)@,
-- each token coloured to match the diff lines it keys (hedgehog's header
-- convention). An interleaved diff doesn't otherwise say which operand a
-- @-@ line came from.
diffLegend :: Doc Ann
diffLegend =
  "(" <> PP.annotate DiffRemoved "- lhs" <> ") (" <> PP.annotate DiffAdded "+ rhs" <> ")"

annToAnsi :: Ann -> AnsiStyle
annToAnsi = \case
  MessageAnn -> PP.Terminal.bold
  LocAnn -> PP.Terminal.colorDull PP.Terminal.White
  DrawnAnn -> PP.Terminal.color PP.Terminal.Cyan
  NoteAnn -> mempty
  DiffSame -> mempty
  DiffRemoved -> PP.Terminal.color PP.Terminal.Red
  DiffAdded -> PP.Terminal.color PP.Terminal.Green
  DeclLocation -> mempty
  StyledLineNo StyleDefault -> mempty
  StyledLineNo StyleAnnotation -> PP.Terminal.colorDull PP.Terminal.Magenta
  StyledLineNo StyleFailure -> PP.Terminal.color PP.Terminal.Red
  StyledBorder _ -> mempty
  StyledSource StyleDefault -> mempty
  StyledSource StyleAnnotation -> mempty
  StyledSource StyleFailure -> PP.Terminal.color PP.Terminal.Red <> PP.Terminal.bold
  AnnotationGutter -> PP.Terminal.colorDull PP.Terminal.Magenta
  AnnotationValue -> PP.Terminal.colorDull PP.Terminal.Magenta
  FailureMark -> PP.Terminal.color PP.Terminal.Red
  FailureGutter -> mempty
  FailureMessage -> mempty
  LaneAnn n -> laneColor n
  RailAnn n -> laneColor n
  -- No SGR-2 faint in prettyprinter-ansi-terminal; dull white is the
  -- established "dim" approximation (see 'LocAnn').
  StepNoAnn -> PP.Terminal.colorDull PP.Terminal.White
  ResponseAnn -> mempty
  ElidedAnn -> PP.Terminal.colorDull PP.Terminal.White

-- | Lane colours cycle through five theme-safe hues; never red (reserved
-- for failure), never white\/black\/grey (theme-fragile). Ordering is
-- colourblind-aware: the deutan confusion pair is deferred to lane 5.
laneColor :: Int -> AnsiStyle
laneColor n =
  PP.Terminal.color case n `mod` 5 of
    0 -> PP.Terminal.Cyan
    1 -> PP.Terminal.Magenta
    2 -> PP.Terminal.Yellow
    3 -> PP.Terminal.Blue
    _ -> PP.Terminal.Green
