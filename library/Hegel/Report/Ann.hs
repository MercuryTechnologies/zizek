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
lineDiffDoc = \case
  LineSame t -> PP.annotate DiffSame ("  " <> PP.pretty t)
  LineRemoved t -> PP.annotate DiffRemoved ("- " <> PP.pretty t)
  LineAdded t -> PP.annotate DiffAdded ("+ " <> PP.pretty t)

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
