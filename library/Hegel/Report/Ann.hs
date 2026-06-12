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
    lineDiffDoc,
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
  | -- | @^^^@ arrows under the failing expression.
    FailureArrows
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

-- | Render one diff line with its @  @\/@- @\/@+ @ prefix and semantic
-- annotation.  Shared by the plain failure layout in "Hegel.Report" and the
-- source-inlined diff in "Hegel.Report.Source".
lineDiffDoc :: LineDiff -> Doc Ann
lineDiffDoc = \case
  LineSame t -> PP.annotate DiffSame ("  " <> PP.pretty t)
  LineRemoved t -> PP.annotate DiffRemoved ("- " <> PP.pretty t)
  LineAdded t -> PP.annotate DiffAdded ("+ " <> PP.pretty t)

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
  FailureArrows -> PP.Terminal.color PP.Terminal.Red
  FailureGutter -> mempty
  FailureMessage -> mempty
