-- | The source-rendering logic in this module is derived from
-- @Hedgehog.Internal.Report@ and @Hedgehog.Internal.Source@ in the @hedgehog@
-- package.
--
-- Copyright 2017-2018, Jacob Stanley. All Rights Reserved.
-- Licensed under the BSD-3-Clause license; see @licenses/hedgehog.LICENSE@
-- for the full license text.
--
-- = Module description
--
-- Source-aware failure rendering: looks up the enclosing top-level
-- declaration in 'Declarations', annotates drawn values and the
-- failure message inline, and produces a pretty-printed, line-numbered
-- listing.  All functions here are pure; the caller loads the cache (see
-- 'Hegel.Report.Discovery.loadDeclarations') and threads it through.
module Hegel.Report.Source
  ( -- * Core types
    Line (..),
    Declaration (..),
    Annotation,
    Context (..),

    -- * Building annotated declarations
    lookupDeclarationSpan,
    ppFailedInput,
    ppInlinedValue,
    ppFailureLocation,

    -- * Rendering
    ppDeclaration,

    -- * Utilities
    mergeDeclarations,
    mergeFileDeclarations,
    applyContext,
    defaultContext,
  )
where

import Data.Bifunctor (first, second)
import Data.Char qualified as Char
import Data.Function (on)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Diff (Diff)
import Hegel.Report.Ann (Ann (..), Style (..), diffDocs)
import Hegel.Report.Discovery (Declarations, Pos (..), Position (..), lookupDeclaration)
import Hegel.Report.Span (ColumnNo (..), LineNo (..), Span (..))
import Prettyprinter (Doc, (<+>))
import Prettyprinter qualified as PP

-- * Core types

-- | One source line, parameterised over an annotation type @a@.
data Line a = Line
  { lineAnnotation :: !a,
    lineNumber :: !LineNo,
    lineSource :: !String
  }
  deriving stock (Eq, Ord, Show, Functor)

-- | A top-level declaration: its file, starting line, name, and source map.
data Declaration a = Declaration
  { declarationFile :: !FilePath,
    declarationLine :: !LineNo,
    declarationName :: !String,
    declarationSource :: !(Map LineNo (Line a))
  }
  deriving stock (Eq, Ord, Show, Functor)

-- | How many context lines to keep around interesting (annotated) source lines.
data Context = FullContext | Context Int
  deriving stock (Eq, Show)

-- | The annotation type for a fully-built source listing.
-- Each line carries a 'Style' for the source text and a list of extra docs
-- (inline values, arrows, diffs) to emit after the source line.
type Annotation = (Style, [(Style, Doc Ann)])

-- | Default context: keep 2 boring lines around each interesting one.
defaultContext :: Context
defaultContext = Context 2

-- * Building declarations from source

-- | Look up the top-level declaration that contains the given 'Span',
-- returning @'Nothing'@ when the source file is missing from the cache or no
-- enclosing declaration exists.
lookupDeclarationSpan :: Declarations -> Span -> Maybe (Declaration ())
lookupDeclarationSpan cache sloc = do
  (name, Pos (Position lineNo _) src) <-
    lookupDeclaration cache sloc.spanFile sloc.spanEndLine
  Just
    Declaration
      { declarationFile = sloc.spanFile,
        declarationLine = lineNo,
        declarationName = name,
        declarationSource =
          Map.fromList
            [ (n, Line {lineAnnotation = (), lineNumber = n, lineSource = srcLine})
            | (n, srcLine) <- zip [lineNo ..] (lines src)
            ]
      }

-- | Seed every line in a 'Declaration' with the default annotation.
defaultStyle :: Declaration a -> Declaration Annotation
defaultStyle = fmap (const (StyleDefault, []))

-- * Span helpers

-- | The half-open span @[start, end)@ of non-whitespace characters on a
-- source line, both 0-based; @start == end@ for blank lines.
lineSpan :: Line a -> (ColumnNo, ColumnNo)
lineSpan line =
  let (pre, rest) = span Char.isSpace line.lineSource
      (_, trimmed) = span Char.isSpace (reverse rest)
      start = length pre
      end = start + length trimmed
   in (fromIntegral start, fromIntegral end)

-- | Slice the source map to just the lines covered by a 'Span'.
takeLines :: Span -> Declaration a -> Map LineNo (Line a)
takeLines sloc decl =
  let (_, afterStart) = Map.split (sloc.spanStartLine - 1) decl.declarationSource
      (beforeEnd, _) = Map.split (sloc.spanEndLine + 1) afterStart
   in beforeEnd

-- | Column range of the last source line touched by a 'Span', or @'Nothing'@
-- if the span covers no lines in the declaration.
lastLineSpan :: Span -> Declaration a -> Maybe (ColumnNo, ColumnNo)
lastLineSpan sloc decl =
  case reverse (Map.elems (takeLines sloc decl)) of
    [] -> Nothing
    lastLine : _ -> Just (lineSpan lastLine)

-- * ppFailedInput

-- | The @Draw N:@ fallback, used when no source is available.
ppFallbackInput :: Int -> Text -> Doc Ann
ppFallbackInput ix val =
  PP.vsep
    [ PP.pretty ("Draw " <> show ix <> ":"),
      PP.indent 2 . PP.vsep . fmap (PP.annotate AnnotationValue . PP.pretty) $
        lines (T.unpack val)
    ]

-- | Splice gutter-prefixed value lines after a 'Span'\'s last source line,
-- styling the covered lines 'StyleAnnotation'. Callers supply pre-annotated
-- line docs, so a caller may prefix its own label (e.g. a step number).
-- Returns @'Nothing'@ if no source is available.
ppInlinedValue ::
  Declarations ->
  [Doc Ann] ->
  Span ->
  Maybe (Declaration Annotation)
ppInlinedValue cache valLines sloc = do
  (decl, (startCol, _)) <- lookupStyledDeclaration cache sloc
  let ppValLine d =
        PP.indent startCol (PP.annotate AnnotationGutter "│ " <> d)
      valDocs = fmap ((StyleAnnotation,) . ppValLine) valLines
  pure (spliceDocs StyleAnnotation valDocs sloc decl)

-- | Try to produce a source-inlined declaration for one drawn/annotated note.
-- Returns @'Left'@ with the @Draw N:@ fallback when no source is available.
ppFailedInput ::
  Declarations ->
  Int ->
  (Maybe Span, Text) ->
  Either (Doc Ann) (Declaration Annotation)
ppFailedInput cache ix (mspan, val) =
  maybe (Left (ppFallbackInput ix val)) Right do
    sloc <- mspan
    let valLines =
          fmap
            (PP.annotate AnnotationValue . PP.pretty)
            (lines (T.unpack val))
    ppInlinedValue cache valLines sloc

-- * ppFailureLocation

-- | Inline the failure message, diff, and @^^^@ arrows at the source line of
-- the failing assertion.
--
-- Returns @'Nothing'@ if no source is available.
ppFailureLocation ::
  Declarations ->
  [Doc Ann] ->
  Maybe Diff ->
  Span ->
  Maybe (Declaration Annotation)
ppFailureLocation cache msgs mdiff sloc = do
  (decl, (startCol, endCol)) <- lookupStyledDeclaration cache sloc
  let arrowDoc =
        PP.indent startCol $
          PP.annotate FailureMark (PP.pretty (replicate (endCol - startCol) '^'))
      inline x = PP.indent startCol (PP.annotate FailureGutter "│ " <> x)
      msgDocs = fmap (inline . PP.annotate FailureMessage) msgs
      diffLines = foldMap (fmap inline . diffDocs) mdiff
      -- A grep-able @at file:line@, last — the listing's gutter carries the
      -- same information, but not in the copyable form.
      locLine =
        inline . PP.annotate LocAnn $
          "at" <+> PP.pretty sloc.spanFile <> ":" <> PP.pretty sloc.spanStartLine.unLineNo
      docs = fmap (StyleFailure,) (arrowDoc : msgDocs <> diffLines <> [locLine])
  pure (spliceDocs StyleFailure docs sloc decl)

-- * Shared lookup\/splice machinery

-- | Look up the declaration enclosing a 'Span', seed it with the default
-- annotation, and return it along with the non-whitespace column range of the
-- span's last source line.
lookupStyledDeclaration :: Declarations -> Span -> Maybe (Declaration Annotation, (Int, Int))
lookupStyledDeclaration cache sloc = do
  decl <- defaultStyle <$> lookupDeclarationSpan cache sloc
  (startCol, endCol) <- lastLineSpan sloc decl
  pure (decl, (fromIntegral startCol, fromIntegral endCol))

-- | Apply a style to every source line a 'Span' covers and attach the given
-- inline docs after the span's last line.
spliceDocs ::
  Style ->
  [(Style, Doc Ann)] ->
  Span ->
  Declaration Annotation ->
  Declaration Annotation
spliceDocs style docs sloc = mapSource (styleLines . insertDocs)
  where
    styleLines kvs =
      foldr
        (Map.adjust (fmap (first (const style))))
        kvs
        [sloc.spanStartLine .. sloc.spanEndLine]
    insertDocs = Map.adjust (fmap (second (const docs))) sloc.spanEndLine

-- * ppDeclaration

-- | Render a fully-annotated 'Declaration' as a source listing with a
-- @┏━━ file ━━━@ header, line-number gutter, @┃@ borders, inline value\/arrow
-- docs, and @⋮@ elision marks for trimmed context.
ppDeclaration :: Declaration Annotation -> Doc Ann
ppDeclaration decl
  | Map.null decl.declarationSource = mempty
  | otherwise = PP.vcat (ppLocation : ppLines)
  where
    LineNo lastLineNo = fst (Map.findMax decl.declarationSource)
    digits = length (show lastLineNo)

    ppLineNo :: LineNo -> Doc Ann
    ppLineNo (LineNo n) = PP.pretty (T.justifyRight digits ' ' (T.pack (show n)))

    ppEmptyNo :: Doc Ann
    ppEmptyNo = PP.pretty (T.replicate digits " ")

    ppLocation :: Doc Ann
    ppLocation =
      PP.indent (digits + 1) $
        PP.annotate (StyledBorder StyleDefault) "┏━━"
          <+> PP.annotate DeclLocation (PP.pretty decl.declarationFile)
          <+> PP.annotate (StyledBorder StyleDefault) "━━━"

    ppSourceLine :: Style -> LineNo -> String -> Doc Ann
    ppSourceLine style n src =
      (if isOmittedLine (pred n) then addEllipsis else id) $
        PP.annotate (StyledLineNo style) (ppLineNo n)
          <+> PP.annotate (StyledBorder style) "┃"
          <+> PP.annotate (StyledSource style) (PP.pretty src)

    addEllipsis :: Doc Ann -> Doc Ann
    addEllipsis doc = PP.pretty ("⋮" :: String) <> PP.hardline <> doc

    -- A line is omitted when it belongs to the declaration (at or after its
    -- true starting line) but is absent from the source map. Elision is only
    -- marked __between__ rendered lines: a leading gap (e.g. a trimmed type
    -- signature) is already implied by the first line number, so no @⋮@ is
    -- emitted above it.
    isOmittedLine n =
      n >= decl.declarationLine
        && Map.notMember n decl.declarationSource
        && maybe False ((< n) . fst) (Map.lookupMin decl.declarationSource)

    ppAnnot :: (Style, Doc Ann) -> Doc Ann
    ppAnnot (style, doc) =
      PP.annotate (StyledLineNo style) ppEmptyNo
        <+> PP.annotate (StyledBorder style) "┃"
        <+> doc

    ppLines :: [Doc Ann]
    ppLines = do
      line <- Map.elems decl.declarationSource
      let (style, xs) = line.lineAnnotation
      ppSourceLine style line.lineNumber line.lineSource : fmap ppAnnot xs

-- * Merge

-- | Combine two declarations that cover the same file and starting line,
-- merging their source annotations.
mergeDeclaration :: Declaration Annotation -> Declaration Annotation -> Declaration Annotation
mergeDeclaration d1 d2 =
  d1 {declarationSource = Map.unionWith mergeLine d1.declarationSource d2.declarationSource}

mergeLine :: Line Annotation -> Line Annotation -> Line Annotation
mergeLine l1 l2 =
  l1 {lineAnnotation = l1.lineAnnotation <> l2.lineAnnotation}

-- | Merge a list of declarations, combining those that share the same file and
-- starting line. Docs attached to the same line stack in first-seen order
-- (@fromListWith@ applies its function as @f new old@, hence the 'flip').
mergeDeclarations :: [Declaration Annotation] -> [Declaration Annotation]
mergeDeclarations =
  Map.elems
    . Map.fromListWith (flip mergeDeclaration)
    . fmap (\d -> ((d.declarationFile, d.declarationLine), d))

-- | Union declarations from the same file into a single listing, so one
-- @┏━━ file ━━━@ header covers them all; the line-number gap between two
-- declarations renders as the existing @⋮@ elision. Expects its input sorted
-- by file then start line, as 'mergeDeclarations' produces.
mergeFileDeclarations :: [Declaration Annotation] -> [Declaration Annotation]
mergeFileDeclarations = fmap (foldr1 union') . List.groupBy ((==) `on` (.declarationFile))
  where
    -- Keeps the leftmost (earliest) declaration's start line and name; the
    -- line maps are disjoint, so the per-line merge never actually fires.
    union' d1 d2 =
      d1 {declarationSource = Map.unionWith mergeLine d1.declarationSource d2.declarationSource}

-- * Context limiting

-- | Apply context trimming: keep at most @n@ boring (unannotated) lines around
-- each interesting (annotated or failure) line, inserting @⋮@ elision via
-- 'ppDeclaration' when lines are dropped.
applyContext :: Context -> Declaration Annotation -> Declaration Annotation
applyContext FullContext decl = decl
applyContext (Context n) decl =
  decl {declarationSource = limitContextTo n decl.declarationSource}

limitContextTo :: Int -> Map LineNo (Line Annotation) -> Map LineNo (Line Annotation)
limitContextTo ctx src =
  Map.fromList (skipBoring (Map.toList src))
  where
    isBoring :: (LineNo, Line Annotation) -> Bool
    isBoring (_, line) = isBoringAnnotation line.lineAnnotation

    isBoringAnnotation :: Annotation -> Bool
    isBoringAnnotation = \case
      (StyleDefault, []) -> True
      _ -> False

    takeEnd :: Int -> [a] -> [a]
    takeEnd n = reverse . take n . reverse

    skipBoring :: [(LineNo, Line Annotation)] -> [(LineNo, Line Annotation)]
    skipBoring xs = case span isBoring xs of
      (boring, []) -> take ctx boring
      (boring, rest) -> takeEnd ctx boring <> keepInteresting rest

    keepInteresting :: [(LineNo, Line Annotation)] -> [(LineNo, Line Annotation)]
    keepInteresting xs = case break isBoring xs of
      (interesting, rest) -> interesting <> take ctx rest <> skipBoring rest

-- * Internal utility

mapSource ::
  (Map LineNo (Line a) -> Map LineNo (Line a)) ->
  Declaration a ->
  Declaration a
mapSource f decl = decl {declarationSource = f decl.declarationSource}
