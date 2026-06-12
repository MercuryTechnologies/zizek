-- | Tests for the source-aware failure renderer: declaration discovery,
-- annotation merging, context limiting, and pretty-printing structure.
module SourceRendering (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Hegel.Report.Ann (Ann, Style (..))
import Hegel.Report.Discovery (Pos (..), Position (..), findDeclarations)
import Hegel.Report.Source
  ( Context (..),
    Declaration (..),
    Line (..),
    applyContext,
    mergeDeclarations,
    ppDeclaration,
  )
import Hegel.Report.Span (LineNo (..))
import Prettyprinter (Doc)
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PP.Text
import Test.Hspec

-- * Helpers

render :: Doc Ann -> String
render = T.unpack . PP.Text.renderStrict . PP.layoutPretty PP.defaultLayoutOptions

-- | The concrete annotation type used by 'ppDeclaration': a style for the
-- source line plus a list of (style, doc) pairs to emit below it.
type Annot = (Style, [(Style, Doc Ann)])

-- | Build a 'Declaration' where all lines have the default annotation.
mkDecl :: FilePath -> LineNo -> String -> [(LineNo, String)] -> Declaration Annot
mkDecl file startLine name srcLines =
  Declaration
    { declarationFile = file,
      declarationLine = startLine,
      declarationName = name,
      declarationSource =
        Map.fromList
          [ (n, Line {lineAnnotation = (StyleDefault, []), lineNumber = n, lineSource = src})
          | (n, src) <- srcLines
          ]
    }

-- | Build an all-default-annotated source map for a range of line numbers.
defaultLines :: [LineNo] -> Map.Map LineNo (Line Annot)
defaultLines ns =
  Map.fromList
    [ (n, Line {lineAnnotation = (StyleDefault, []), lineNumber = n, lineSource = "src"})
    | n <- ns
    ]

-- | Mark one line as interesting (has an inline doc) so context limiting
-- keeps it.
markInteresting :: LineNo -> Declaration Annot -> Declaration Annot
markInteresting n decl =
  decl
    { declarationSource =
        Map.adjust
          (\l -> l {lineAnnotation = (StyleAnnotation, [(StyleAnnotation, PP.pretty ("val" :: String))])})
          n
          decl.declarationSource
    }

spec :: Spec
spec = do
  describe "Discovery.findDeclarations" $ do
    it "finds a single top-level binding" $ do
      let src = "foo = 1\n"
          decls = findDeclarations src
      Map.member "foo" decls `shouldBe` True

    it "finds multiple top-level bindings" $ do
      let src = "foo = 1\n\nbar = 2\n"
          decls = findDeclarations src
      Map.member "foo" decls `shouldBe` True
      Map.member "bar" decls `shouldBe` True

    it "does not mistake indented lines for declarations" $ do
      let src = "foo = do\n  bar\n  baz\n\nqux = 4\n"
          decls = findDeclarations src
      Map.member "foo" decls `shouldBe` True
      Map.member "qux" decls `shouldBe` True
      Map.size decls `shouldBe` 2

    it "ignores line comments at column 1" $ do
      let src = "-- this is a comment\nfoo = 1\n"
          decls = findDeclarations src
      Map.member "foo" decls `shouldBe` True
      Map.member "--" decls `shouldBe` False

    it "ignores block-comment content at column 1" $ do
      let src = "{- block\ncomment -}\nfoo = 1\n"
          decls = findDeclarations src
      Map.member "foo" decls `shouldBe` True
      Map.size decls `shouldBe` 1

    it "handles nested block comments" $ do
      let src = "{- outer {- inner -} still comment -}\nfoo = 1\n"
          decls = findDeclarations src
      Map.member "foo" decls `shouldBe` True
      Map.size decls `shouldBe` 1

    it "records the correct start line" $ do
      let src = "foo = 1\n\nbar = 2\n"
          decls = findDeclarations src
      case Map.lookup "bar" decls of
        Nothing -> expectationFailure "bar not found"
        Just (Pos (Position lineNo _) _) -> lineNo `shouldBe` LineNo 3

    it "does not find uppercase-starting identifiers as declarations" $ do
      let src = "Foo = 1\nfoo = 2\n" :: String
          decls = findDeclarations src
      Map.member "foo" decls `shouldBe` True
      Map.member "Foo" decls `shouldBe` False

  describe "applyContext" $ do
    it "FullContext keeps all lines" $ do
      let decl =
            Declaration
              { declarationFile = "f.hs",
                declarationLine = 1,
                declarationName = "foo",
                declarationSource = defaultLines [1 .. 10]
              }
      Map.size (applyContext FullContext decl).declarationSource `shouldBe` 10

    it "Context n keeps n boring lines around each interesting line" $ do
      let decl0 =
            Declaration
              { declarationFile = "f.hs",
                declarationLine = 1,
                declarationName = "foo",
                declarationSource = defaultLines [1 .. 10]
              }
          decl = markInteresting 5 decl0
          trimmed = applyContext (Context 2) decl
          kept = Map.keys trimmed.declarationSource
      5 `elem` kept `shouldBe` True
      3 `elem` kept `shouldBe` True
      7 `elem` kept `shouldBe` True
      1 `elem` kept `shouldBe` False
      10 `elem` kept `shouldBe` False

  describe "mergeDeclarations" $ do
    it "keeps distinct declarations separate" $ do
      let d1 = mkDecl "f.hs" 1 "foo" [(1, "foo = 1")]
          d2 = mkDecl "f.hs" 5 "bar" [(5, "bar = 2")]
      length (mergeDeclarations [d1, d2]) `shouldBe` 2

    it "merges declarations with the same file and start line" $ do
      let d1 = mkDecl "f.hs" 1 "foo" [(1, "foo = 1"), (2, "  helper")]
          d2 = mkDecl "f.hs" 1 "foo" [(1, "foo = 1"), (3, "  other")]
          merged = mergeDeclarations [d1, d2]
      length merged `shouldBe` 1
      case merged of
        [m] -> Map.size m.declarationSource `shouldBe` 3
        _ -> expectationFailure "expected exactly one merged declaration"

  describe "ppDeclaration" $ do
    it "includes the ┏━━ header line" $ do
      let decl = mkDecl "Example.hs" 1 "foo" [(1, "foo = 42")]
          output = render (ppDeclaration decl)
      "┏━━" `T.isInfixOf` T.pack output `shouldBe` True
      "Example.hs" `T.isInfixOf` T.pack output `shouldBe` True

    it "includes the ┃ gutter for source lines" $ do
      let decl = mkDecl "Example.hs" 1 "foo" [(1, "foo = 42")]
          output = render (ppDeclaration decl)
      "┃" `T.isInfixOf` T.pack output `shouldBe` True

    it "includes the source text" $ do
      let decl = mkDecl "Example.hs" 1 "foo" [(1, "foo = 42")]
          output = render (ppDeclaration decl)
      "foo = 42" `T.isInfixOf` T.pack output `shouldBe` True

    it "aligns ┏ and ┃ in the same column" $ do
      -- With a single-digit line number, the ┃ should sit at the same byte
      -- offset as ┏ in the header row.
      let decl = mkDecl "F.hs" 1 "foo" [(1, "foo = 1")]
          ls = T.lines (T.pack (render (ppDeclaration decl)))
          borderIdx t sym = T.length (fst (T.breakOn sym t))
      case ls of
        headerLine : srcLine : _ -> do
          let hIdx = borderIdx headerLine "┏"
              sIdx = borderIdx srcLine "┃"
          hIdx `shouldBe` sIdx
        _ -> expectationFailure "expected at least two lines in ppDeclaration output"

    it "emits ⋮ for omitted lines when context is limited" $ do
      let mkLine n@(LineNo i) = (n, "line " <> show i)
          decl0 = mkDecl "F.hs" 1 "foo" (map mkLine [1 .. (10 :: LineNo)])
          -- Mark line 8 as interesting so lines 1-5 get dropped.
          decl = markInteresting 8 decl0
          output = render (ppDeclaration (applyContext (Context 2) decl))
      "⋮" `T.isInfixOf` T.pack output `shouldBe` True
