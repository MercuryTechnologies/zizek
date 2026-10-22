module GrammarGenerators (spec) where

import Control.Monad (void)
import Data.Char (isAsciiLower, isDigit)
import Data.Default.Class (def)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.IO.Exception (IOErrorType (InappropriateType), IOException, ioe_type)
import GHC.Stack (SrcLoc (..), getCallStack)
import Hegel (prop)
import Hegel.Exception (Diagnostic (..), GrammarError (..), ValidationError (..))
import Hegel.Gen qualified as Gen
import Hegel.Gen.Internal qualified as Internal
import Hegel.Grammar qualified as Grammar
import Hegel.Property (assert, forEach)
import Hegel.Report (Report (..), isDrawn)
import Hegel.Runner (check)
import Hegel.Runner qualified as Runner
import Hegel.Seed (Seed (..))
import Hegel.Settings (Settings (..), defaultSettings)
import System.IO.Error (isDoesNotExistError)
import Test.Hspec
import TestSupport (failureNotes, noteKind, noteText)

fixture :: FilePath -> FilePath
fixture name = "tests/unit/grammars/" <> name

load :: FilePath -> IO Grammar.Source
load = Grammar.readFile . fixture

-- | Whether a string is balanced parentheses.
balanced :: Text -> Bool
balanced = go 0 . T.unpack
  where
    go :: Int -> String -> Bool
    go n [] = n == 0
    go n ('(' : cs) = go (n + 1) cs
    go n (')' : cs) = n > 0 && go (n - 1) cs
    go _ _ = False

-- | The deepest nesting of parentheses in a balanced string.
nesting :: Text -> Int
nesting = snd . T.foldl' step (0, 0)
  where
    step :: (Int, Int) -> Char -> (Int, Int)
    step (n, deepest) '(' = (n + 1, max deepest (n + 1))
    step (n, deepest) _ = (n - 1, deepest)

-- | Whether a string is one or more decimal integers joined by @sep@ characters.
joinedInts :: [Char] -> Text -> Bool
joinedInts seps = all (\t -> not (T.null t) && T.all isDigit t) . T.split (`elem` seps)

grammarRejection :: Text -> ValidationError -> Bool
grammarRejection needle (ValidationError d) =
  d.context == "Hegel.Gen.Grammar"
    && needle `T.isInfixOf` d.detail
    && case getCallStack d.callStack of
      (operation, loc) : _ -> operation == "build" && loc.srcLocFile == "tests/unit/GrammarGenerators.hs"
      _ -> False

grammarError :: FilePath -> Text -> GrammarError -> Bool
grammarError file needle (GrammarError d) =
  d.values == [("file", T.pack file)] && needle `T.isInfixOf` d.detail

spec :: Spec
spec = do
  describe "Gen.grammar" $ do
    it "derives only balanced parentheses" $ do
      g <- load "parens.lark"
      prop (Gen.grammar g & Gen.maxSize 64 & Gen.build) \s -> s `shouldSatisfy` balanced

    it "derives sums of integers using a bundled import" $ do
      g <- load "sum.lark"
      prop (Gen.grammar g & Gen.maxSize 64 & Gen.build) \s -> s `shouldSatisfy` joinedInts "+"

    it "derives keywords and lowercase words" $ do
      g <- load "keyword.lark"
      prop (Gen.grammar g & Gen.maxSize 64 & Gen.build) \s ->
        s `shouldSatisfy` \t -> not (T.null t) && T.all isAsciiLower t

    it "derives from the start symbol it is given" $ do
      g <- load "parens.lark"
      prop (Gen.grammar g & Gen.startSymbol "pair" & Gen.maxSize 64 & Gen.build) \s ->
        s `shouldSatisfy` \t -> balanced t && not (T.null t)

    it "keeps every string within maxSize" $ do
      g <- load "sum.lark"
      prop (Gen.grammar g & Gen.maxSize 5 & Gen.build) \s -> T.length s `shouldSatisfy` (<= 5)

    it "bounds the nesting of a derivation with maxDepth" $ do
      g <- load "parens.lark"
      -- Each parenthesis pair nests two rules, start and pair, so ten levels of
      -- rules hold at most five pairs.
      prop (Gen.grammar g & Gen.maxDepth 10 & Gen.maxSize 100 & Gen.build) \s -> nesting s `shouldSatisfy` (<= 5)

    it "draws every character from its alphabet" $ do
      g <- load "sum.lark"
      prop (Gen.grammar g & Gen.alphabet (Gen.char & Gen.minCodepoint 43 & Gen.maxCodepoint 51) & Gen.build) \s ->
        s `shouldSatisfy` T.all (\c -> c >= '+' && c <= '3')

    it "shrinks a failing string to a minimal derivation" $ do
      g <- load "arith.lark"
      report <- check def $ forEach (Gen.grammar g & Gen.build) \s -> assert (not ("*" `T.isInfixOf` s)) "contains *"
      let drawn = noteText <$> filter (isDrawn . noteKind) (failureNotes report.result)
      -- The shortest string with a '*' is one digit either side of it.
      fmap (read @Text . T.unpack) drawn `shouldSatisfy` \case
        [s] -> T.length s == 3 && T.count "*" s == 1 && joinedInts "*" s
        _ -> False

    it "produces the same strings from the same seed" $ do
      g <- load "parens.lark"
      let settings = defaultSettings {seed = Just (SeedFixed 11)}
      first <- Runner.samples settings 20 (Gen.grammar g & Gen.build)
      second <- Runner.samples settings 20 (Gen.grammar g & Gen.build)
      first `shouldBe` second

  describe "Grammar.readFile" $ do
    it "follows a relative import of a sibling file" $ do
      g <- load "sibling.lark"
      prop (Gen.grammar g & Gen.maxSize 64 & Gen.build) \s ->
        s `shouldSatisfy` \t -> not (T.null t) && T.all (`elem` ("ab" :: String)) t

    it "follows relative imports into a subdirectory and within it" $ do
      g <- load "nested.lark"
      prop (Gen.grammar g & Gen.maxSize 64 & Gen.build) \s ->
        s `shouldSatisfy` \t -> not (T.null t) && T.all (`elem` ("xy" :: String)) t

    it "throws GrammarError naming a relative import with no file" $
      load "missing-import.lark" `shouldThrow` grammarError "nowhere.lark" "nowhere.lark"

    it "throws GrammarError naming the root for a Lark syntax error" $
      load "syntax-error.lark" `shouldThrow` \(GrammarError d) -> d.context == T.pack (fixture "syntax-error.lark") && null d.values

    it "throws GrammarError naming an imported file with a Lark syntax error" $
      load "imports-broken.lark" `shouldThrow` grammarError "lib/broken.lark" "lib/broken.lark"

    it "treats an import through a plain file as a missing file" $
      load "paths/through-file.lark" `shouldThrow` grammarError "notdir/x.lark" "notdir/x.lark"

    it "rethrows the IO error for an import naming a directory" $
      load "paths/names-dir.lark" `shouldThrow` \(e :: IOException) -> ioe_type e == InappropriateType

    it "throws GrammarError naming a root file that isn't UTF-8" $
      load "latin1.lark" `shouldThrow` grammarError (fixture "latin1.lark") "UTF-8"

    it "throws GrammarError naming an imported file that isn't UTF-8" $
      load "imports-latin1.lark" `shouldThrow` grammarError "latin1.lark" "UTF-8"

    it "rethrows the IO error for a missing root file" $
      load "no-such-grammar.lark" `shouldThrow` isDoesNotExistError

  describe "grammar errors at draw" $ do
    for_
      [ ("an import cycle", "cycle_a.lark", id, "Circular grammar import"),
        ("an undefined rule", "undefined-rule.lark", id, "missing"),
        ("an undefined start symbol", "parens.lark", Gen.startSymbol "nowhere", "nowhere"),
        ("a maxSize below the shortest string", "fixed.lark", Gen.maxSize 3, "smallest string"),
        ("an alphabet a terminal can't be drawn from", "keyword.lark", Gen.alphabet (Gen.char & Gen.minCodepoint 120 & Gen.maxCodepoint 122), "cannot be produced")
      ]
      \(name, root, configure, needle) ->
        it ("raises ValidationError for " <> name) $ do
          g <- load root
          Runner.sample defaultSettings (Gen.grammar g & configure & Gen.build) `shouldThrow` grammarRejection needle

    for_
      [ ("maxSize", 0, \n -> Gen.maxSize n),
        ("maxSize", 1048577, \n -> Gen.maxSize n),
        ("maxDepth", 0, \n -> Gen.maxDepth n),
        ("maxDepth", 501, \n -> Gen.maxDepth n)
      ]
      \(field, n, configure) ->
        it ("rejects " <> T.unpack field <> " " <> show n) $ do
          g <- load "parens.lark"
          Runner.sample defaultSettings (void (Gen.grammar g & configure n & Gen.build)) `shouldThrow` \(ValidationError d) ->
            d.context == "Hegel.Gen.Grammar" && d.values == [(field, T.pack (show n))]

  describe "grammar span labels" $ do
    it "labels the same content loaded from two paths alike" $ do
      a <- load "sum.lark"
      b <- load "copy/sum.lark"
      Internal.labelOf (Gen.grammar a & Gen.build) `shouldBe` Internal.labelOf (Gen.grammar b & Gen.build)

    it "ignores size and depth caps" $ do
      g <- load "sum.lark"
      Internal.labelOf (Gen.grammar g & Gen.maxSize 10 & Gen.build) `shouldBe` Internal.labelOf (Gen.grammar g & Gen.maxDepth 10 & Gen.build)

    it "distinguishes different grammars" $ do
      a <- load "sum.lark"
      b <- load "parens.lark"
      Internal.labelOf (Gen.grammar a & Gen.build) `shouldNotBe` Internal.labelOf (Gen.grammar b & Gen.build)

    it "distinguishes start symbols" $ do
      g <- load "parens.lark"
      Internal.labelOf (Gen.grammar g & Gen.build) `shouldNotBe` Internal.labelOf (Gen.grammar g & Gen.startSymbol "pair" & Gen.build)
