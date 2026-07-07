-- | The declaration scanner in this module is derived from
-- @Hedgehog.Internal.Discovery@ in the @hedgehog@ package.
--
-- Copyright 2017-2018, Jacob Stanley. All Rights Reserved.
-- Licensed under the BSD-3-Clause license; see @licenses/hedgehog.LICENSE@
-- for the full license text.
--
-- = Module description
--
-- Comment-aware Haskell source scanner that locates the top-level declaration
-- enclosing a given line number, used to splice annotated values into the
-- failure report.
module Hegel.Report.Discovery
  ( -- * Declaration cache
    Declarations,
    loadDeclarations,
    lookupDeclaration,

    -- * Lower-level building blocks (exposed for testing)
    findDeclarations,
    Pos (..),
    Position (..),
  )
where

import Control.Exception (IOException)
import Control.Exception qualified as Ex
import Data.Char qualified as Char
import Data.Containers.ListUtils (nubOrd)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Hegel.Report.Span (ColumnNo (..), LineNo (..))
import System.IO qualified as IO

-- * Positioned characters

-- | A source position: 1-based line and column numbers.
data Position = Position
  { posLine :: !LineNo,
    posColumn :: !ColumnNo
  }
  deriving stock (Eq, Ord, Show)

-- | A value tagged with a source position.
data Pos a = Pos
  { posPosition :: !Position,
    posValue :: a
  }
  deriving stock (Eq, Ord, Show, Functor)

instance (Semigroup a) => Semigroup (Pos a) where
  Pos p x <> Pos q y =
    if p < q
      then Pos p (x <> y)
      else Pos q (y <> x)

-- | Tag each character in a string with its 1-based line and column position.
positioned :: String -> [Pos Char]
positioned = loop 1 1
  where
    loop :: LineNo -> ColumnNo -> String -> [Pos Char]
    loop ln col = \case
      [] -> []
      '\n' : rest ->
        Pos Position {posLine = ln, posColumn = col} '\n'
          : loop (ln + 1) 1 rest
      ch : rest ->
        Pos Position {posLine = ln, posColumn = col} ch
          : loop ln (col + 1) rest

-- * Comment classification

data Class = NotComment | Comment
  deriving stock (Eq, Ord, Show)

data Classified a = Classified
  { classifiedClass :: !Class,
    classifiedValue :: !a
  }
  deriving stock (Eq, Ord, Show)

-- | Classify each positioned character as code or comment, tracking nested
-- block comments (@{- -}@) and line comments (@--@).
classified :: [Pos Char] -> [Classified (Pos Char)]
classified = loop 0 False
  where
    asCode :: Pos Char -> Classified (Pos Char)
    asCode = Classified NotComment
    asComment :: Pos Char -> Classified (Pos Char)
    asComment = Classified Comment

    loop :: Int -> Bool -> [Pos Char] -> [Classified (Pos Char)]
    loop nesting inLine = \case
      [] -> []
      -- End a line comment on newline.
      p@(Pos _ '\n') : rest
        | inLine ->
            asCode p : loop nesting False rest
      -- Inside a line comment: treat everything as comment.
      p : rest
        | inLine ->
            asComment p : loop nesting inLine rest
      -- Open a block comment.
      p@(Pos _ '{') : q@(Pos _ '-') : rest ->
        asComment p : asComment q : loop (nesting + 1) inLine rest
      -- Close a block comment.
      p@(Pos _ '-') : q@(Pos _ '}') : rest
        | nesting > 0 ->
            asComment p : asComment q : loop (nesting - 1) inLine rest
      -- Inside a block comment.
      p : rest
        | nesting > 0 ->
            asComment p : loop nesting inLine rest
      -- Start a line comment (two dashes not followed by a symbol char).
      p@(Pos _ '-') : q@(Pos _ '-') : r@(Pos _ rc) : rest
        | not (Char.isSymbol rc) ->
            asComment p : asComment q : loop nesting True (r : rest)
      -- Ordinary code character.
      p : rest ->
        asCode p : loop nesting inLine rest

-- * Declaration identification

-- | True for a classified character that opens a top-level declaration:
-- non-comment, at column 1, starting with a lowercase letter or @_@.
isDeclaration :: Classified (Pos Char) -> Bool
isDeclaration (Classified cls (Pos pos x)) =
  cls == NotComment
    && pos.posColumn == 1
    && (Char.isLower x || x == '_')

-- | True for a classified character that is whitespace or a comment.
isWhitespace :: Classified (Pos Char) -> Bool
isWhitespace (Classified cls (Pos _ x)) =
  cls == Comment || Char.isSpace x

-- | Strip trailing whitespace and comments from a classified list, keeping
-- any same-line trailing content and at most one trailing newline so
-- declaration boundaries are clean.
trimEnd :: [Classified (Pos Char)] -> [Classified (Pos Char)]
trimEnd xs =
  let (trailing, revCode) = span isWhitespace (reverse xs)
      (sameLine, nextLines) = break isNewline (reverse trailing)
   in reverse revCode ++ sameLine ++ take 1 nextLines
  where
    isNewline :: Classified (Pos Char) -> Bool
    isNewline (Classified _ (Pos _ ch)) = ch == '\n'

-- | Reconstruct a @'Pos' 'String'@ from the first character of a declaration
-- and the remaining classified characters in its body.
reconstitute :: Classified (Pos Char) -> [Classified (Pos Char)] -> Pos String
reconstitute (Classified _ (Pos pos x)) rest =
  Pos pos (x : fmap (\cl -> cl.classifiedValue.posValue) rest)

-- | Extract the first identifier token from a declaration (the binding name).
declName :: String -> String
declName src = case words src of
  [] -> ""
  w : _ -> w

-- | Partition a classified character stream into top-level declarations.
declarations :: [Classified (Pos Char)] -> Map String (Pos String)
declarations =
  Map.fromListWith (<>)
    . loop
    . dropWhile (not . isDeclaration)
  where
    loop :: [Classified (Pos Char)] -> [(String, Pos String)]
    loop = \case
      [] -> []
      first : rest ->
        let (body, remainder) = break isDeclaration rest
            pos@(Pos _ src) = reconstitute first (trimEnd body)
         in (declName src, pos) : loop remainder

-- | Find all top-level declarations in a Haskell source string, keyed by
-- binding name.
findDeclarations :: String -> Map String (Pos String)
findDeclarations = declarations . classified . positioned

-- * Declaration cache

-- | Scanned source files, one entry per readable file: that file's top-level
-- declarations keyed by their starting line.
type Declarations = Map FilePath (Map LineNo (String, Pos String))

-- | Read and scan every distinct file once.  Files that can't be read are
-- absent from the cache, so later lookups against them degrade to
-- @'Nothing'@.
loadDeclarations :: [FilePath] -> IO Declarations
loadDeclarations paths = do
  entries <- traverse load (nubOrd paths)
  pure (Map.fromList (catMaybes entries))
  where
    load :: FilePath -> IO (Maybe (FilePath, Map LineNo (String, Pos String)))
    load path = do
      mcontents <- readFileSafe path
      pure case mcontents of
        Nothing -> Nothing
        Just contents -> Just (path, declarationsByLine contents)

-- | Find the top-level declaration containing @line@ in @path@.  Returns
-- @'Nothing'@ when the file wasn't readable at cache-load time or no
-- enclosing declaration exists.
lookupDeclaration :: Declarations -> FilePath -> LineNo -> Maybe (String, Pos String)
lookupDeclaration cache path line = do
  byLine <- Map.lookup path cache
  -- The enclosing declaration is the one with the greatest start line that
  -- is still at or before the target line.
  snd <$> Map.lookupLE line byLine

-- | Scan a file's contents and key its declarations by starting line.
declarationsByLine :: String -> Map LineNo (String, Pos String)
declarationsByLine contents =
  Map.fromList
    [ (p.posPosition.posLine, (name, p))
    | (name, p) <- Map.toList (findDeclarations contents)
    ]

-- | Read a file's contents strictly, returning @'Nothing'@ on any
-- 'IOException'.
readFileSafe :: FilePath -> IO (Maybe String)
readFileSafe path =
  Ex.handle (\(_ :: IOException) -> pure Nothing) (Just <$> IO.readFile' path)
