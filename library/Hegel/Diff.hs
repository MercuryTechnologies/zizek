-- | The structural-diff logic in this module is derived from
-- @Hedgehog.Internal.Show@ in the @hedgehog@ package.
--
-- Copyright 2017-2018, Jacob Stanley. All Rights Reserved.
-- Licensed under the BSD-3-Clause license; see @licenses/hedgehog.LICENSE@
-- for the full license text.
--
-- = Module description
--
-- Structural diff of two 'Show'-rendered values.
--
-- Ported from @Hedgehog.Internal.Show@: parse both sides into
-- 'Text.Show.Pretty.Value' ASTs, recurse structurally so a single changed
-- field in a record shows up as one @-@\/@+@ pair surrounded by unchanged
-- context, and fall back to a line-level diff when either side fails to parse.
module Hegel.Diff
  ( -- * Result type
    LineDiff (..),
    Diff,

    -- * Structural diff (parse-then-diff)
    diffValues,
    diffShown,

    -- * Line-level diff (always succeeds)
    diffLines,

    -- * Rendering
    renderDiff,
  )
where

import Data.Bifunctor (second)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Show.Pretty (Value (..), parseValue, valToStr)

-- | One line in a diff.
data LineDiff
  = -- | A line that appears in both sides (context).
    LineSame Text
  | -- | A line present only in the left (old) value.
    LineRemoved Text
  | -- | A line present only in the right (new) value.
    LineAdded Text
  deriving stock (Eq, Show)

-- | A diff: an ordered sequence of 'LineDiff' entries.
type Diff = [LineDiff]

-- | Render a 'Diff' as plain text with @  @\/@- @\/@+ @ line prefixes.
renderDiff :: Diff -> Text
renderDiff = T.intercalate "\n" . fmap renderLine
  where
    renderLine :: LineDiff -> Text
    renderLine (LineSame t) = "  " <> t
    renderLine (LineRemoved t) = "- " <> t
    renderLine (LineAdded t) = "+ " <> t

-- | Diff two parsed 'Value's structurally.
diffValues :: Value -> Value -> Diff
diffValues x y = toLineDiff (valueDiff x y)

-- | Attempt to diff two @show@-rendered strings structurally.
--
-- Returns 'Nothing' when either string fails to parse as a value AST; the
-- caller should fall back to 'diffLines'.
diffShown :: Text -> Text -> Maybe Diff
diffShown lhs rhs = do
  x <- parseValue (T.unpack lhs)
  y <- parseValue (T.unpack rhs)
  pure (diffValues x y)

-- | Line-level diff of two texts.
--
-- Common leading\/trailing lines are rendered as context (@  @); the differing
-- middle gets @- @\/@+ @ markers.
--
-- Used as the fallback if 'diffShown' returns 'Nothing'.
diffLines :: Text -> Text -> Diff
diffLines lhs rhs =
  fmap LineSame prefix
    <> fmap LineRemoved lhsMid
    <> fmap LineAdded rhsMid
    <> fmap LineSame suffix
  where
    ls :: [Text]
    ls = T.lines lhs
    rs :: [Text]
    rs = T.lines rhs
    prefix :: [Text]
    prefix = commonPrefix ls rs
    ls' :: [Text]
    ls' = drop (length prefix) ls
    rs' :: [Text]
    rs' = drop (length prefix) rs
    suffix :: [Text]
    suffix = reverse (commonPrefix (reverse ls') (reverse rs'))
    lhsMid :: [Text]
    lhsMid = take (length ls' - length suffix) ls'
    rhsMid :: [Text]
    rhsMid = take (length rs' - length suffix) rs'
    commonPrefix :: [Text] -> [Text] -> [Text]
    commonPrefix (a : as) (b : bs) | a == b = a : commonPrefix as bs
    commonPrefix _ _ = []

-- * Internal structural diff tree

data ValueDiff
  = ValueCon String [ValueDiff]
  | ValueRec String [(String, ValueDiff)]
  | ValueTuple [ValueDiff]
  | ValueList [ValueDiff]
  | ValueSame Value
  | ValueDiff Value Value

valueDiff :: Value -> Value -> ValueDiff
valueDiff x y
  | x == y = ValueSame x
  | otherwise =
      case (x, y) of
        (Con nx xs, Con ny ys)
          | nx == ny,
            length xs == length ys ->
              ValueCon nx (zipWith valueDiff xs ys)
        (Rec nx nxs, Rec ny nys)
          | nx == ny,
            fmap fst nxs == fmap fst nys ->
              let ns :: [String]
                  ns = fmap fst nxs
                  xs :: [Value]
                  xs = fmap snd nxs
                  ys :: [Value]
                  ys = fmap snd nys
               in ValueRec nx (zip ns (zipWith valueDiff xs ys))
        (Tuple xs, Tuple ys)
          | length xs == length ys ->
              ValueTuple (zipWith valueDiff xs ys)
        (List xs, List ys)
          | length xs == length ys ->
              ValueList (zipWith valueDiff xs ys)
        _ ->
          ValueDiff x y

takeLeft :: ValueDiff -> Value
takeLeft = \case
  ValueCon n xs -> Con n (fmap takeLeft xs)
  ValueRec n nxs -> Rec n (fmap (second takeLeft) nxs)
  ValueTuple xs -> Tuple (fmap takeLeft xs)
  ValueList xs -> List (fmap takeLeft xs)
  ValueSame x -> x
  ValueDiff x _ -> x

takeRight :: ValueDiff -> Value
takeRight = \case
  ValueCon n xs -> Con n (fmap takeRight xs)
  ValueRec n nxs -> Rec n (fmap (second takeRight) nxs)
  ValueTuple xs -> Tuple (fmap takeRight xs)
  ValueList xs -> List (fmap takeRight xs)
  ValueSame x -> x
  ValueDiff _ y -> y

-- * DocDiff → LineDiff linearisation (ported from Hedgehog.Internal.Show)

data DocDiff
  = DocSame Int String
  | DocRemoved Int String
  | DocAdded Int String
  | DocOpen Int String
  | DocItem Int String [DocDiff]
  | DocClose Int String

toLineDiff :: ValueDiff -> Diff
toLineDiff =
  concatMap (mkLineDiff 0 "")
    . collapseOpen
    . dropLeadingSep
    . mkDocDiff 0

mkDocDiff :: Int -> ValueDiff -> [DocDiff]
mkDocDiff indent vd = case vd of
  ValueSame x ->
    same indent (renderVal x)
  diff
    | x <- takeLeft diff,
      y <- takeRight diff,
      oneLiner x,
      oneLiner y ->
        removed indent (renderVal x) ++ added indent (renderVal y)
  ValueCon n xs ->
    same indent n ++ concatMap (mkDocDiff (indent + 2)) xs
  ValueRec n nxs ->
    same indent n
      ++ [DocOpen indent "{"]
      ++ fmap
        ( \(name, x) ->
            DocItem (indent + 2) ", " (same 0 (name ++ " =") ++ mkDocDiff 2 x)
        )
        nxs
      ++ [DocClose (indent + 2) "}"]
  ValueTuple xs ->
    DocOpen indent "("
      : fmap (DocItem indent ", " . mkDocDiff 0) xs
      ++ [DocClose indent ")"]
  ValueList xs ->
    DocOpen indent "["
      : fmap (DocItem indent ", " . mkDocDiff 0) xs
      ++ [DocClose indent "]"]
  ValueDiff x y ->
    removed indent (renderVal x) ++ added indent (renderVal y)

mkLineDiff :: Int -> String -> DocDiff -> Diff
mkLineDiff indent0 prefix0 dd =
  let mkLinePrefix :: Int -> String
      mkLinePrefix indent = spaces indent0 ++ prefix0 ++ spaces indent
      mkLineIndent :: Int -> Int
      mkLineIndent indent = indent0 + length prefix0 + indent
   in case dd of
        DocSame indent x -> [LineSame $ T.pack (mkLinePrefix indent ++ x)]
        DocRemoved indent x -> [LineRemoved $ T.pack (mkLinePrefix indent ++ x)]
        DocAdded indent x -> [LineAdded $ T.pack (mkLinePrefix indent ++ x)]
        DocOpen indent x -> [LineSame $ T.pack (mkLinePrefix indent ++ x)]
        DocItem _ _ [] -> []
        DocItem indent prefix (x@DocRemoved {} : y@DocAdded {} : xs) ->
          mkLineDiff (mkLineIndent indent) prefix x
            ++ mkLineDiff (mkLineIndent indent) prefix y
            ++ concatMap (mkLineDiff (mkLineIndent (indent + length prefix)) "") xs
        DocItem indent prefix (x : xs) ->
          mkLineDiff (mkLineIndent indent) prefix x
            ++ concatMap (mkLineDiff (mkLineIndent (indent + length prefix)) "") xs
        DocClose indent x ->
          [LineSame $ T.pack (spaces (mkLineIndent indent) ++ x)]

collapseOpen :: [DocDiff] -> [DocDiff]
collapseOpen = \case
  DocSame indent line : DocOpen _ bra : xs ->
    DocSame indent (line ++ " " ++ bra) : collapseOpen xs
  DocItem indent prefix xs : ys ->
    DocItem indent prefix (collapseOpen xs) : collapseOpen ys
  x : xs ->
    x : collapseOpen xs
  [] ->
    []

dropLeadingSep :: [DocDiff] -> [DocDiff]
dropLeadingSep = \case
  DocOpen oindent bra : DocItem indent prefix xs : ys ->
    DocOpen oindent bra
      : DocItem (indent + length prefix) "" (dropLeadingSep xs)
      : dropLeadingSep ys
  DocItem indent prefix xs : ys ->
    DocItem indent prefix (dropLeadingSep xs) : dropLeadingSep ys
  x : xs ->
    x : dropLeadingSep xs
  [] ->
    []

renderVal :: Value -> String
renderVal = valToStr

oneLiner :: Value -> Bool
oneLiner x = case lines (renderVal x) of
  _ : _ : _ -> False
  _ -> True

same :: Int -> String -> [DocDiff]
same indent = fmap (DocSame indent) . lines

removed :: Int -> String -> [DocDiff]
removed indent = fmap (DocRemoved indent) . lines

added :: Int -> String -> [DocDiff]
added indent = fmap (DocAdded indent) . lines

spaces :: Int -> String
spaces n = replicate n ' '
