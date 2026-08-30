-- | Coverage for 'Gen.recursive': respecting 'Gen.maxDepth'\/'Gen.maxLeaves',
-- the depth\/cap the branch function sees via 'Gen.RecursionContext',
-- subtree-hoisting shrinking, and the leaf-budget retry path's span
-- discipline (see Note [Span discipline on retry] in
-- "Hegel.Gen.Recursive").
module RecursiveGenerators (spec) where

import Data.Default.Class (def)
import Data.Function ((&))
import Data.Word (Word64)
import Hegel (prop)
import Hegel.Gen qualified as Gen
import Hegel.Property (check, check_, forEach)
import Hegel.Report (Report (..), Result (..))
import Hegel.Settings (Settings (..))
import Test.Hspec
import UnliftIO.IORef (modifyIORef', newIORef, readIORef, writeIORef)

-- | A tree whose branches hold a variable number of children, built through
-- 'Gen.list'.
data Tree = Leaf Int | Branch [Tree]
  deriving stock (Eq, Show)

treeLeafCount :: Tree -> Int
treeLeafCount (Leaf _) = 1
treeLeafCount (Branch cs) = sum (map treeLeafCount cs)

isTreeBranch :: Tree -> Bool
isTreeBranch Branch {} = True
isTreeBranch Leaf {} = False

trees :: Gen.RecursiveBuilder Tree
trees =
  Gen.recursive
    (Leaf <$> (Gen.int & Gen.build))
    (\_ctx subtrees -> Branch <$> (Gen.list subtrees & Gen.maxSize 3 & Gen.build))

treeHeight :: Tree -> Int
treeHeight (Leaf _) = 0
treeHeight (Branch cs) = 1 + foldr (max . treeHeight) 0 cs

-- | A tree whose branches always hold exactly two children, built through
-- 'Applicative' composition rather than a collection. Binary branches can
-- never end the recursion early the way an empty list can, so a tight
-- 'Gen.maxLeaves' budget on this shape forces frequent leaf-budget retries.
data BinTree = BLeaf | BBranch BinTree BinTree
  deriving stock (Eq, Show)

binLeafCount :: BinTree -> Int
binLeafCount BLeaf = 1
binLeafCount (BBranch l r) = binLeafCount l + binLeafCount r

binTrees :: Gen.RecursiveBuilder BinTree
binTrees = Gen.recursive (pure BLeaf) (\_ctx subtrees -> BBranch <$> subtrees <*> subtrees)

-- | A binary tree carrying an 'Int' at every leaf, for shrink tests that need
-- a witness deep in the tree to be hoisted toward the root rather than only
-- shrunk in place.
data IntTree = ILeaf Int | IBranch IntTree IntTree
  deriving stock (Eq, Show)

hasOddLeafPair :: IntTree -> Bool
hasOddLeafPair (ILeaf _) = False
hasOddLeafPair (IBranch (ILeaf a) (ILeaf b)) | odd a && odd b = True
hasOddLeafPair (IBranch l r) = hasOddLeafPair l || hasOddLeafPair r

intTrees :: Gen.RecursiveBuilder IntTree
intTrees = Gen.recursive (ILeaf <$> (Gen.int & Gen.build)) (\_ctx subtrees -> IBranch <$> subtrees <*> subtrees)

-- | A tree that records, at every branch node, the 'Gen.RecursionContext' it
-- was built with: its own depth and the generator's configured 'maxDepth'.
data DTree = DLeaf | DBranch Word64 Word64 [DTree]
  deriving stock (Eq, Show)

-- | Every branch node's recorded depth is below the cap it also recorded,
-- and that cap matches @expected@, all the way down.
depthContextIsConsistent :: Word64 -> DTree -> Bool
depthContextIsConsistent _ DLeaf = True
depthContextIsConsistent expected (DBranch d maxD cs) =
  d < maxD && maxD == expected && all (depthContextIsConsistent expected) cs

dTrees :: Gen.RecursiveBuilder DTree
dTrees =
  Gen.recursive
    (pure DLeaf)
    (\ctx subtrees -> DBranch ctx.depth ctx.maxDepth <$> (Gen.list subtrees & Gen.maxSize 3 & Gen.build))

spec :: Spec
spec = describe "Gen.recursive" $ do
  it "respects maxDepth" $ do
    prop (trees & Gen.maxDepth 3 & Gen.build) $ \t ->
      treeHeight t `shouldSatisfy` (<= 3)

  it "respects maxLeaves with list-shaped branches" $ do
    prop (trees & Gen.maxLeaves 5 & Gen.build) $ \t ->
      treeLeafCount t `shouldSatisfy` (<= 5)

  it "respects maxLeaves with binary branches" $ do
    prop (binTrees & Gen.maxLeaves 4 & Gen.build) $ \t ->
      binLeafCount t `shouldSatisfy` (<= 4)

  it "generates both leaves and branches" $ do
    seen <- newIORef ([] :: [Tree])
    check_ def {testCases = 200} $
      forEach (trees & Gen.build) $
        \t -> modifyIORef' seen (t :)
    ts <- readIORef seen
    ts `shouldSatisfy` any (not . isTreeBranch)
    ts `shouldSatisfy` any isTreeBranch

  it "hands the branch function its own depth and the configured maxDepth" $ do
    prop (dTrees & Gen.maxDepth 4 & Gen.build) $ \t ->
      t `shouldSatisfy` depthContextIsConsistent 4

  it "shrinks to a single leaf when nothing else constrains the search" $ do
    capture <- newIORef (Branch [])
    report <- check def $ forEach (trees & Gen.build) $ \t -> do
      writeIORef capture t
      expectationFailure "always fails, to drive shrinking to the global minimum"
    case report.result of
      Counterexample {} -> readIORef capture >>= (`shouldBe` Leaf 0)
      other -> expectationFailure ("expected a counterexample, got: " <> show other)

  it "shrinks a branch predicate to the smallest branch" $ do
    capture <- newIORef (Branch [])
    report <- check def $ forEach (trees & Gen.build) $ \t -> do
      writeIORef capture t
      isTreeBranch t `shouldBe` False
    case report.result of
      Counterexample {} -> readIORef capture >>= (`shouldBe` Branch [])
      other -> expectationFailure ("expected a counterexample, got: " <> show other)

  it "hoists a deep witness toward the root instead of only shrinking leaves in place" $ do
    capture <- newIORef (ILeaf 0)
    report <- check def $ forEach (intTrees & Gen.maxDepth 3 & Gen.build) $ \t -> do
      writeIORef capture t
      hasOddLeafPair t `shouldBe` False
    case report.result of
      Counterexample {} -> readIORef capture >>= (`shouldBe` IBranch (ILeaf 1) (ILeaf 1))
      other -> expectationFailure ("expected a counterexample, got: " <> show other)

  -- Binary branches always draw exactly two children, so almost every
  -- generation attempt against a leaf budget this tight overruns it and
  -- unwinds through 'Hegel.Internal.Control.LeafBudgetExceeded'. Getting the
  -- span discipline in 'Hegel.Gen.Recursive' wrong doesn't crash the run; it
  -- corrupts shrinking, so the assertion here is on the shrunk shape itself,
  -- not merely that the run completes.
  it "keeps span bookkeeping balanced when leaf-budget retries fire during shrinking" $ do
    capture <- newIORef BLeaf
    report <- check def $ forEach (binTrees & Gen.maxLeaves 3 & Gen.build) $ \t -> do
      writeIORef capture t
      binLeafCount t `shouldSatisfy` (< 2)
    case report.result of
      Counterexample {} -> readIORef capture >>= (`shouldBe` BBranch BLeaf BLeaf)
      other -> expectationFailure ("expected a counterexample, got: " <> show other)
