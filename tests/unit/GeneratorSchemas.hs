module GeneratorSchemas (spec) where

import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty (..))
import Hegel (runProperty_)
import Hegel.Generators (Generator, assume, filtered, oneOf)
import Hegel.Generators.Integer qualified as Integer
import Hegel.Runner (defaultSettings)
import Test.Hspec

intR :: (Int, Int) -> Generator Int
intR r = Integer.gen $ Integer.integers @Int & Integer.withRange r

spec :: Spec
spec = do
  it "pure yields the constant value" $
    runProperty_ defaultSettings (pure (42 :: Int)) (`shouldBe` 42)

  it "ap of two basics generates pairs within both ranges" $ do
    let g = (,) <$> intR (0, 10) <*> intR (0, 10)
    runProperty_ defaultSettings g $ \(a, b) -> do
      a `shouldSatisfy` (\x -> x >= 0 && x <= 10)
      b `shouldSatisfy` (\x -> x >= 0 && x <= 10)

  it "ap (pure f) g uses single-leaf optimisation" $ do
    let g = pure (+ 1) <*> intR (0, 10)
    runProperty_ defaultSettings g $ \n ->
      n `shouldSatisfy` (\x -> x >= 1 && x <= 11)

  it "ap g (pure a) uses single-leaf optimisation without TUPLE span" $ do
    let g = fmap const (filtered even (intR (0, 20))) <*> pure ()
    runProperty_ defaultSettings g $ \n -> do
      n `shouldSatisfy` even
      n `shouldSatisfy` (\x -> x >= 0 && x <= 20)

  it "fmap fuses: fmap f (fmap g x) = fmap (f . g) x" $ do
    let g = fmap (+ 1) (fmap (* 2) (intR (0, 10)))
    runProperty_ defaultSettings g $ \n -> do
      n `shouldSatisfy` (\x -> x >= 1 && x <= 21)
      n `shouldSatisfy` odd

  it "oneOf of all-basic generators uses oneOfSchema" $ do
    let g = oneOf (intR (0, 10) :| [intR (20, 30)])
    runProperty_ defaultSettings g $ \n ->
      n `shouldSatisfy` (\x -> (x >= 0 && x <= 10) || (x >= 20 && x <= 30))

  it "nested ap + oneOf produces correct schema nesting" $ do
    let g = (,) <$> oneOf (intR (0, 5) :| [intR (10, 15)]) <*> intR (0, 10)
    runProperty_ defaultSettings g $ \(a, b) -> do
      a `shouldSatisfy` (\x -> (x >= 0 && x <= 5) || (x >= 10 && x <= 15))
      b `shouldSatisfy` (\x -> x >= 0 && x <= 10)

  it "monadic bind falls back to FLAT_MAP span" $ do
    let g = intR (0, 5) >>= \lo -> intR (lo, lo + 5)
    runProperty_ defaultSettings g $ \n ->
      n `shouldSatisfy` (\x -> x >= 0 && x <= 10)

  it "filtered discards values that fail the predicate" $ do
    let g = filtered even (intR (0, 20))
    runProperty_ defaultSettings g $ \n ->
      n `shouldSatisfy` even

  it "assume discards test cases that violate the assumption" $ do
    let g = do
          n <- intR (0, 20)
          assume (n /= 7)
          pure n
    runProperty_ defaultSettings g $ \n ->
      n `shouldNotBe` 7
