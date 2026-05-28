module GeneratorSchemas (spec) where

import CBOR.Value (Value (..))
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as T
import Data.Vector qualified as V
import Hegel (Gen, runProperty_)
import Hegel.Gen qualified as Gen
import Hegel.Gen.Internal (BasicGenerator (..), Gen (..))
import Hegel.Protocol.Cbor (ParseError (..))
import Hegel.Runner (defaultSettings)
import Test.Hspec

intR :: (Int, Int) -> Gen Int
intR (lo, hi) = Gen.integral & Gen.min lo & Gen.max hi & Gen.build

spec :: Spec
spec = do
  it "pure yields the constant value" $
    runProperty_ defaultSettings (pure (42 :: Int)) (`shouldBe` 42)

  it "ap of two basics generates pairs from different generator types" $ do
    let g = (,) <$> (Gen.bool & Gen.build) <*> intR (0, 10)
    runProperty_ defaultSettings g $ \(_, n) ->
      n `shouldSatisfy` (\x -> x >= 0 && x <= 10)

  it "ap (pure f) g uses single-leaf optimisation" $ do
    let g = pure (+ 1) <*> intR (0, 10)
    runProperty_ defaultSettings g $ \n ->
      n `shouldSatisfy` (\x -> x >= 1 && x <= 11)

  it "ap g (pure a) uses single-leaf optimisation without TUPLE span" $ do
    let g = fmap const (Gen.filtered even (intR (0, 20))) <*> pure ()
    runProperty_ defaultSettings g $ \n -> do
      n `shouldSatisfy` even
      n `shouldSatisfy` (\x -> x >= 0 && x <= 20)

  it "fmap fuses: fmap f (fmap g x) = fmap (f . g) x" $ do
    let g = fmap (+ 1) (fmap (* 2) (intR (0, 10)))
    runProperty_ defaultSettings g $ \n -> do
      n `shouldSatisfy` (\x -> x >= 1 && x <= 21)
      n `shouldSatisfy` odd

  it "oneOf of all-basic generators uses oneOfSchema" $ do
    let g = Gen.oneOf (intR (0, 10) :| [intR (20, 30)])
    runProperty_ defaultSettings g $ \n ->
      n `shouldSatisfy` (\x -> (x >= 0 && x <= 10) || (x >= 20 && x <= 30))

  it "nested ap + oneOf produces correct schema nesting" $ do
    let g = (,) <$> Gen.oneOf (intR (0, 5) :| [intR (10, 15)]) <*> intR (0, 10)
    runProperty_ defaultSettings g $ \(a, b) -> do
      a `shouldSatisfy` (\x -> (x >= 0 && x <= 5) || (x >= 10 && x <= 15))
      b `shouldSatisfy` (\x -> x >= 0 && x <= 10)

  it "monadic bind falls back to FLAT_MAP span" $ do
    let g = intR (0, 5) >>= \lo -> intR (lo, lo + 5)
    runProperty_ defaultSettings g $ \n ->
      n `shouldSatisfy` (\x -> x >= 0 && x <= 10)

  it "monadic bind from Bool selects between integer ranges" $ do
    let g = (Gen.bool & Gen.build) >>= \b -> intR (if b then (0, 5) else (10, 15))
    runProperty_ defaultSettings g $ \n ->
      n `shouldSatisfy` (\x -> (x >= 0 && x <= 5) || (x >= 10 && x <= 15))

  it "filtered discards values that fail the predicate" $ do
    let g = Gen.filtered even (intR (0, 20))
    runProperty_ defaultSettings g $ \n ->
      n `shouldSatisfy` even

  it "assume discards test cases that violate the assumption" $ do
    let g = do
          n <- intR (0, 20)
          Gen.assume (n /= 7)
          pure n
    runProperty_ defaultSettings g $ \n ->
      n `shouldNotBe` 7

  it "ap chain of length 3 produces flat schemaParts, not nested" $ do
    let g = (,,) <$> (Gen.bool & Gen.build) <*> intR (0, 10) <*> (Gen.bool & Gen.build)
    case g of
      Ap (Just bg) _ _ -> length bg.schemaParts `shouldBe` 3
      _ -> expectationFailure "expected Ap with basic schema"

  it "ap chain of length 3 parser round-trips a 3-element array" $ do
    let g = (,,) <$> (Gen.bool & Gen.build) <*> intR (0, 10) <*> (Gen.bool & Gen.build)
    case g of
      Ap (Just bg) _ _ ->
        bg.parse (Array (V.fromList [Bool True, UInt 5, Bool False]))
          `shouldBe` Right (True, 5 :: Int, False)
      _ -> expectationFailure "expected Ap with basic schema"

  it "basicAp parser rejects arrays longer than 2 elements" $ do
    let g = (,) <$> (Gen.bool & Gen.build) <*> (Gen.bool & Gen.build)
    case g of
      Ap (Just bg) _ _ ->
        case bg.parse (Array (V.fromList [Bool True, Bool False, Bool False])) of
          Left err -> T.unpack err.expected `shouldContain` "2-element array"
          Right _ -> expectationFailure "expected parse error"
      _ -> expectationFailure "expected Ap with basic schema"

  it "basicAp parser rejects arrays of wrong arity for length-3 chain" $ do
    let g = (,,) <$> (Gen.bool & Gen.build) <*> intR (0, 10) <*> (Gen.bool & Gen.build)
    case g of
      Ap (Just bg) _ _ ->
        case bg.parse (Array (V.fromList [Bool True, UInt 5, Bool False, Bool True])) of
          Left err -> T.unpack err.expected `shouldContain` "3-element array"
          Right _ -> expectationFailure "expected parse error"
      _ -> expectationFailure "expected Ap with basic schema"

  it "basicOneOf parser rejects arrays longer than 2 elements" $ do
    let g = Gen.oneOf ((Gen.bool & Gen.build) :| [Gen.bool & Gen.build])
    case g of
      OneOf (Just bg) _ ->
        case bg.parse (Array (V.fromList [UInt 0, Bool True, Bool False])) of
          Left err -> T.unpack err.expected `shouldContain` "[index, value] array"
          Right _ -> expectationFailure "expected parse error"
      _ -> expectationFailure "expected OneOf with basic schema"

  it "basicOneOf parser error names both bounds on out-of-range index" $ do
    let g = Gen.oneOf ((Gen.bool & Gen.build) :| [Gen.bool & Gen.build])
    case g of
      OneOf (Just bg) _ ->
        case bg.parse (Array (V.fromList [NInt 0, Bool True])) of
          Left err -> T.unpack err.expected `shouldContain` "0 <= index"
          Right _ -> expectationFailure "expected parse error"
      _ -> expectationFailure "expected OneOf with basic schema"
