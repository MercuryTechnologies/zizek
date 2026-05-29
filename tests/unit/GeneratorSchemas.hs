module GeneratorSchemas (spec) where

import CBOR.Class (ToCBOR (..))
import CBOR.Value (Value (..))
import Data.Function ((&))
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Vector qualified as V
import Hegel (Gen, runProperty_)
import Hegel.Gen qualified as Gen
import Hegel.Gen.Internal (BasicGenerator (..), Gen (..), materialize, schemaArity, toBasic)
import Hegel.Protocol.Cbor (ParseError (..))
import Hegel.Runner (defaultSettings)
import Hegel.Schema qualified as Schema
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
    let g = Gen.oneOf [intR (0, 10), intR (20, 30)]
    runProperty_ defaultSettings g $ \n ->
      n `shouldSatisfy` (\x -> (x >= 0 && x <= 10) || (x >= 20 && x <= 30))

  it "nested ap + oneOf produces correct schema nesting" $ do
    let g = (,) <$> Gen.oneOf [intR (0, 5), intR (10, 15)] <*> intR (0, 10)
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

  it "ap chain of length 3 produces flat schema parts, not nested" $ do
    let g = (,,) <$> (Gen.bool & Gen.build) <*> intR (0, 10) <*> (Gen.bool & Gen.build)
    case g of
      Ap (Just bg) _ _ -> schemaArity bg.schema `shouldBe` 3
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
    let g = Gen.oneOf [Gen.bool & Gen.build, Gen.bool & Gen.build]
    case g of
      OneOf (Just bg) _ ->
        case bg.parse (Array (V.fromList [UInt 0, Bool True, Bool False])) of
          Left err -> T.unpack err.expected `shouldContain` "[index, value] array"
          Right _ -> expectationFailure "expected parse error"
      _ -> expectationFailure "expected OneOf with basic schema"

  it "basicOneOf parser error names both bounds on out-of-range index" $ do
    let g = Gen.oneOf [Gen.bool & Gen.build, Gen.bool & Gen.build]
    case g of
      OneOf (Just bg) _ ->
        case bg.parse (Array (V.fromList [NInt 0, Bool True])) of
          Left err -> T.unpack err.expected `shouldContain` "0 <= index"
          Right _ -> expectationFailure "expected parse error"
      _ -> expectationFailure "expected OneOf with basic schema"

  describe "Gen.binary" $ do
    it "schema: default bounds, no max_size" $ do
      let g = Gen.binary & Gen.build
      case toBasic g of
        Just bg -> materialize bg.schema `shouldBe` toCBOR (Schema.binary 0 Nothing)
        Nothing -> expectationFailure "expected basic generator"

    it "schema: explicit minSize and maxSize" $ do
      let g = Gen.binary & Gen.minSize 4 & Gen.maxSize 64 & Gen.build
      case toBasic g of
        Just bg -> materialize bg.schema `shouldBe` toCBOR (Schema.binary 4 (Just 64))
        Nothing -> expectationFailure "expected basic generator"

    it "parser rejects non-ByteString values" $ do
      let g = Gen.binary & Gen.build
      case toBasic g of
        Just bg ->
          case bg.parse (Bool True) of
            Left err -> T.unpack err.expected `shouldBe` "bytes"
            Right _ -> expectationFailure "expected parse error"
        Nothing -> expectationFailure "expected basic generator"

  describe "Gen.list" $ do
    let elemGen = Gen.int & Gen.min 0 & Gen.max 100 & Gen.build
        intElemSchema = case toBasic elemGen of
          Just bg -> materialize bg.schema
          Nothing -> error "intElemSchema: elemGen should be basic"

    it "basic path: produces a Basic gen with list schema" $ do
      let g = Gen.list elemGen & Gen.build
      case toBasic g of
        Just _bg -> pure ()
        Nothing -> expectationFailure "expected basic generator for list of basic elements"

    it "basic path with size bounds: respects min and max"
      $ runProperty_
        defaultSettings
        (Gen.list elemGen & Gen.minSize 1 & Gen.maxSize 5 & Gen.build)
      $ \xs -> do
        length xs `shouldSatisfy` (>= 1)
        length xs `shouldSatisfy` (<= 5)

    it "basic path with unique: produces distinct elements"
      $ runProperty_
        defaultSettings
        ( Gen.list (Gen.int & Gen.min 0 & Gen.max 1000 & Gen.build)
            & Gen.minSize 3
            & Gen.maxSize 10
            & Gen.unique (==)
            & Gen.build
        )
      $ \xs ->
        length xs `shouldBe` length (nub xs)

    it "non-basic path: list of filtered elements exercises collection loop"
      $ runProperty_
        defaultSettings
        ( Gen.list (Gen.filtered even elemGen)
            & Gen.minSize 1
            & Gen.maxSize 5
            & Gen.build
        )
      $ \xs -> do
        length xs `shouldSatisfy` (>= 1)
        all even xs `shouldBe` True

    it "schema: default bounds, unique=False" $ do
      let g = Gen.list elemGen & Gen.build
      case toBasic g of
        Just bg -> materialize bg.schema `shouldBe` toCBOR (Schema.list intElemSchema 0 Nothing False)
        Nothing -> expectationFailure "expected basic generator"

    it "schema: min/max size present" $ do
      let g = Gen.list elemGen & Gen.minSize 1 & Gen.maxSize 5 & Gen.build
      case toBasic g of
        Just bg -> materialize bg.schema `shouldBe` toCBOR (Schema.list intElemSchema 1 (Just 5) False)
        Nothing -> expectationFailure "expected basic generator"

    it "schema: unique=True when predicate set" $ do
      let g = Gen.list elemGen & Gen.unique (==) & Gen.build
      case toBasic g of
        Just bg -> materialize bg.schema `shouldBe` toCBOR (Schema.list intElemSchema 0 Nothing True)
        Nothing -> expectationFailure "expected basic generator"

    it "non-basic path: toBasic returns Nothing for filtered elements" $ do
      let g = Gen.list (Gen.filtered even elemGen) & Gen.build
      case toBasic g of
        Nothing -> pure ()
        Just _ -> expectationFailure "expected non-basic generator"

    it "parser rejects non-Array values" $ do
      let g = Gen.list elemGen & Gen.build
      case toBasic g of
        Just bg ->
          case bg.parse (Bool True) of
            Left err -> T.unpack err.expected `shouldBe` "array"
            Right _ -> expectationFailure "expected parse error"
        Nothing -> expectationFailure "expected basic generator"

  describe "Gen.set" $ do
    let elemGen' = Gen.int & Gen.min 0 & Gen.max 100 & Gen.build
        intElemSchema' = case toBasic elemGen' of
          Just bg -> materialize bg.schema
          Nothing -> error "intElemSchema': elemGen' should be basic"

    it "basic path: schema is list with unique=True" $ do
      let g = Gen.set elemGen' & Gen.build :: Gen (Set Int)
      case toBasic g of
        Just bg -> materialize bg.schema `shouldBe` toCBOR (Schema.list intElemSchema' 0 Nothing True)
        Nothing -> expectationFailure "expected basic generator"

    it "schema: min/max size present" $ do
      let g = Gen.set elemGen' & Gen.minSize 2 & Gen.maxSize 8 & Gen.build :: Gen (Set Int)
      case toBasic g of
        Just bg -> materialize bg.schema `shouldBe` toCBOR (Schema.list intElemSchema' 2 (Just 8) True)
        Nothing -> expectationFailure "expected basic generator"

    it "basic path: produces distinct elements"
      $ runProperty_
        defaultSettings
        (Gen.set (Gen.int & Gen.min 0 & Gen.max 1000 & Gen.build) & Gen.minSize 3 & Gen.maxSize 10 & Gen.build)
      $ \s ->
        length (nub (Set.toList s)) `shouldBe` length (Set.toList s)

  describe "Gen.map" $ do
    let keyGen' = Gen.int & Gen.min 0 & Gen.max 100 & Gen.build
        valGen' = Gen.int & Gen.min 0 & Gen.max 100 & Gen.build
        keySchema' = case toBasic keyGen' of
          Just bg -> materialize bg.schema
          Nothing -> error "keySchema': keyGen' should be basic"
        valSchema' = case toBasic valGen' of
          Just bg -> materialize bg.schema
          Nothing -> error "valSchema': valGen' should be basic"

    it "basic path: schema is dict with key and value sub-schemas" $ do
      let g = Gen.map keyGen' valGen' & Gen.build :: Gen (Map Int Int)
      case toBasic g of
        Just bg -> materialize bg.schema `shouldBe` toCBOR (Schema.map keySchema' valSchema' 0 Nothing)
        Nothing -> expectationFailure "expected basic generator"

    it "schema: min/max size present" $ do
      let g = Gen.map keyGen' valGen' & Gen.minSize 1 & Gen.maxSize 5 & Gen.build :: Gen (Map Int Int)
      case toBasic g of
        Just bg -> materialize bg.schema `shouldBe` toCBOR (Schema.map keySchema' valSchema' 1 (Just 5))
        Nothing -> expectationFailure "expected basic generator"

    it "non-basic path when key generator is non-basic" $ do
      let g = Gen.map (Gen.filtered even keyGen') valGen' & Gen.build :: Gen (Map Int Int)
      case toBasic g of
        Nothing -> pure ()
        Just _ -> expectationFailure "expected non-basic generator"
