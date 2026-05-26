module StandardGenerators (spec) where

import Data.ByteString qualified as BS
import Hegel (runProperty_)
import Hegel.Gen qualified as Gen
import Hegel.Gen.Float (FloatOptions (..))
import Hegel.Range qualified as Range
import Hegel.Runner (defaultSettings)
import Test.Hspec

spec :: Spec
spec = do
  describe "Gen.bool" $ do
    it "draws Bool values" $
      runProperty_ defaultSettings Gen.bool $
        \_ -> pure ()

  describe "Gen.binary" $ do
    it "draws ByteStrings" $
      runProperty_ defaultSettings (Gen.binaryWith Gen.defaultBinaryOptions) $
        \_ -> pure ()

    it "respects lower bound" $
      runProperty_ defaultSettings (Gen.binary (Range.between 5 100)) $ \bs ->
        BS.length bs `shouldSatisfy` (>= 5)

    it "respects upper bound" $
      runProperty_ defaultSettings (Gen.binary (Range.between 0 10)) $ \bs ->
        BS.length bs `shouldSatisfy` (<= 10)

    it "respects both bounds" $
      runProperty_ defaultSettings (Gen.binary (Range.between 3 7)) $ \bs -> do
        BS.length bs `shouldSatisfy` (>= 3)
        BS.length bs `shouldSatisfy` (<= 7)

  describe "Gen.double" $ do
    it "draws Double values" $
      runProperty_ defaultSettings (Gen.doubleWith Gen.defaultFloatOptions) $
        \_ -> pure ()

    it "respects min and max bounds" $
      runProperty_ defaultSettings (Gen.double (Range.between (-2.0) 3.0)) $ \x ->
        x `shouldSatisfy` (\v -> v >= -2.0 && v <= 3.0)

    it "respects min bound" $
      runProperty_ defaultSettings (Gen.doubleWith Gen.defaultFloatOptions {minValue = Just 0.0}) $ \x ->
        x `shouldSatisfy` (>= 0.0)

    it "respects max bound" $
      runProperty_ defaultSettings (Gen.doubleWith Gen.defaultFloatOptions {maxValue = Just 0.0}) $ \x ->
        x `shouldSatisfy` (<= 0.0)

    it "generates exact value when min equals max" $
      runProperty_ defaultSettings (Gen.double (Range.singleton 3.14)) $ \x ->
        x `shouldBe` 3.14

  describe "Gen.float" $ do
    it "draws Float values" $
      runProperty_ defaultSettings (Gen.floatWith Gen.defaultFloatOptions) $
        \_ -> pure ()

-- StopTest (server entropy exhausted mid-case) is exercised end-to-end by
-- StopTestOnGenerateConformance in tests/conformance/test_conformance.py.
