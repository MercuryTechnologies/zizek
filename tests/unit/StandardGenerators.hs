module StandardGenerators (spec) where

import Data.ByteString qualified as BS
import Data.Function ((&))
import Hegel (runProperty_)
import Hegel.Gen qualified as Gen
import Hegel.Runner (defaultSettings)
import Test.Hspec

spec :: Spec
spec = do
  describe "Gen.bool" $ do
    it "draws Bool values" $
      runProperty_ defaultSettings (Gen.bool & Gen.build) $
        \_ -> pure ()

  describe "Gen.binary" $ do
    it "draws ByteStrings" $
      runProperty_ defaultSettings (Gen.binary & Gen.build) $
        \_ -> pure ()

    it "respects lower bound" $
      runProperty_ defaultSettings (Gen.binary & Gen.minSize 5 & Gen.maxSize 100 & Gen.build) $ \bs ->
        BS.length bs `shouldSatisfy` (>= 5)

    it "respects upper bound" $
      runProperty_ defaultSettings (Gen.binary & Gen.maxSize 10 & Gen.build) $ \bs ->
        BS.length bs `shouldSatisfy` (<= 10)

    it "respects both bounds" $
      runProperty_ defaultSettings (Gen.binary & Gen.minSize 3 & Gen.maxSize 7 & Gen.build) $ \bs -> do
        BS.length bs `shouldSatisfy` (>= 3)
        BS.length bs `shouldSatisfy` (<= 7)

  describe "Gen.double" $ do
    it "draws Double values" $
      runProperty_ defaultSettings (Gen.double & Gen.build) $
        \_ -> pure ()

    it "respects min and max bounds" $
      runProperty_ defaultSettings (Gen.double & Gen.min (-2.0) & Gen.max 3.0 & Gen.build) $ \x ->
        x `shouldSatisfy` (\v -> v >= -2.0 && v <= 3.0)

    it "respects min bound" $
      runProperty_ defaultSettings (Gen.double & Gen.min 0.0 & Gen.build) $ \x ->
        x `shouldSatisfy` (>= 0.0)

    it "respects max bound" $
      runProperty_ defaultSettings (Gen.double & Gen.max 0.0 & Gen.build) $ \x ->
        x `shouldSatisfy` (<= 0.0)

    it "generates exact value when min equals max" $
      runProperty_ defaultSettings (Gen.double & Gen.min 3.14 & Gen.max 3.14 & Gen.build) $ \x ->
        x `shouldBe` 3.14

  describe "Gen.float" $ do
    it "draws Float values" $
      runProperty_ defaultSettings (Gen.float & Gen.build) $
        \_ -> pure ()

-- StopTest (server entropy exhausted mid-case) is exercised end-to-end by
-- StopTestOnGenerateConformance in tests/conformance/test_conformance.py.
