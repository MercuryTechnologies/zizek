module StandardGenerators (spec) where

import Data.ByteString qualified as BS
import Data.Function ((&))
import Hegel (runProperty_)
import Hegel.Generators.Binary qualified as Binary
import Hegel.Generators.Bool qualified as Bool
import Hegel.Generators.Float qualified as Float
import Hegel.Runner (defaultSettings)
import Test.Hspec

spec :: Spec
spec = do
  describe "Bool.gen" $ do
    it "draws Bool values" $
      runProperty_ defaultSettings Bool.gen $
        \_ -> pure ()

  describe "Binary.gen" $ do
    it "draws ByteStrings" $
      runProperty_ defaultSettings (Binary.gen Binary.binary) $
        \_ -> pure ()

    it "respects minSize" $
      runProperty_ defaultSettings (Binary.gen Binary.binary {Binary.minSize = 5}) $ \bs ->
        BS.length bs `shouldSatisfy` (>= 5)

    it "respects maxSize" $
      runProperty_ defaultSettings (Binary.gen Binary.binary {Binary.maxSize = Just 10}) $ \bs ->
        BS.length bs `shouldSatisfy` (<= 10)

    it "respects minSize and maxSize together"
      $ runProperty_
        defaultSettings
        (Binary.gen Binary.binary {Binary.minSize = 3, Binary.maxSize = Just 7})
      $ \bs -> do
        BS.length bs `shouldSatisfy` (>= 3)
        BS.length bs `shouldSatisfy` (<= 7)

  describe "Float.genDouble" $ do
    it "draws Double values" $
      runProperty_ defaultSettings (Float.genDouble Float.floats) $
        \_ -> pure ()

    it "respects min and max bounds"
      $ runProperty_
        defaultSettings
        (Float.genDouble $ Float.floats & Float.withMinValue (-2.0) & Float.withMaxValue 3.0)
      $ \x -> x `shouldSatisfy` (\v -> v >= -2.0 && v <= 3.0)

    it "respects min bound"
      $ runProperty_
        defaultSettings
        (Float.genDouble $ Float.floats & Float.withMinValue 0.0)
      $ \x -> x `shouldSatisfy` (>= 0.0)

    it "respects max bound"
      $ runProperty_
        defaultSettings
        (Float.genDouble $ Float.floats & Float.withMaxValue 0.0)
      $ \x -> x `shouldSatisfy` (<= 0.0)

    it "generates exact value when min equals max"
      $ runProperty_
        defaultSettings
        (Float.genDouble $ Float.floats & Float.withMinValue 3.14 & Float.withMaxValue 3.14)
      $ \x -> x `shouldBe` 3.14

  describe "Float.genFloat" $ do
    it "draws Float values" $
      runProperty_ defaultSettings (Float.genFloat Float.floats) $
        \_ -> pure ()

-- StopTest (server entropy exhausted mid-case) is exercised end-to-end by
-- StopTestOnGenerateConformance in tests/conformance/test_conformance.py.
