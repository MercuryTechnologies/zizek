module StandardGenerators (spec) where

import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Hegel (runProperty_)
import Hegel.Gen qualified as Gen
import Hegel.Runner (Settings (..), defaultSettings)
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

  describe "Gen.element" $ do
    it "only emits values from the given list" $
      runProperty_ defaultSettings (Gen.element ('a' :| "bcd")) $ \c ->
        c `shouldSatisfy` (`elem` ("abcd" :: String))

  describe "Gen.frequency" $ do
    it "covers all branches across many draws" $ do
      seen <- newIORef ([] :: [Int])
      runProperty_
        defaultSettings {testCases = 200}
        (Gen.frequency ((1, pure (1 :: Int)) :| [(1, pure 2), (1, pure 3)]))
        $ \n -> modifyIORef' seen (n :)
      vs <- readIORef seen
      vs `shouldSatisfy` (\xs -> 1 `elem` xs && 2 `elem` xs && 3 `elem` xs)

  describe "Gen.maybe" $ do
    it "emits both Nothing and Just" $ do
      seen <- newIORef ([] :: [Bool])
      runProperty_
        defaultSettings {testCases = 200}
        (Gen.maybe (Gen.bool & Gen.build))
        $ \m -> modifyIORef' seen (maybe False (const True) m :)
      vs <- readIORef seen
      vs `shouldSatisfy` (\xs -> True `elem` xs && False `elem` xs)

  describe "Gen.either" $ do
    it "emits both Left and Right" $ do
      seen <- newIORef ([] :: [Bool])
      runProperty_
        defaultSettings {testCases = 200}
        (Gen.either (Gen.bool & Gen.build) (Gen.bool & Gen.build))
        $ \e -> modifyIORef' seen (either (const False) (const True) e :)
      vs <- readIORef seen
      vs `shouldSatisfy` (\xs -> True `elem` xs && False `elem` xs)

  describe "Gen.mapMaybe" $ do
    it "only produces values satisfying the predicate" $
      runProperty_ defaultSettings (Gen.mapMaybe (\n -> if even n then Just n else Nothing) (Gen.int & Gen.min 0 & Gen.max 100 & Gen.build)) $ \n ->
        n `shouldSatisfy` even

  describe "Gen.just" $ do
    it "unwraps Just values" $
      runProperty_ defaultSettings (Gen.just (Gen.maybe (Gen.bool & Gen.build))) $ \b ->
        b `shouldSatisfy` (\x -> x == True || x == False)

  describe "Gen.enumBounded" $ do
    it "covers all constructors of a bounded enum" $ do
      seen <- newIORef ([] :: [Ordering])
      runProperty_
        defaultSettings {testCases = 200}
        Gen.enumBounded
        $ \o -> modifyIORef' seen (o :)
      vs <- readIORef seen
      vs `shouldSatisfy` (\xs -> LT `elem` xs && EQ `elem` xs && GT `elem` xs)

  describe "Gen.enum" $ do
    it "stays within the given range" $
      runProperty_ defaultSettings (Gen.enum LT GT) $ \o ->
        o `shouldSatisfy` (\x -> x >= LT && x <= GT)

-- StopTest (server entropy exhausted mid-case) is exercised end-to-end by
-- StopTestOnGenerateConformance in tests/conformance/test_conformance.py.
