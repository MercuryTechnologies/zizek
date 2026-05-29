module StandardGenerators (spec) where

import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text qualified as T
import Hegel (runProperty_)
import Hegel.Gen qualified as Gen
import Hegel.Runner (Settings (..), defaultSettings)
import Network.URI (uriScheme)
import Test.Hspec

spec :: Spec
spec = do
  describe "Gen.bool" $ do
    it "draws Bool values" $
      runProperty_ defaultSettings {testCases = 1} (Gen.bool & Gen.build) $
        \_ -> pure ()

  describe "Gen.binary" $ do
    it "draws ByteStrings" $
      runProperty_ defaultSettings {testCases = 1} (Gen.binary & Gen.build) $
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
      runProperty_ defaultSettings {testCases = 1} (Gen.double & Gen.build) $
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
      runProperty_ defaultSettings {testCases = 1} (Gen.float & Gen.build) $
        \_ -> pure ()

  describe "Gen.element" $ do
    it "only emits values from the given list" $
      runProperty_ defaultSettings (Gen.element "abcd") $ \c ->
        c `shouldSatisfy` (`elem` ("abcd" :: String))

  describe "Gen.frequency" $ do
    it "covers all branches across many draws" $ do
      seen <- newIORef ([] :: [Int])
      runProperty_
        defaultSettings {testCases = 200}
        (Gen.frequency @Int [(1, pure 1), (1, pure 2), (1, pure 3)])
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

  describe "Gen.text" $ do
    it "draws Text values" $
      runProperty_ defaultSettings {testCases = 1} (Gen.text & Gen.build) $
        \_ -> pure ()

    it "respects minSize bound" $
      runProperty_ defaultSettings (Gen.text & Gen.minSize 5 & Gen.maxSize 100 & Gen.build) $ \t ->
        T.length t `shouldSatisfy` (>= 5)

    it "respects maxSize bound" $
      runProperty_ defaultSettings (Gen.text & Gen.maxSize 10 & Gen.build) $ \t ->
        T.length t `shouldSatisfy` (<= 10)

    it "respects both bounds" $
      runProperty_ defaultSettings (Gen.text & Gen.minSize 3 & Gen.maxSize 7 & Gen.build) $ \t -> do
        T.length t `shouldSatisfy` (>= 3)
        T.length t `shouldSatisfy` (<= 7)

  describe "Gen.char" $ do
    it "draws Char values" $
      runProperty_ defaultSettings {testCases = 1} (Gen.char & Gen.build) $
        \_ -> pure ()

  describe "Gen.regex" $ do
    it "draws Text values" $
      runProperty_ defaultSettings {testCases = 1} (Gen.regex "[a-z]+" & Gen.build) $
        \_ -> pure ()

    it "respects the pattern with fullMatch" $
      runProperty_ defaultSettings (Gen.regex "[0-9]+" & Gen.fullMatch & Gen.build) $ \t ->
        t `shouldSatisfy` T.all (\c -> c >= '0' && c <= '9')

    it "fullMatch produces complete matches" $
      runProperty_ defaultSettings (Gen.regex "[a-z]+" & Gen.fullMatch & Gen.build) $ \t ->
        t `shouldSatisfy` (not . T.null)

  describe "Gen.uuid" $ do
    it "draws UUID values" $
      runProperty_ defaultSettings {testCases = 1} (Gen.uuid & Gen.build) $
        \_ -> pure ()

    it "draws version-4 UUIDs" $
      runProperty_ defaultSettings {testCases = 1} (Gen.uuid & Gen.version 4 & Gen.build) $
        \_ -> pure ()

  describe "Gen.uri" $ do
    it "draws URI values with http or https scheme" $
      runProperty_ defaultSettings (Gen.uri & Gen.build) $ \u ->
        uriScheme u `shouldSatisfy` (`elem` ["http:", "https:"])

  describe "Gen.uriText" $ do
    it "draws URI text starting with http" $
      runProperty_ defaultSettings (Gen.uriText & Gen.build) $ \t ->
        t `shouldSatisfy` (\s -> "http://" `T.isPrefixOf` s || "https://" `T.isPrefixOf` s)

  describe "Gen.domain" $ do
    it "draws non-empty domain names containing a dot" $
      runProperty_ defaultSettings (Gen.domain & Gen.build) $ \t -> do
        t `shouldSatisfy` (not . T.null)
        t `shouldSatisfy` T.isInfixOf "."

    it "respects maxLength" $
      runProperty_ defaultSettings (Gen.domain & Gen.maxLength 30 & Gen.build) $ \t ->
        T.length t `shouldSatisfy` (<= 30)

-- StopTest (server entropy exhausted mid-case) is exercised end-to-end by
-- StopTestOnGenerateConformance in tests/conformance/test_conformance.py.
