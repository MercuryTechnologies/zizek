module StandardGenerators (spec) where

import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.Text qualified as T
import Hegel (Gen, prop)
import Hegel.Gen qualified as Gen
import Hegel.HealthCheck (HealthCheck (..))
import Hegel.Property (check, check_, forEach)
import Hegel.Report (Report (..), Result (..), Stats (..))
import Hegel.Settings (Settings (..), defaultSettings)
import Network.URI (uriScheme)
import Test.Hspec
import UnliftIO.IORef (modifyIORef', newIORef, readIORef)

-- | A self-referential structure whose generator recurses through 'Gen.defer'.
data Tree = Leaf | Branch Tree Tree
  deriving stock (Eq, Show)

-- | A recursive tree generator whose construction terminates because each
-- recursive edge is wrapped in 'Gen.defer'.
treeGen :: Gen Tree
treeGen = Gen.oneOf [pure Leaf, Branch <$> Gen.defer treeGen <*> Gen.defer treeGen]

isBranch :: Tree -> Bool
isBranch Branch {} = True
isBranch Leaf = False

spec :: Spec
spec = do
  describe "Gen.bool" $ do
    it "draws Bool values" $ do
      check_ (defaultSettings {testCases = 1}) $ forEach (Gen.bool & Gen.build) $ \_ -> pure ()

    it "weighted 1.0 always draws True, 0.0 always False" $ do
      prop (Gen.bool & Gen.weighted 1.0 & Gen.build) $ \b ->
        b `shouldBe` True
      prop (Gen.bool & Gen.weighted 0.0 & Gen.build) $ \b ->
        b `shouldBe` False

  describe "Gen.binary" $ do
    it "draws ByteStrings" $ do
      check_ (defaultSettings {testCases = 1}) $ forEach (Gen.binary & Gen.build) $ \_ -> pure ()

    it "respects lower bound" $ do
      prop (Gen.binary & Gen.minSize 5 & Gen.maxSize 100 & Gen.build) $ \bs ->
        BS.length bs `shouldSatisfy` (>= 5)

    it "respects upper bound" $ do
      prop (Gen.binary & Gen.maxSize 10 & Gen.build) $ \bs ->
        BS.length bs `shouldSatisfy` (<= 10)

    it "respects both bounds" $ do
      prop (Gen.binary & Gen.minSize 3 & Gen.maxSize 7 & Gen.build) $ \bs -> do
        BS.length bs `shouldSatisfy` (>= 3)
        BS.length bs `shouldSatisfy` (<= 7)

  describe "Gen.double" $ do
    it "draws Double values" $ do
      check_ (defaultSettings {testCases = 1}) $ forEach (Gen.double & Gen.build) $ \_ -> pure ()

    it "respects min and max bounds" $ do
      prop (Gen.double & Gen.min (-2.0) & Gen.max 3.0 & Gen.build) $ \x ->
        x `shouldSatisfy` (\v -> v >= -2.0 && v <= 3.0)

    it "respects min bound" $ do
      prop (Gen.double & Gen.min 0.0 & Gen.build) $ \x ->
        x `shouldSatisfy` (>= 0.0)

    it "respects max bound" $ do
      prop (Gen.double & Gen.max 0.0 & Gen.build) $ \x ->
        x `shouldSatisfy` (<= 0.0)

    it "generates exact value when min equals max" $ do
      prop (Gen.double & Gen.min 3.14 & Gen.max 3.14 & Gen.build) $ \x ->
        x `shouldBe` 3.14

  describe "Gen.float" $ do
    it "draws Float values" $ do
      check_ (defaultSettings {testCases = 1}) $ forEach (Gen.float & Gen.build) $ \_ -> pure ()

  describe "Gen.element" $ do
    it "only emits values from the given list" $ do
      prop (Gen.element "abcd") $ \c ->
        c `shouldSatisfy` (`elem` ("abcd" :: String))

  describe "Gen.frequency" $ do
    it "covers all branches across many draws" $ do
      seen <- newIORef ([] :: [Int])
      check_
        (defaultSettings {testCases = 200})
        $ forEach (Gen.frequency @Int [(1, pure 1), (1, pure 2), (1, pure 3)])
        $ \n -> modifyIORef' seen (n :)
      vs <- readIORef seen
      vs `shouldSatisfy` (\xs -> 1 `elem` xs && 2 `elem` xs && 3 `elem` xs)

  describe "Gen.maybe" $ do
    it "emits both Nothing and Just" $ do
      seen <- newIORef ([] :: [Bool])
      check_
        (defaultSettings {testCases = 200})
        $ forEach (Gen.maybe (Gen.bool & Gen.build))
        $ \m -> modifyIORef' seen (maybe False (const True) m :)
      vs <- readIORef seen
      vs `shouldSatisfy` (\xs -> True `elem` xs && False `elem` xs)

  describe "Gen.either" $ do
    it "emits both Left and Right" $ do
      seen <- newIORef ([] :: [Bool])
      check_
        (defaultSettings {testCases = 200})
        $ forEach (Gen.either (Gen.bool & Gen.build) (Gen.bool & Gen.build))
        $ \e -> modifyIORef' seen (either (const False) (const True) e :)
      vs <- readIORef seen
      vs `shouldSatisfy` (\xs -> True `elem` xs && False `elem` xs)

  describe "Gen.mapMaybe" $ do
    it "only produces values satisfying the predicate" $ do
      prop
        (Gen.mapMaybe (\n -> if even n then Just n else Nothing) (Gen.int & Gen.min 0 & Gen.max 100 & Gen.build))
        $ \n -> n `shouldSatisfy` even

    -- A highly selective mapMaybe over a *finite* source (Gen.element is
    -- enumerable) collapses to the pre-mapped list statically, so it never
    -- falls back to the 3-try retry loop — and therefore never discards a
    -- satisfiable case just because the retries missed the one match.
    it "does not discard when the source is finite" $ do
      report <-
        check (defaultSettings {testCases = 200}) $
          forEach (Gen.mapMaybe (\n -> if n == 10 then Just n else Nothing) (Gen.element [1 .. 10 :: Int])) $
            \n -> n `shouldBe` 10
      report.stats.invalid `shouldBe` 0

  describe "Gen.just" $ do
    it "unwraps Just values" $ do
      prop (Gen.just (Gen.maybe (Gen.bool & Gen.build))) $ \b ->
        b `shouldSatisfy` (\x -> x == True || x == False)

    it "does not discard when the source is finite" $ do
      report <-
        check (defaultSettings {testCases = 200}) $
          forEach (Gen.just (Gen.element [Nothing, Nothing, Just (42 :: Int)])) $
            \n -> n `shouldBe` 42
      report.stats.invalid `shouldBe` 0

  describe "Gen.enumBounded" $ do
    it "covers all constructors of a bounded enum" $ do
      seen <- newIORef ([] :: [Ordering])
      check_ (defaultSettings {testCases = 200}) $
        forEach Gen.enumBounded $
          \o -> modifyIORef' seen (o :)
      vs <- readIORef seen
      vs `shouldSatisfy` (\xs -> LT `elem` xs && EQ `elem` xs && GT `elem` xs)

  describe "Gen.enum" $ do
    it "stays within the given range" $ do
      prop (Gen.enum LT GT) $ \o ->
        o `shouldSatisfy` (\x -> x >= LT && x <= GT)

  describe "Gen.text" $ do
    it "draws Text values" $ do
      check_ (defaultSettings {testCases = 1}) $ forEach (Gen.text & Gen.build) $ \_ -> pure ()

    it "respects minSize bound" $ do
      prop (Gen.text & Gen.minSize 5 & Gen.maxSize 100 & Gen.build) $ \t ->
        T.length t `shouldSatisfy` (>= 5)

    it "respects maxSize bound" $ do
      prop (Gen.text & Gen.maxSize 10 & Gen.build) $ \t ->
        T.length t `shouldSatisfy` (<= 10)

    it "respects both bounds" $ do
      prop (Gen.text & Gen.minSize 3 & Gen.maxSize 7 & Gen.build) $ \t -> do
        T.length t `shouldSatisfy` (>= 3)
        T.length t `shouldSatisfy` (<= 7)

    it "respects an alphabet" $ do
      prop
        ( Gen.text
            & Gen.minSize 1
            & Gen.maxSize 10
            & Gen.alphabet (Gen.char & Gen.minCodepoint 97 & Gen.maxCodepoint 122)
            & Gen.build
        )
        $ \t -> t `shouldSatisfy` T.all (\c -> c >= 'a' && c <= 'z')

  describe "Gen.char" $ do
    it "draws Char values" $ do
      check_ (defaultSettings {testCases = 1}) $ forEach (Gen.char & Gen.build) $ \_ -> pure ()

  describe "Gen.regex" $ do
    it "draws Text values" $ do
      check_ (defaultSettings {testCases = 1}) $ forEach (Gen.regex "[a-z]+" & Gen.build) $ \_ -> pure ()

    it "respects the pattern with fullMatch" $ do
      prop (Gen.regex "[0-9]+" & Gen.fullMatch & Gen.build) $ \t ->
        t `shouldSatisfy` T.all (\c -> c >= '0' && c <= '9')

    it "fullMatch produces complete matches" $ do
      prop (Gen.regex "[a-z]+" & Gen.fullMatch & Gen.build) $ \t ->
        t `shouldSatisfy` (not . T.null)

    it "respects an alphabet" $ do
      prop
        (Gen.regex ".+" & Gen.fullMatch & Gen.alphabet (Gen.char & Gen.minCodepoint 97 & Gen.maxCodepoint 122) & Gen.build)
        $ \t -> t `shouldSatisfy` T.all (\c -> c >= 'a' && c <= 'z')

  describe "Gen.uuid" $ do
    it "draws UUID values" $ do
      check_ (defaultSettings {testCases = 1}) $ forEach (Gen.uuid & Gen.build) $ \_ -> pure ()

    it "draws version-4 UUIDs" $ do
      check_ (defaultSettings {testCases = 1}) $ forEach (Gen.uuid & Gen.version 4 & Gen.build) $ \_ -> pure ()

  describe "Gen.uri" $ do
    it "draws URI values with http or https scheme" $ do
      prop (Gen.uri & Gen.build) $ \u ->
        uriScheme u `shouldSatisfy` (`elem` ["http:", "https:"])

  describe "Gen.uriText" $ do
    it "draws URI text starting with http" $ do
      prop (Gen.uriText & Gen.build) $ \t ->
        t `shouldSatisfy` (\s -> "http://" `T.isPrefixOf` s || "https://" `T.isPrefixOf` s)

  describe "Gen.domain" $ do
    it "draws non-empty domain names containing a dot" $ do
      prop (Gen.domain & Gen.build) $ \t -> do
        t `shouldSatisfy` (not . T.null)
        t `shouldSatisfy` T.isInfixOf "."

    it "respects maxLength" $ do
      prop (Gen.domain & Gen.maxLength 30 & Gen.build) $ \t ->
        T.length t `shouldSatisfy` (<= 30)

  describe "Gen.assume" $ do
    -- A true condition is a no-op, so every case stays valid.
    it "keeps every case valid when the condition holds" $ do
      report <-
        check (defaultSettings {testCases = 50}) $
          forEach (Gen.assume True >> pure (1 :: Int)) $
            \n -> n `shouldBe` 1
      report.stats.invalid `shouldBe` 0

    -- A false condition discards the case, so survivors satisfy the assumed
    -- bound and the discards count as invalid.
    it "discards the case when the condition fails" $ do
      let bounded = do
            x <- Gen.int & Gen.min 0 & Gen.max 100 & Gen.build
            Gen.assume (x < 50)
            pure x
      report <- check (defaultSettings {testCases = 200}) $ forEach bounded $ \x -> x `shouldSatisfy` (< 50)
      case report.result of
        Ok -> report.stats.invalid `shouldSatisfy` (> 0)
        other -> expectationFailure ("expected Ok, got: " <> show other)

  describe "Gen.discard" $ do
    -- A conditional discard rejects the cases that reach it, so survivors
    -- satisfy the guard and the rejects count as invalid.
    it "discards the cases that reach it" $ do
      let bounded = do
            x <- Gen.int & Gen.min 0 & Gen.max 100 & Gen.build
            if x < 50 then pure x else Gen.discard
      report <- check (defaultSettings {testCases = 200}) $ forEach bounded $ \x -> x `shouldSatisfy` (< 50)
      case report.result of
        Ok -> report.stats.invalid `shouldSatisfy` (> 0)
        other -> expectationFailure ("expected Ok, got: " <> show other)

    -- A generator that only ever discards yields no valid examples, so the
    -- run gives up with every case counted invalid.
    it "gives up when every case discards" $ do
      report <-
        check (defaultSettings {testCases = 50, suppressHealthCheck = [FilterTooMuch]}) $
          forEach (Gen.discard :: Gen Int) $
            \_ -> pure ()
      case report.result of
        GaveUp _ -> do
          report.stats.valid `shouldBe` 0
          report.stats.invalid `shouldSatisfy` (> 0)
        other -> expectationFailure ("expected GaveUp, got: " <> show other)

  describe "Gen.filtered" $ do
    -- A finite, satisfiable source takes the static fast path: the predicate
    -- applies to the enumerated values directly, so no case is discarded.
    it "never discards over a finite satisfiable source" $ do
      report <-
        check (defaultSettings {testCases = 200}) $
          forEach (Gen.filtered even $ Gen.element [1 .. 10 :: Int]) $
            \n -> n `shouldSatisfy` even
      report.stats.invalid `shouldBe` 0

    -- A finite source with no satisfying value collapses to a discard on
    -- every case, so the run gives up.
    it "gives up over a finite unsatisfiable source" $ do
      report <-
        check (defaultSettings {testCases = 50, suppressHealthCheck = [FilterTooMuch]}) $
          forEach (Gen.filtered (const False) $ Gen.element [1 .. 10 :: Int]) $
            \_ -> pure ()
      case report.result of
        GaveUp _ -> do
          report.stats.valid `shouldBe` 0
          report.stats.invalid `shouldSatisfy` (> 0)
        other -> expectationFailure ("expected GaveUp, got: " <> show other)

    -- A non-enumerable source falls back to the bounded retry loop. Every
    -- emitted value still respects the predicate.
    it "emits only satisfying values over a non-enumerable source" $ do
      prop (Gen.filtered even $ Gen.int & Gen.min 0 & Gen.max 100 & Gen.build) $ \n ->
        n `shouldSatisfy` even

    -- A non-enumerable source whose values never satisfy the predicate
    -- exhausts the retry budget on every case, so the run gives up.
    it "gives up over a non-enumerable unsatisfiable source" $ do
      report <-
        check (defaultSettings {testCases = 50, suppressHealthCheck = [FilterTooMuch]}) $
          forEach (Gen.filtered (const False) $ Gen.int & Gen.build) $
            \_ -> pure ()
      case report.result of
        GaveUp _ -> do
          report.stats.valid `shouldBe` 0
          report.stats.invalid `shouldSatisfy` (> 0)
        other -> expectationFailure ("expected GaveUp, got: " <> show other)

  describe "Gen.defer" $ do
    -- A recursive generator built with 'defer' terminates and produces both
    -- the base and the recursive shape.
    it "lets a recursive generator terminate and branch" $ do
      seen <- newIORef ([] :: [Tree])
      check_ (defaultSettings {testCases = 200}) $
        forEach treeGen $
          \t -> modifyIORef' seen (t :)
      ts <- readIORef seen
      ts `shouldSatisfy` elem Leaf
      ts `shouldSatisfy` any isBranch

    -- 'defer' turns its argument into an opaque draw, so a deferred generator
    -- is not enumerable even when its underlying generator is.
    it "is opaque to enumerate" $ do
      Gen.enumerate (Gen.defer (pure (1 :: Int))) `shouldBe` Nothing
      Gen.enumerate (pure (1 :: Int)) `shouldBe` Just [1]

  describe "Gen.enumerate" $ do
    it "enumerates a Pure value" $ do
      Gen.enumerate (pure (5 :: Int)) `shouldBe` Just [5]

    it "maps over an enumerable source" $ do
      Gen.enumerate ((+ 1) <$> Gen.element [1, 2, 3 :: Int]) `shouldBe` Just [2, 3, 4]

    it "takes the cartesian product when both sides enumerate" $ do
      Gen.enumerate ((,) <$> Gen.element [1, 2 :: Int] <*> Gen.element ['a', 'b'])
        `shouldBe` Just [(1, 'a'), (1, 'b'), (2, 'a'), (2, 'b')]

    it "concatenates enumerable branches of a choice" $ do
      Gen.enumerate (Gen.oneOf [pure 1, pure 2, pure 3] :: Gen Int)
        `shouldBe` Just [1, 2, 3]

    it "does not enumerate a draw leaf" $ do
      Gen.enumerate (Gen.int & Gen.build) `shouldBe` Nothing

    it "does not enumerate a monadic bind" $ do
      Gen.enumerate (Gen.element [1, 2 :: Int] >>= pure) `shouldBe` Nothing

    it "does not enumerate a product with a non-enumerable side" $ do
      Gen.enumerate ((,) <$> Gen.element [1, 2 :: Int] <*> (Gen.int & Gen.build))
        `shouldBe` Nothing

    it "does not enumerate a choice with a non-enumerable branch" $ do
      Gen.enumerate (Gen.oneOf [pure 1, Gen.int & Gen.build] :: Gen Int)
        `shouldBe` Nothing
