{-# LANGUAGE CPP #-}

-- | Regression coverage for 'Hegel.Internal.DataSource'\'s string-generator
-- handle census: transient handles should be GC-reclaimed, and retained ones
-- should not be. This is the assertable half of the same question
-- @tests/profile/Main.hs@'s @strgen-reclaim@ scenario answers by printing a
-- diagnostic for a human to eyeball — this spec fails if a real regression
-- shows up, rather than relying on someone watching the profiling output.
--
-- __Needs the @census@ cabal flag__ (@cabal test zizek:string-gen-handles
-- --flag census@): the handle count this suite asserts on is compiled out
-- otherwise, so every case is 'pendingWith' rather than run for real.
--
-- __Isolated in its own test-suite, deliberately__ (see
-- 'Hegel.Internal.DataSource.settleStringGenerators'\'s haddock): the same
-- assertion is flaky when run inside the shared, hundreds-of-tests @unit@
-- binary, where dozens of worker threads and property runs can be in flight
-- at once. It's reliable in a quiet process, which is what this is.
--
-- __All three scenarios live in a single @it@, deliberately__: tasty runs
-- sibling tests concurrently by default, and every scenario here shares one
-- process-global census ('Hegel.Internal.DataSource.currentLiveStringGenerators')
-- — as separate tests they raced (the \"retained\" scenario's 200 live
-- handles would get counted mid-settle by the \"reclaim\" scenario, since
-- both are reading\/writing the same global counter at once). One @it@ makes
-- every scenario run strictly in sequence, by construction, regardless of
-- any @-j@\/threading setting.
module Main (main) where

#ifdef HEGEL_CENSUS
import Control.Monad (forM, forM_, void)
import Data.Text (Text)
import Data.Text qualified as T
import Foreign.ForeignPtr (finalizeForeignPtr)
import Hegel.Internal.DataSource (buildRegexGen, currentLiveStringGenerators, settleStringGenerators)
#endif
import Test.Hspec
import Test.Tasty (defaultMain, localOption)
import Test.Tasty.Hspec (TreatPendingAs (..), testSpec)

spec :: Spec
#ifdef HEGEL_CENSUS
spec = describe "string-generator handle census" do
  it "reclaims transient handles but not retained ones, and the finalizer runs deterministically on demand" do
    -- Scenario 1: nothing retains these — they should settle back down.
    before1 <- currentLiveStringGenerators
    forM_ [1 .. handleCount] \i -> void (buildRegexGen (patternFor i) False Nothing)
    after1 <- settleStringGenerators
    after1 `shouldSatisfy` (<= before1)

    -- Scenario 2: `handles` is used again below, so nothing here is
    -- eligible for collection before the assertion runs — these should
    -- \*not* settle back down.
    before2 <- currentLiveStringGenerators
    handles <- forM [1 .. handleCount] \i -> buildRegexGen (patternFor i) False Nothing
    after2 <- settleStringGenerators
    after2 `shouldSatisfy` (>= before2 + handleCount)
    length handles `shouldBe` handleCount

    -- Scenario 3: unlike the two above, this doesn't depend on GC scheduling.
    -- 'finalizeForeignPtr' runs the finalizer synchronously, so the count
    -- drops immediately, not eventually.
    before3 <- currentLiveStringGenerators
    fp <- buildRegexGen (patternFor 0) False Nothing
    afterBuild <- currentLiveStringGenerators
    afterBuild `shouldBe` before3 + 1
    finalizeForeignPtr fp
    after3 <- currentLiveStringGenerators
    after3 `shouldBe` before3
#else
spec = describe "string-generator handle census" do
  it "reclaims transient handles but not retained ones, and the finalizer runs deterministically on demand" $
    pendingWith "requires the census cabal flag (cabal test --flag census)"
#endif

#ifdef HEGEL_CENSUS
-- | A regex pattern matching a run of lowercase letters, varied by @i@ so
-- each build is a distinct 'Hegel.Internal.DataSource.buildRegexGen' call.
patternFor :: Int -> Text
patternFor i = "[a-" <> T.singleton (toEnum (fromEnum 'a' + (i `mod` 26))) <> "]+"

-- | How many handles each GC-settle scenario above builds.
handleCount :: Int
handleCount = 200
#endif

-- | @tasty-hspec@ has no native concept of a pending test and defaults to
-- reporting one as a failure. Treat it as a success instead, since every
-- case here is 'pendingWith' when the library is built without the
-- @census@ flag.
main :: IO ()
main = defaultMain . localOption TreatPendingAsSuccess =<< testSpec "zizek:string-gen-handles" spec
