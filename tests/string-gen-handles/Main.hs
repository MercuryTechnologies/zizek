-- | Regression coverage for 'Hegel.Internal.DataSource'\'s string-generator
-- handle census: transient handles should be GC-reclaimed, and retained ones
-- should not be. This is the assertable half of the same question
-- @tests/profile/Main.hs@'s @strgen-reclaim@ scenario answers by printing a
-- diagnostic for a human to eyeball — this spec fails if a real regression
-- shows up, rather than relying on someone watching the profiling output.
--
-- __Isolated in its own test-suite, deliberately__ (see
-- 'Hegel.Internal.DataSource.settleStringGenerators'\'s haddock): the same
-- assertion is flaky when run inside the shared, hundreds-of-tests @unit@
-- binary, where dozens of worker threads and property runs can be in flight
-- at once. It's reliable in a quiet process, which is what this is.
--
-- __Both scenarios live in a single @it@, deliberately__: tasty runs sibling
-- tests concurrently by default, and these two share one process-global
-- census ('Hegel.Internal.DataSource.currentLiveStringGenerators') — as two
-- separate tests they raced (the \"retained\" scenario's 200 live handles
-- would get counted mid-settle by the \"reclaim\" scenario, since both are
-- reading\/writing the same global counter at once). One @it@ makes the two
-- scenarios run strictly in sequence, by construction, regardless of any
-- @-j@\/threading setting.
module Main (main) where

import Control.Monad (forM, forM_, void)
import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Internal.DataSource (buildRegexGen, currentLiveStringGenerators, settleStringGenerators)
import Test.Hspec
import Test.Tasty (defaultMain)
import Test.Tasty.Hspec (testSpec)

-- | A regex pattern matching a run of lowercase letters, varied by @i@ so
-- each build is a distinct 'Hegel.Internal.DataSource.buildRegexGen' call.
patternFor :: Int -> Text
patternFor i = "[a-" <> T.singleton (toEnum (fromEnum 'a' + (i `mod` 26))) <> "]+"

-- | How many handles each scenario below builds.
handleCount :: Int
handleCount = 200

spec :: Spec
spec = describe "string-generator handle census" do
  it "reclaims transient handles but not retained ones" do
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

main :: IO ()
main = defaultMain =<< testSpec "zizek:string-gen-handles" spec
