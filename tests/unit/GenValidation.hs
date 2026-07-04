-- | Coverage for builder-level validation: 'Hegel.Gen.Builder's checkers
-- ('checkOrdered'\/'checkOrderedMaybe'\/'checkNonNegative'\/'checkSizeBounds')
-- directly, plus a sample of the 'Hegel.Gen.Builder.Build' instances that
-- call them, confirming a misconfigured builder raises 'GenValidationError'
-- at draw rather than wrapping silently or deferring to an opaque engine
-- 'HegelError'.
module GenValidation (spec) where

import Data.Function ((&))
import Data.Text qualified as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..), midnight)
import Hegel (prop)
import Hegel.Gen qualified as Gen
import Hegel.Gen.Builder (GenValidationError (..), checkNonNegative, checkOrdered, checkOrderedMaybe, checkSizeBounds)
import Hegel.Report (PropertyFailed (..))
import Test.Hspec

-- | Does this 'PropertyFailed's message contain @needle@?
--
-- Builder misuse reaches 'prop' as a failing property whose message is the
-- checker's 'displayException', so the 'Gen'-level tests match on that.
messageContains :: T.Text -> PropertyFailed -> Bool
messageContains needle PropertyFailed {message} = needle `T.isInfixOf` message

spec :: Spec
spec = do
  describe "Hegel.Gen.Builder checkers" $ do
    describe "checkOrdered" $ do
      it "passes when lo <= hi" $ do
        checkOrdered "Test" (1 :: Int) 2

      it "passes when lo == hi" $ do
        checkOrdered "Test" (1 :: Int) 1

      it "throws GenValidationError when lo > hi" $ do
        checkOrdered "Test.context" (2 :: Int) 1
          `shouldThrow` \GenValidationError {context = ctx} -> ctx == "Test.context"

    describe "checkOrderedMaybe" $ do
      it "passes when either bound is absent" $ do
        checkOrderedMaybe "Test" (Nothing :: Maybe Int) (Just 1)
        checkOrderedMaybe "Test" (Just (1 :: Int)) Nothing
        checkOrderedMaybe "Test" (Nothing :: Maybe Int) Nothing

      it "throws only when both bounds are present and inverted" $ do
        checkOrderedMaybe "Test" (Just (2 :: Int)) (Just 1) `shouldThrow` \GenValidationError {} -> True

    describe "checkNonNegative" $ do
      it "passes on zero and positive values" $ do
        checkNonNegative "Test" (0 :: Int)
        checkNonNegative "Test" (5 :: Int)

      it "throws GenValidationError on a negative value" $ do
        checkNonNegative "Test.context" (-1 :: Int)
          `shouldThrow` \GenValidationError {context = ctx} -> ctx == "Test.context"

    describe "checkSizeBounds" $ do
      it "passes a valid minSize/maxSize pair" $ do
        checkSizeBounds "Test" 1 (Just 10)

      it "throws on a negative minSize even with no maxSize set" $ do
        -- With no maxSize there is no ordering check to catch a negative
        -- minSize as a side effect, so non-negativity is checked directly.
        checkSizeBounds "Test" (-1) Nothing `shouldThrow` \GenValidationError {} -> True

      it "throws on a negative maxSize" $ do
        checkSizeBounds "Test" 0 (Just (-1)) `shouldThrow` \GenValidationError {} -> True

      it "throws on an inverted pair even when both are non-negative" $ do
        checkSizeBounds "Test" 10 (Just 5) `shouldThrow` \GenValidationError {} -> True

  describe "Gen validation wiring" $ do
    describe "Gen.text" $ do
      it "rejects a negative minSize" $ do
        prop (Gen.text & Gen.minSize (-1) & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.Text"

      it "rejects an inverted minSize/maxSize" $ do
        prop (Gen.text & Gen.minSize 10 & Gen.maxSize 5 & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.Text"

    describe "Gen.binary" $ do
      it "rejects a negative minSize" $ do
        prop (Gen.binary & Gen.minSize (-1) & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.Binary"

    describe "Gen.list" $ do
      it "rejects an inverted minSize/maxSize" $ do
        prop (Gen.list (Gen.bool & Gen.build) & Gen.minSize 10 & Gen.maxSize 5 & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.List"

    describe "Gen.map" $ do
      it "rejects a negative minSize" $ do
        prop
          (Gen.map (Gen.bool & Gen.build) (Gen.bool & Gen.build) & Gen.minSize (-1) & Gen.build)
          (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.Map"

    describe "Gen.integral" $ do
      it "rejects an inverted min/max" $ do
        prop (Gen.int & Gen.min 10 & Gen.max 5 & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.Integer"

    describe "Gen.weighted" $ do
      it "rejects a probability above 1" $ do
        prop (Gen.bool & Gen.weighted 1.5 & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.Bool"

      it "rejects a probability below 0" $ do
        prop (Gen.bool & Gen.weighted (-0.1) & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.Bool"

      it "rejects a NaN probability" $ do
        prop (Gen.bool & Gen.weighted (0 / 0) & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.Bool"

    describe "Gen.char" $ do
      it "rejects an inverted minCodepoint/maxCodepoint" $ do
        prop (Gen.char & Gen.minCodepoint 122 & Gen.maxCodepoint 97 & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.Char"

      it "rejects a negative minCodepoint" $ do
        prop (Gen.char & Gen.minCodepoint (-1) & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.Char"

    describe "Gen.uuid" $ do
      it "rejects a version above the nibble range" $ do
        prop (Gen.uuid & Gen.version 16 & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.Uuid"

    describe "Gen.domain" $ do
      it "rejects a maxLength below the engine's floor" $ do
        prop (Gen.domain & Gen.maxLength 3 & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.Domain"

      it "rejects a maxLength above RFC 1035's ceiling" $ do
        prop (Gen.domain & Gen.maxLength 256 & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.Domain"

    describe "Gen.double" $ do
      it "rejects an exclusive bound that empties the range" $ do
        prop (Gen.double & Gen.min 1 & Gen.max 1 & Gen.exclusiveMin & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.Float"

      it "rejects a NaN bound" $ do
        prop (Gen.double & Gen.min (0 / 0) & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.Float"

    describe "Gen.date" $ do
      it "rejects an inverted min/max" $ do
        prop (Gen.date & Gen.min (fromGregorian 2000 1 2) & Gen.max (fromGregorian 2000 1 1) & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.Date"

      it "rejects a year above libhegel's representable range" $ do
        prop (Gen.date & Gen.min (fromGregorian 1000000 1 1) & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.Date"

      it "rejects a year below libhegel's representable range" $ do
        prop (Gen.date & Gen.max (fromGregorian (-1000000) 1 1) & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.Date"

      it "rejects a minYear above libhegel's representable range" $ do
        prop (Gen.date & Gen.minYear 1000000 & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.Date"

    describe "Gen.time" $ do
      it "rejects an inverted min/max" $ do
        prop (Gen.time & Gen.min (TimeOfDay 12 0 0) & Gen.max (TimeOfDay 6 0 0) & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.Time"

      it "rejects an hour above 23" $ do
        prop (Gen.time & Gen.max (TimeOfDay 24 0 0) & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.Time"

      it "rejects a minute above 59" $ do
        prop (Gen.time & Gen.max (TimeOfDay 23 60 0) & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.Time"

      it "rejects a leap-second bound" $ do
        prop (Gen.time & Gen.max (TimeOfDay 23 59 60) & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.Time"

      it "rejects a bound finer than microsecond resolution" $ do
        prop (Gen.time & Gen.max (TimeOfDay 0 0 0.0000005) & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.Time"

    describe "Gen.datetime" $ do
      it "rejects an inverted min/max" $ do
        let lo = LocalTime (fromGregorian 2000 1 2) midnight
            hi = LocalTime (fromGregorian 2000 1 1) midnight
        prop (Gen.datetime & Gen.min lo & Gen.max hi & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.DateTime"

      it "rejects a year outside libhegel's representable range, attributed to Gen.DateTime" $ do
        let hi = LocalTime (fromGregorian 1000000 1 1) midnight
        prop (Gen.datetime & Gen.max hi & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.DateTime"

      it "rejects an out-of-range time-of-day field, attributed to Gen.DateTime" $ do
        let hi = LocalTime (fromGregorian 2000 1 1) (TimeOfDay 24 0 0)
        prop (Gen.datetime & Gen.max hi & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.DateTime"

    describe "Gen.duration" $ do
      it "rejects an inverted min/max" $ do
        prop (Gen.duration & Gen.min 20 & Gen.max 10 & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.Duration"

      it "rejects a negative min" $ do
        prop (Gen.duration & Gen.min (-1) & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.Duration"
