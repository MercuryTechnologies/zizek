-- | Coverage for builder-level validation: 'Hegel.Gen.Builder's checkers
-- ('checkOrdered'\/'checkOrderedMaybe'\/'checkNonNegative'\/'checkSizeBounds')
-- directly, plus a sample of the 'Hegel.Gen.Builder.Build' instances that
-- call them, confirming a misconfigured builder raises 'ValidationError'
-- at draw rather than wrapping silently or deferring to an opaque engine
-- 'HegelError'.
module GenValidation (spec) where

import Control.Exception (evaluate, fromException, throwIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_, traverse_)
import Data.Function ((&))
import Data.IORef
import Data.Text qualified as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..), midnight)
import GHC.Stack (SrcLoc (..), getCallStack)
import Hegel (prop)
import Hegel.Exception (Diagnostic (..), HegelError (..), MalformedTest (..), SettingsError (..))
import Hegel.Gen qualified as Gen
import Hegel.Gen.Builder (ValidationError (..), checkNonNegative, checkOrdered, checkOrderedMaybe, checkSizeBounds)
import Hegel.Property qualified as Property
import Hegel.Property.Branch qualified as Branch
import Hegel.Property.Fork qualified as Fork
import Hegel.Report (Abort (..), FailureEvidence (..), FailureOutcome (..), PropertyFailed (..), Report (..), Result (..))
import Hegel.Runner qualified as Runner
import Hegel.Settings (Settings (..), defaultSettings)
import Hegel.Settings qualified as Settings
import Hegel.Stateful.Concurrent qualified as Concurrent
import Test.Hspec
import TestSupport (allFailureOutcomes, expectReconstructed, expectToken, failureMessages, forRenderers)

-- | Does this 'PropertyFailed's message contain @needle@?
--
-- Builder misuse reaches 'prop' as a failing property whose message is the
-- checker's 'displayException', so the 'Gen'-level tests match on that.
messageContains :: T.Text -> PropertyFailed -> Bool
messageContains needle PropertyFailed {report = Report {result}} = any (T.isInfixOf needle) (failureMessages result)

spec :: Spec
spec = do
  describe "validation classification" do
    for_
      [ ("char", void (Gen.char & Gen.minCodepoint (-1) & Gen.build)),
        ("text alphabet", void (Gen.text & Gen.alphabet (Gen.char & Gen.minCodepoint (-1)) & Gen.build)),
        ("regex alphabet", void (Gen.regex "." & Gen.alphabet (Gen.char & Gen.minCodepoint (-1)) & Gen.build))
      ]
      \(name, gen) ->
        it ("preserves the build call site for an invalid " <> name) do
          Runner.sample defaultSettings gen `shouldThrow` \(ValidationError d) ->
            d.values == [("minCodepoint", "-1")]
              && case getCallStack d.callStack of
                (operation, loc) : _ -> operation == "build" && loc.srcLocFile == "tests/unit/GenValidation.hs"
                _ -> False

    for_
      [ ("maxCodepoint", void (Gen.char & Gen.maxCodepoint (-1) & Gen.build)),
        ("minSize", void (Gen.text & Gen.minSize (-1) & Gen.build)),
        ("maxSize", void (Gen.binary & Gen.maxSize (-1) & Gen.build)),
        ("maxDepth", void (Gen.recursive (pure ()) (\_ _ -> pure ()) & Gen.maxDepth (-1) & Gen.build)),
        ("maxLeaves", void (Gen.recursive (pure ()) (\_ _ -> pure ()) & Gen.maxLeaves (-1) & Gen.build))
      ]
      \(field, gen) ->
        it ("names the invalid " <> T.unpack field <> " field") do
          Runner.sample defaultSettings gen `shouldThrow` \(ValidationError d) ->
            d.values == [(field, "-1")] && d.detail == field <> " must be nonnegative"

    it "shrinks generated invalid bounds and replays the smallest failing input" do
      let body :: Property.Property ()
          body = do
            n <- Property.forAll (Gen.int & Gen.min 0 & Gen.max 50 & Gen.build)
            void (Property.forAll (Gen.int & Gen.min n & Gen.max 0 & Gen.build))
      report <- Property.check defaultSettings {seed = Just 42} body
      evidence <- expectReconstructed report.result
      evidence.message `shouldSatisfy` T.isInfixOf "min = 1"
      fmap (.srcLocFile) evidence.loc `shouldBe` Just "tests/unit/GenValidation.hs"
      case allFailureOutcomes report.result of
        [outcome] -> do
          outcome.failureOrigin `shouldSatisfy` T.isInfixOf "ValidationError Hegel.Gen.Integer"
          token <- expectToken outcome
          replayed <- Runner.replay defaultSettings token body
          actual <- expectReconstructed replayed.result
          actual.message `shouldBe` evidence.message
        other -> expectationFailure (show other)
      forRenderers report \rendered -> do
        rendered `shouldSatisfy` T.isInfixOf "ValidationError"
        rendered `shouldSatisfy` T.isInfixOf "GenValidation.hs"

    for_ [("oneOf", Gen.oneOf @Int []), ("element", Gen.element @Int []), ("frequency", Gen.frequency @Int [])] \(name, gen) ->
      it ("reports typed empty " <> name <> " diagnostics in sampling") do
        Runner.sample defaultSettings gen `shouldThrow` \(ValidationError d) ->
          d.context == "Gen." <> T.pack name
            && d.values == [("choices", "0")]
            && case getCallStack d.callStack of
              (_, loc) : _ -> loc.srcLocFile == "tests/unit/GenValidation.hs"
              _ -> False

    it "captures the build call site through a deferred sampling draw" do
      let gen = Gen.int & Gen.min 2 & Gen.max 1 & Gen.build
      Runner.sample defaultSettings gen `shouldThrow` \(ValidationError d) ->
        case getCallStack d.callStack of
          (name, loc) : _ -> name == "build" && loc.srcLocFile == "tests/unit/GenValidation.hs"
          _ -> False

    it "renders live concurrent validation failures with their user location" do
      let machine =
            Concurrent.Machine
              { initial = pure (),
                rules = [Concurrent.rule "invalid draw" (\() -> void (Property.forAll (Gen.int & Gen.min 2 & Gen.max 1 & Gen.build)))],
                invariants = []
              }
      report <- Property.check defaultSettings (Concurrent.run (Concurrent.fixed 2) machine)
      forRenderers report \rendered -> do
        rendered `shouldSatisfy` T.isInfixOf "ValidationError"
        rendered `shouldSatisfy` T.isInfixOf "Hegel.Gen.Integer"
        rendered `shouldSatisfy` T.isInfixOf "GenValidation.hs"

    for_ [("direct", id), ("branch", \body -> Branch.concurrently_ (Property.failure "sibling") body), ("fork", \body -> Fork.spawn body >>= Fork.join)] \(name, wrap) ->
      it ("aborts a body-level engine error through " <> name <> " and drains cleanup") do
        cleaned <- newIORef False
        report <- Property.check defaultSettings $ wrap do
          Property.registerFinalizer (writeIORef cleaned True)
          liftIO (throwIO HegelError {code = 1, message = Just "body engine error"})
        case report.result of
          Aborted (Errored e) -> case fromException e of
            Just (he :: HegelError) -> he.message `shouldBe` Just "body engine error"
            Nothing -> expectationFailure (show e)
          other -> expectationFailure (show other)
        readIORef cleaned `shouldReturn` True

    it "throws the original settings error from check_" do
      Property.check_ defaultSettings {maxCloneDepth = -1} (pure ()) `shouldThrow` \SettingsError {} -> True

    it "rejects a nonpositive branch cap as a malformed operation" do
      Property.check_ defaultSettings (void (Branch.replicateConcurrentlyBounded 0 1 (pure ()))) `shouldThrow` \(MalformedTest d) -> d.values == [("cap", "0")]

    it "permits zero clone depth for ordinary properties and rejects a fork at its call site" do
      report <- Property.check defaultSettings {maxCloneDepth = 0} (pure ())
      show report.result `shouldBe` "Ok"
      empty <- Property.check defaultSettings {maxCloneDepth = 0} (void (Branch.mapConcurrently pure ([] :: [Int])))
      show empty.result `shouldBe` "Ok"
      forked <- Property.check defaultSettings {maxCloneDepth = 0} (void (Fork.spawn (pure ())))
      case forked.result of
        Aborted (Errored e) -> case fromException e of
          Just (MalformedTest d) -> case getCallStack d.callStack of
            (name, loc) : _ -> do
              name `shouldBe` "spawn"
              loc.srcLocFile `shouldBe` "tests/unit/GenValidation.hs"
            _ -> expectationFailure "missing clone operation location"
          Nothing -> expectationFailure (show e)
        other -> expectationFailure (show other)

    it "validates every numeric settings field before checking, progress, replay, or sampling" do
      baseline <- Property.check defaultSettings (Property.failure "baseline")
      token <- case allFailureOutcomes baseline.result of
        [outcome] -> expectToken outcome
        other -> fail (show other)
      for_ [defaultSettings {testCases = -1}, defaultSettings {statefulStepCount = 0}, defaultSettings {maxCloneDepth = -1}] \settings -> do
        ran <- newIORef False
        let body = liftIO (writeIORef ran True)
        for_ [Property.check settings body, Runner.checkWithProgress (\_ -> writeIORef ran True) settings body, Runner.replay settings token body] \run -> do
          report <- run
          case report.result of
            Aborted (Errored e) -> (case fromException e of Just SettingsError {} -> True; _ -> False) `shouldBe` True
            other -> expectationFailure (show other)
        Runner.sample settings (pure ()) `shouldThrow` \SettingsError {} -> True
        readIORef ran `shouldReturn` False

    it "allows zero test cases and reports no valid examples" do
      report <- Property.check defaultSettings {testCases = 0} (Property.failure "must not run")
      case report.result of
        GaveUp _ -> pure ()
        other -> expectationFailure (show other)

    it "rejects settings before executing the property" do
      ran <- newIORef False
      report <- Property.check defaultSettings {testCases = -1} (liftIO (writeIORef ran True))
      case report.result of
        Aborted (Errored e) -> case fromException e of
          Just (SettingsError d) -> d.values `shouldBe` [("testCases", "-1")]
          Nothing -> expectationFailure (show e)
        other -> expectationFailure (show other)
      readIORef ran `shouldReturn` False
    it "accepts the numeric limits without starting an engine" do
      Settings.validate defaultSettings {testCases = 0, statefulStepCount = 1, maxCloneDepth = 0} `shouldSatisfy` either (const False) (const True)
      Settings.validate defaultSettings {testCases = maxBound, statefulStepCount = maxBound, maxCloneDepth = maxBound} `shouldSatisfy` either (const False) (const True)
    it "validates the effective sample count" do
      Runner.samples defaultSettings {testCases = -1} 0 (pure True) `shouldReturn` []
      Runner.samples defaultSettings (-1) (pure True) `shouldThrow` \SettingsError {} -> True

  describe "Gen.frequency validation" $ do
    it "rejects empty choices at the call site" $ do
      evaluate (Gen.frequency @Int []) `shouldThrow` \(ValidationError d) -> d.context == "Gen.frequency"
    it "rejects zero and negative weights at the call site" $ do
      traverse_
        ( \w ->
            evaluate (Gen.frequency [(1, pure True), (w, pure False)])
              `shouldThrow` \(ValidationError d) -> d.context == "Gen.frequency"
        )
        [0, -1, minBound]

  describe "Hegel.Gen.Builder checkers" $ do
    describe "checkOrdered" $ do
      it "passes when lo <= hi" $ do
        checkOrdered "Test" (1 :: Int) 2

      it "passes when lo == hi" $ do
        checkOrdered "Test" (1 :: Int) 1

      it "throws ValidationError when lo > hi" $ do
        checkOrdered "Test.context" (2 :: Int) 1
          `shouldThrow` \(ValidationError d) -> d.context == "Test.context"

    describe "checkOrderedMaybe" $ do
      it "passes when either bound is absent" $ do
        checkOrderedMaybe "Test" (Nothing :: Maybe Int) (Just 1)
        checkOrderedMaybe "Test" (Just (1 :: Int)) Nothing
        checkOrderedMaybe "Test" (Nothing :: Maybe Int) Nothing

      it "throws only when both bounds are present and inverted" $ do
        checkOrderedMaybe "Test" (Just (2 :: Int)) (Just 1) `shouldThrow` \ValidationError {} -> True

    describe "checkNonNegative" $ do
      it "passes on zero and positive values" $ do
        checkNonNegative "Test" (0 :: Int)
        checkNonNegative "Test" (5 :: Int)

      it "throws ValidationError on a negative value" $ do
        checkNonNegative "Test.context" (-1 :: Int)
          `shouldThrow` \(ValidationError d) -> d.context == "Test.context"

    describe "checkSizeBounds" $ do
      it "passes a valid minSize/maxSize pair" $ do
        checkSizeBounds "Test" 1 (Just 10)

      it "throws on a negative minSize even with no maxSize set" $ do
        -- With no maxSize there is no ordering check to catch a negative
        -- minSize as a side effect, so non-negativity is checked directly.
        checkSizeBounds "Test" (-1) Nothing `shouldThrow` \ValidationError {} -> True

      it "throws on a negative maxSize" $ do
        checkSizeBounds "Test" 0 (Just (-1)) `shouldThrow` \ValidationError {} -> True

      it "throws on an inverted pair even when both are non-negative" $ do
        checkSizeBounds "Test" 10 (Just 5) `shouldThrow` \ValidationError {} -> True

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

    describe "Gen.nonEmpty" $ do
      it "rejects a minSize of 0" $ do
        prop (Gen.nonEmpty (Gen.bool & Gen.build) & Gen.minSize 0 & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.NonEmpty"

      it "rejects a negative minSize" $ do
        prop (Gen.nonEmpty (Gen.bool & Gen.build) & Gen.minSize (-1) & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.NonEmpty"

      it "rejects an inverted minSize/maxSize" $ do
        prop (Gen.nonEmpty (Gen.bool & Gen.build) & Gen.minSize 10 & Gen.maxSize 5 & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.NonEmpty"

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

    describe "Gen.recursive" $ do
      it "rejects a negative maxDepth" $ do
        prop (Gen.recursive (pure True) (\_ctx sub -> sub) & Gen.maxDepth (-1) & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.Recursive"

      it "rejects a negative maxLeaves" $ do
        prop (Gen.recursive (pure True) (\_ctx sub -> sub) & Gen.maxLeaves (-1) & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.Recursive"

    describe "Gen.duration" $ do
      it "rejects an inverted min/max" $ do
        prop (Gen.duration & Gen.min 20 & Gen.max 10 & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.Duration"

      it "rejects a negative min" $ do
        prop (Gen.duration & Gen.min (-1) & Gen.build) (\_ -> pure ())
          `shouldThrow` messageContains "Hegel.Gen.Duration"
