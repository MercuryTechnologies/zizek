-- | Pure tests for the control-signal exception discipline in
-- 'Hegel.Internal.Control': 'isFailure' and 'onFailure'; no engine involved.
module ControlSignals (spec) where

import Control.Exception
  ( Exception (..),
    SomeException,
    asyncExceptionFromException,
    asyncExceptionToException,
    throwIO,
    try,
  )
import Data.IORef (modifyIORef', newIORef, readIORef)
import Hegel.Internal.Control
  ( AssumeRejected (..),
    ControlSignal (..),
    MalformedTest (..),
    TestStopped (..),
    catchControl,
    isFailure,
    onFailure,
  )
import Test.Hspec
import UnliftIO.Exception (isSyncException)

-- | An async-flavored exception that is /not/ one of Hegel's control signals,
-- wrapped via 'asyncExceptionToException' exactly like the control signals
-- are. (A bare @UserInterrupt@ would not do: unliftio's 'isSyncException'
-- only recognizes the 'Control.Exception.SomeAsyncException' wrapper.)
data Interrupt = Interrupt
  deriving stock (Show)

instance Exception Interrupt where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

-- | Run an action, capturing every exception (sync or async) as a
-- 'SomeException' so tests can inspect what escaped.
tryAll :: IO a -> IO (Either SomeException a)
tryAll = try

-- | 'onFailure' with a hook that records the exceptions it sees; returns the
-- escaped exception (the action is expected to throw) and the hook's log.
observeHook :: IO () -> IO (SomeException, [SomeException])
observeHook act = do
  seen <- newIORef []
  res <- tryAll (act `onFailure` \e -> modifyIORef' seen (e :))
  hooked <- reverse <$> readIORef seen
  case res of
    Left e -> pure (e, hooked)
    Right () -> fail "observeHook: action did not throw"

spec :: Spec
spec = do
  describe "isFailure" do
    it "is True for an ordinary synchronous exception" do
      isFailure (toException (userError "boom")) `shouldBe` True

    it "is False for the control signals" do
      isFailure (toException AssumeRejected) `shouldBe` False
      isFailure (toException TestStopped) `shouldBe` False

    it "is False for a genuine async exception" do
      isFailure (toException Interrupt) `shouldBe` False

    it "is True for MalformedTest (deliberate: aborted runs never render a journal)" do
      isFailure (toException (MalformedTest "no rules")) `shouldBe` True

  describe "onFailure" do
    it "fires the hook on a failure and still rethrows it" do
      (escaped, hooked) <- observeHook (throwIO (userError "boom"))
      fmap show hooked `shouldBe` [show escaped]
      show escaped `shouldContain` "boom"

    it "does not fire the hook for control signals, which stay catchable by catchControl" do
      (assumeEscaped, assumeHooked) <- observeHook (throwIO AssumeRejected)
      assumeHooked `shouldSatisfy` null
      caught <- (throwIO assumeEscaped >> pure Nothing) `catchControl` (pure . Just)
      caught `shouldBe` Just Assume

      (stopEscaped, stopHooked) <- observeHook (throwIO TestStopped)
      stopHooked `shouldSatisfy` null
      caught' <- (throwIO stopEscaped >> pure Nothing) `catchControl` (pure . Just)
      caught' `shouldBe` Just Stop

    it "does not fire the hook for async exceptions and preserves their flavor" do
      (escaped, hooked) <- observeHook (throwIO Interrupt)
      hooked `shouldSatisfy` null
      isSyncException escaped `shouldBe` False

  describe "onFailure composed inside catchControl" do
    -- The bracket shape Hegel.Stateful.run uses: the hook observes
    -- failures without disturbing the control-signal handling around it.
    let bracketed :: IO () -> (SomeException -> IO ()) -> IO (Either ControlSignal ())
        bracketed act hook =
          (Right <$> (act `onFailure` hook)) `catchControl` (pure . Left)

    it "routes a control signal to catchControl without touching the hook" do
      seen <- newIORef []
      verdict <- bracketed (throwIO AssumeRejected) (\e -> modifyIORef' seen (e :))
      verdict `shouldBe` Left Assume
      hooked <- readIORef seen
      hooked `shouldSatisfy` null

    it "fires the hook on a failure, which then escapes catchControl" do
      seen <- newIORef []
      res <- tryAll (bracketed (throwIO (userError "boom")) (\e -> modifyIORef' seen (e :)))
      case res of
        Left e -> show e `shouldContain` "boom"
        Right v -> expectationFailure ("expected the failure to escape, got: " <> show v)
      hooked <- readIORef seen
      length hooked `shouldBe` 1
