module ConformanceUtils
  ( aesonOpts,
    camelToSnake,
    decodeArgs,
    getTestCases,
    nonBasic,
    runConformanceProperty,
    runConformancePropertyExpectFailures,
    runConformancePropertyPaired,
    writeEmptyMetrics,
    writeMetrics,
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (SomeException, catch, finally, throwIO)
import Control.Monad (unless)
import Data.Aeson (FromJSON, Options (..), ToJSON, defaultOptions, eitherDecodeStrict', encode, object, (.=))
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.Char (isUpper, toLower)
import Data.Set qualified as Set
import Hegel (Gen)
import Hegel.Assertion (originOf)
import Hegel.Report (Abort (..), Report (..), Result (..))
import Hegel.Runner qualified as Native
import Hegel.Settings (Settings (..), defaultSettings)
import System.Environment (getArgs, getProgName, lookupEnv)
import System.Exit (ExitCode (..), die, exitSuccess, exitWith)
import System.IO (Handle, IOMode (..), hFlush, openFile)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)
import UnliftIO.IORef (modifyIORef', newIORef, readIORef, writeIORef)

_metricsHandle :: MVar (Maybe Handle)
_metricsHandle = unsafePerformIO (newMVar Nothing)
{-# NOINLINE _metricsHandle #-}

getMetricsHandle :: IO (Maybe Handle)
getMetricsHandle = modifyMVar _metricsHandle $ \case
  Just h -> pure (Just h, Just h)
  Nothing ->
    lookupEnv "CONFORMANCE_METRICS_FILE" >>= \case
      Nothing -> pure (Nothing, Nothing)
      Just f -> do
        h <- openFile f AppendMode
        pure (Just h, Just h)

camelToSnake :: String -> String
camelToSnake [] = []
camelToSnake (c : cs) = toLower c : go cs
  where
    go [] = []
    go (x : xs)
      | isUpper x = '_' : toLower x : go xs
      | otherwise = x : go xs

-- | Standard Aeson options for conformance test @Params@/@Metrics@ records:
-- maps Haskell camelCase field names to the snake_case keys the Python harness
-- emits and consumes.
aesonOpts :: Options
aesonOpts = defaultOptions {fieldLabelModifier = camelToSnake}

-- | Parse the single JSON argument every conformance binary takes from the
-- Python harness. Dies with a uniform @Usage:@ message (using the program
-- name) if invoked with the wrong number of arguments or with an undecodable
-- payload.
decodeArgs :: forall p. (FromJSON p) => IO p
decodeArgs = do
  args <- getArgs
  j <- case args of
    [j] -> pure j
    _ -> do
      prog <- getProgName
      die ("Usage: " <> prog <> " '<json_params>'")
  either die pure (eitherDecodeStrict' @p (BS8.pack j))

getTestCases :: IO Int
getTestCases = do
  mv <- lookupEnv "CONFORMANCE_TEST_CASES"
  case mv of
    Nothing -> pure 50
    Just s -> case readMaybe s of
      Just n -> pure n
      Nothing -> ioError (userError ("CONFORMANCE_TEST_CASES: not an integer: " <> s))

writeMetrics :: (ToJSON a) => a -> IO ()
writeMetrics v = do
  mh <- getMetricsHandle
  case mh of
    Nothing -> pure ()
    Just h -> do
      BL.hPut h (encode v <> "\n")
      hFlush h

-- | No-op: previously appended an empty JSON object for server metric pairing.
-- Retained for call-site compatibility; the native backend has no such pairing step.
writeEmptyMetrics :: IO ()
writeEmptyMetrics = pure ()

-- | Force the generator into the compositional (non-basic) fallback path by
-- wrapping it with a trivial monadic bind when @mode == "non_basic"@.
--
-- This exercises the multi-request span path instead of the single-request
-- CBOR schema path.
--
-- See the Rust analogue: @hegel_conformance::maybe_non_basic@.
nonBasic :: String -> Gen a -> Gen a
nonBasic "non_basic" g = g >>= pure
nonBasic _ g = g

-- | Standard conformance runner: exits non-zero on any non-passing outcome.
runConformanceProperty :: (Show a) => Gen a -> (a -> IO ()) -> IO ()
runConformanceProperty gen body = run `finally` pure ()
  where
    run = do
      n <- getTestCases
      report <- Native.runProperty (defaultSettings {testCases = n}) gen body
      case report.result of
        Ok -> exitSuccess
        GaveUp msg -> do
          putStrLn ("conformance property rejected: " <> show msg)
          exitWith (ExitFailure 1)
        Counterexample {} -> do
          putStrLn "conformance property failed"
          exitWith (ExitFailure 1)
        Aborted (Errored e) -> do
          putStrLn ("conformance property errored: " <> show e)
          exitWith (ExitFailure 1)
        Aborted (UnhealthyInput msg) -> do
          putStrLn ("conformance health check failed: " <> show msg)
          exitWith (ExitFailure 1)

-- | Conformance runner that guarantees a client metric line per test case,
-- even when the generator rejects. The body returns @Just m@ to emit @m@ as
-- the case's metric, or @Nothing@ to skip.
runConformancePropertyPaired ::
  forall a m.
  (Show a, ToJSON m) =>
  Gen a ->
  (a -> Maybe m) ->
  IO ()
runConformancePropertyPaired gen toMetric =
  run `finally` pure ()
  where
    run = do
      n <- getTestCases
      wroteRef <- newIORef False
      let body v = case toMetric v of
            Just m -> writeMetrics m *> writeIORef wroteRef True
            Nothing -> pure ()
          finalizer = do
            wrote <- readIORef wroteRef
            unless wrote writeEmptyMetrics
            writeIORef wroteRef False
      report <-
        Native.runProperty
          (defaultSettings {testCases = n, perCaseFinalizer = finalizer})
          gen
          body
      case report.result of
        Ok -> exitSuccess
        GaveUp msg -> do
          putStrLn ("conformance property rejected: " <> show msg)
          exitWith (ExitFailure 1)
        Counterexample {} -> do
          putStrLn "conformance property failed"
          exitWith (ExitFailure 1)
        Aborted (Errored e) -> do
          putStrLn ("conformance property errored: " <> show e)
          exitWith (ExitFailure 1)
        Aborted (UnhealthyInput msg) -> do
          putStrLn ("conformance health check failed: " <> show msg)
          exitWith (ExitFailure 1)

-- | Variant of 'runConformanceProperty' for tests whose property is *expected*
-- to find failures (the Python harness inspects the interesting test-case
-- count, not this binary's exit status).
--
-- Counts distinct failure origins via 'originOf' and writes
-- @{\"interesting_test_cases\": N}@ to @CONFORMANCE_SERVER_RUN_METRICS_FILE@.
-- This is exactly the dedup key the runner reports to libhegel, so the count
-- matches the engine's.
--
-- Exits zero on 'Ok', 'Counterexample', or 'GaveUp'; only 'Errored' or
-- 'UnhealthyInput' (binary-level breakage) propagate as a non-zero exit.
runConformancePropertyExpectFailures :: (Show a) => Gen a -> (a -> IO ()) -> IO ()
runConformancePropertyExpectFailures gen body = run `finally` pure ()
  where
    run = do
      n <- getTestCases
      let settings = defaultSettings {testCases = n}
      seen <- newIORef Set.empty
      let body' x =
            body x `catch` \(e :: SomeException) -> do
              modifyIORef' seen (Set.insert (originOf e))
              throwIO e
      report <- Native.runProperty settings gen body'
      distinct <- Set.size <$> readIORef seen
      writeNativeRunMetrics distinct
      case report.result of
        Ok -> exitSuccess
        Counterexample {} -> exitSuccess
        GaveUp _ -> exitSuccess
        Aborted (Errored e) -> do
          putStrLn ("conformance property errored: " <> show e)
          exitWith (ExitFailure 1)
        Aborted (UnhealthyInput msg) -> do
          putStrLn ("conformance health check failed: " <> show msg)
          exitWith (ExitFailure 1)

-- | Write @{\"interesting_test_cases\": N}@ to @CONFORMANCE_SERVER_RUN_METRICS_FILE@.
-- No-op when the env var is absent.
writeNativeRunMetrics :: Int -> IO ()
writeNativeRunMetrics n =
  lookupEnv "CONFORMANCE_SERVER_RUN_METRICS_FILE" >>= \case
    Nothing -> pure ()
    Just path -> do
      h <- openFile path WriteMode
      BL.hPut h (encode (object ["interesting_test_cases" .= n]) <> "\n")
      hFlush h
