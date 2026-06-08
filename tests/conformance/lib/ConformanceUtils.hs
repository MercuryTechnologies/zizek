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
import Control.Exception (finally)
import Control.Monad (unless)
import Data.Aeson (FromJSON, Options (..), ToJSON, defaultOptions, eitherDecodeStrict', encode, object)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.Char (isUpper, toLower)
import Data.IORef (newIORef, readIORef, writeIORef)
import Hegel (Gen)
import Hegel.Native.Runner qualified as Native
import Hegel.Outcome (Outcome (..))
import Hegel.Server.Runner qualified as Server
import Hegel.Server.Session (closeSession, globalSession)
import Hegel.Settings (Settings (..), defaultSettings)
import System.Environment (getArgs, getProgName, lookupEnv)
import System.Exit (ExitCode (..), die, exitSuccess, exitWith)
import System.IO (Handle, IOMode (..), hFlush, openFile)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)

-- | Which backend the conformance binary should use.
-- Controlled by the @HEGEL_BACKEND@ environment variable:
-- @"native"@ or absent → 'NativeBackend'; @"server"@ → 'ServerBackend'.
data Backend = NativeBackend | ServerBackend

-- | Read 'Backend' from @HEGEL_BACKEND@, defaulting to 'NativeBackend'.
getBackend :: IO Backend
getBackend =
  lookupEnv "HEGEL_BACKEND" >>= \case
    Nothing -> pure NativeBackend
    Just "native" -> pure NativeBackend
    Just "server" -> pure ServerBackend
    Just other -> die ("HEGEL_BACKEND: expected native|server, got: " <> other)

-- | Dispatch to the backend selected by 'getBackend'.
runSelected ::
  Settings ->
  Gen a ->
  (a -> IO ()) ->
  IO (Outcome a)
runSelected settings gen body =
  getBackend >>= \case
    NativeBackend -> Native.runProperty settings gen body
    ServerBackend -> Server.runProperty settings gen body

-- | Close the server session only when running under the server backend.
-- No-op for the native backend.
closeSelectedSession :: IO ()
closeSelectedSession =
  getBackend >>= \case
    NativeBackend -> pure ()
    ServerBackend -> closeSession globalSession

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

-- | Append an empty JSON object to the metrics file, if one is configured.
--
-- Used by tests that need to keep client and server metric line counts paired
-- without emitting per-test-case data.
writeEmptyMetrics :: IO ()
writeEmptyMetrics = writeMetrics (object [])

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
runConformanceProperty :: Gen a -> (a -> IO ()) -> IO ()
runConformanceProperty gen body = run `finally` closeSelectedSession
  where
    run = do
      n <- getTestCases
      outcome <- runSelected (defaultSettings {testCases = n}) gen body
      case outcome of
        Passed _ -> exitSuccess
        Rejected msg -> do
          putStrLn ("conformance property rejected: " <> show msg)
          exitWith (ExitFailure 1)
        Failed {} -> do
          putStrLn "conformance property failed"
          exitWith (ExitFailure 1)
        Errored e -> do
          putStrLn ("conformance property errored: " <> show e)
          exitWith (ExitFailure 1)
        UnhealthyInput msg -> do
          putStrLn ("conformance health check failed: " <> show msg)
          exitWith (ExitFailure 1)

-- | Conformance runner that guarantees a client metric line per test case the
-- server records, even when the generator rejects (e.g. exhausted
-- 'Hegel.Gen.filtered' retries). Mirrors the @defer EnsureMetric()@ pattern
-- from @hegel-go@'s @internal/conformance/helpers.go@.
--
-- The body returns @Just m@ to emit @m@ as the case's metric, or @Nothing@ to
-- skip (rare; usually return @Just@). After each test case completes the
-- finalizer checks whether the body wrote a metric; if not, it appends an
-- empty @{}@ line so the client and server metric files stay 1:1.
runConformancePropertyPaired ::
  forall a m.
  (ToJSON m) =>
  Gen a ->
  (a -> Maybe m) ->
  IO ()
runConformancePropertyPaired gen toMetric =
  run `finally` closeSelectedSession
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
      outcome <-
        runSelected
          (defaultSettings {testCases = n, perCaseFinalizer = finalizer})
          gen
          body
      case outcome of
        Passed _ -> exitSuccess
        Rejected msg -> do
          putStrLn ("conformance property rejected: " <> show msg)
          exitWith (ExitFailure 1)
        Failed {} -> do
          putStrLn "conformance property failed"
          exitWith (ExitFailure 1)
        Errored e -> do
          putStrLn ("conformance property errored: " <> show e)
          exitWith (ExitFailure 1)
        UnhealthyInput msg -> do
          putStrLn ("conformance health check failed: " <> show msg)
          exitWith (ExitFailure 1)

-- | Variant of 'runConformanceProperty' for tests whose property is *expected*
-- to find failures (the Python harness inspects the server's interesting test
-- cases, not this binary's exit status).
--
-- Exits zero on 'Passed', 'Failed', or 'Rejected'; only 'Errored' or
-- 'UnhealthyInput' (binary-level breakage) propagate as a non-zero exit.
runConformancePropertyExpectFailures :: Gen a -> (a -> IO ()) -> IO ()
runConformancePropertyExpectFailures gen body = run `finally` closeSelectedSession
  where
    run = do
      n <- getTestCases
      outcome <- runSelected (defaultSettings {testCases = n}) gen body
      case outcome of
        Passed _ -> exitSuccess
        Failed {} -> exitSuccess
        Rejected _ -> exitSuccess
        Errored e -> do
          putStrLn ("conformance property errored: " <> show e)
          exitWith (ExitFailure 1)
        UnhealthyInput msg -> do
          putStrLn ("conformance health check failed: " <> show msg)
          exitWith (ExitFailure 1)
