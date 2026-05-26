module Common
  ( camelToSnake,
    getTestCases,
    nonBasic,
    runConformanceProperty,
    writeMetrics,
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy qualified as BL
import Data.Char (isUpper, toLower)
import Hegel.Generators (Generator)
import Hegel.Outcome (Outcome (..))
import Hegel.Runner (Settings (..), defaultSettings, runPropertyWith)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.IO (Handle, IOMode (..), hFlush, openFile)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)

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

-- | Force the generator into the compositional (non-basic) fallback path by
-- wrapping it with a trivial monadic bind when @mode == "non_basic"@. This
-- makes 'Hegel.Generators.asBasic' return 'Nothing', exercising the
-- multi-request span path instead of the single-request CBOR schema path.
-- See the Rust analogue: @hegel_conformance::maybe_non_basic@.
nonBasic :: String -> Generator a -> Generator a
nonBasic "non_basic" g = g >>= pure
nonBasic _ g = g

runConformanceProperty :: Generator a -> (a -> IO ()) -> IO ()
runConformanceProperty gen body = do
  n <- getTestCases
  outcome <- runPropertyWith (defaultSettings {testCases = n}) gen body
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
