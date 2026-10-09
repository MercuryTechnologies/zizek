-- | A gallery of deliberately failing properties, one per report shape the
-- failure renderers draw.
--
-- Run it with @just gallery@ from the repository root, since source splicing
-- reads each declaration from a path relative to the working directory. Every
-- scenario renders through 'renderReportAuto', the path real failures take.
--
-- @--check@ renders nothing and instead confirms that each scenario's report
-- still has the shape it pins, exiting nonzero if any has drifted. @--sweep N@
-- reruns the seeded scenarios under @N@ fresh seeds and prints how often each
-- failed and how often it produced its pinned shape. Naming scenarios after
-- either flag, or alone, limits the run to those scenarios.
module Main (main) where

import Control.Monad (forM, forM_, unless, when)
import Data.List (isPrefixOf)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Gallery.Bank qualified as Bank
import Gallery.Codec qualified as Codec
import Gallery.Library qualified as Library
import Gallery.Locales qualified as Locales
import Gallery.Scenario (Scenario (..))
import Gallery.Upload qualified as Upload
import Gallery.Warehouse qualified as Warehouse
import Hegel.Report (Report (..), Result (..), renderReportAuto, renderReportRichAnsiWith)
import Hegel.Report.Style (defaultStyle)
import Hegel.Report.Style qualified as Style
import Hegel.Runner (check)
import Hegel.Settings (Settings (..))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr, stdout)
import Text.Read (readMaybe)
import UnliftIO.Directory (doesFileExist, removePathForcibly)

scenarios :: [Scenario]
scenarios = [Codec.scenario, Warehouse.scenario, Library.scenario, Locales.scenario, Upload.scenario, Bank.scenario]

main :: IO ()
main = do
  args <- getArgs
  requireRepositoryRoot
  -- A failure stored by an earlier run would be replayed instead of found
  -- afresh, which changes the report's statistics line.
  removePathForcibly ".hegel/gallery"
  case args of
    "--check" : names -> checkAll =<< select names
    "--sweep" : n : names | Just runs <- readMaybe n, runs > 0 -> mapM_ (sweep runs) =<< select names
    names | not (any ("--" `isPrefixOf`) names) -> mapM_ render =<< select names
    _ -> usage

usage :: IO a
usage = do
  T.hPutStrLn stderr ("usage: gallery [--check | --sweep N] [SCENARIO...]\nscenarios: " <> T.unwords (map (.name) scenarios))
  exitFailure

-- | The scenarios with the given names, or every scenario when none are given.
select :: [String] -> IO [Scenario]
select [] = pure scenarios
select names = case filter (`notElem` map (T.unpack . (.name)) scenarios) names of
  [] -> pure [s | s <- scenarios, T.unpack s.name `elem` names]
  _ -> usage

-- | Exit unless a scenario's source is readable from the working directory,
-- since every splice would otherwise fall back to bare journal lines.
requireRepositoryRoot :: IO ()
requireRepositoryRoot = do
  found <- doesFileExist "examples/gallery/Gallery/Library.hs"
  unless found do
    T.hPutStrLn stderr "gallery: run from the repository root so source splicing can find its files"
    exitFailure

render :: Scenario -> IO ()
render s = do
  report <- check s.settings s.property
  pref <- Style.preference stdout
  T.putStrLn (Style.cleanFor pref ("\n━━━━━ " <> s.name <> ": " <> s.title <> " ━━━━━"))
  T.putStrLn =<< renderReportAuto True pref report
  when s.ascii do
    T.putStrLn "-- ascii --"
    T.putStrLn . Style.sevenBitClean =<< renderReportRichAnsiWith (defaultStyle Style.ascii) report

checkAll :: [Scenario] -> IO ()
checkAll selected = do
  results <- forM selected \s -> do
    start <- getCurrentTime
    mismatches <- firstMatch s.attempts s
    elapsed <- (`diffUTCTime` start) <$> getCurrentTime
    let timing = "  (" <> T.pack (show elapsed) <> ")"
    if null mismatches
      then T.putStrLn ("ok    " <> s.name <> timing)
      else do
        T.putStrLn ("FAIL  " <> s.name <> timing)
        forM_ mismatches \m -> T.putStrLn ("        " <> m)
    pure (null mismatches)
  unless (and results) exitFailure

-- | Run a scenario up to @n@ times, stopping at the first report that has its
-- pinned shape, and return the last run's mismatches.
firstMatch :: Int -> Scenario -> IO [Text]
firstMatch n s = do
  mismatches <- s.expect =<< check s.settings s.property
  if null mismatches || n <= 1 then pure mismatches else firstMatch (n - 1) s

-- | Rerun a seeded scenario under fresh seeds, clearing the example database
-- before each run, and print how often it failed and how often it matched its
-- pinned shape.
sweep :: Int -> Scenario -> IO ()
sweep runs s = when (isJust s.settings.seed) do
  let settings = s.settings {seed = Nothing}
  outcomes <- forM [1 .. runs] \_ -> do
    removePathForcibly ".hegel/gallery"
    report <- check settings s.property
    mismatches <- s.expect report
    pure (isFailure report.result, null mismatches)
  let count p = T.pack (show (length (filter p outcomes)))
      total = T.pack (show runs)
  T.putStrLn (s.name <> ": failed " <> count fst <> "/" <> total <> ", pinned shape " <> count snd <> "/" <> total)
  where
    isFailure = \case
      Failures _ -> True
      _ -> False
