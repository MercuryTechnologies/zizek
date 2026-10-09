-- | A caching HTTP client on a fake clock, tested by a state machine whose
-- fixture draws from a 'Slot'.
--
-- The same machine runs with one 'Supply' for the whole run and with a fresh
-- one for every step, to compare how well each shrinks.
module StatefulFixture (spec) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (def)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Traversable (for)
import GHC.Clock (getMonotonicTime)
import Hegel (Gen)
import Hegel.Gen qualified as Gen
import Hegel.Property (Property, assert, check, forAll)
import Hegel.Report (FailureEvidence (..), FailureEvidenceStatus (..), FailureOutcome (..), Note (..), NoteKind (..), Report (..), Result (..), isDrawn, renderReportRich)
import Hegel.Seed (Seed (..))
import Hegel.Settings (Settings (..))
import Hegel.Stateful qualified as Stateful
import Hegel.Supply (Slot)
import Hegel.Supply qualified as Supply
import Support (viable)
import System.Environment (lookupEnv)
import Test.Hspec
import Text.Printf (printf)
import UnliftIO.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)

-- * The system under test

ttl :: Int
ttl = 60

intR :: (Int, Int) -> Gen Int
intR (lo, hi) = Gen.int & Gen.min lo & Gen.max hi & Gen.build

-- | A client that caches each key's upstream version for 'ttl' seconds.
data Client = Client
  { -- | The fake clock, in seconds, advancing by a drawn multiple of 15 on
    -- every read.
    now :: IO Int,
    -- | The upstream service, answering with the key's current version.
    fetch :: Text -> IO Int,
    cache :: IORef (Map Text (Int, Int))
  }

newClient :: Slot -> IO Client
newClient slot = do
  clock <- newIORef 0
  cache <- newIORef Map.empty
  let now = do
        step <- Supply.drawFrom slot "tick" ((* 15) <$> intR (0, 8))
        atomicModifyIORef' clock \t -> (t + step, t + step)
      fetch k = Supply.drawFrom slot ("GET /" <> k) (intR (0, 3))
  pure Client {now, fetch, cache}

-- | Look a key up, returning its version, whether the cache served it, and
-- the cached entry's age.
--
-- The bug: an entry exactly 'ttl' seconds old should have expired, but the
-- cache still serves it.
get :: Client -> Text -> IO (Int, Bool, Int)
get client k = do
  t <- client.now
  Map.lookup k <$> readIORef client.cache >>= \case
    Just (at, v) | t - at <= ttl -> pure (v, True, t - at)
    _ -> do
      v <- client.fetch k
      modifyIORef' client.cache (Map.insert k (t, v))
      pure (v, False, 0)

-- * The machine

-- | How long each 'Supply' behind the fixture's 'Slot' lives.
data Granularity = PerRun | PerStep
  deriving stock (Eq, Show)

-- | The model: each key's version as of its last upstream fetch.
type Model = Map Text Int

cachingProperty :: Granularity -> Property ()
cachingProperty granularity = do
  slot <- Supply.newSlot
  client <- liftIO (newClient slot)
  let perStep = if granularity == PerStep then Supply.withSlot slot else id
      perRun = if granularity == PerRun then Supply.withSlot slot else id
      getRule :: Stateful.Rule Model IO
      getRule = Stateful.rule "get" \model -> perStep do
        k <- forAll (Gen.element ["a", "b"])
        (v, cached, age) <- liftIO (get client k)
        when cached do
          assert (age < ttl) ("served " <> k <> " from a cache entry " <> T.pack (show age) <> "s old")
          assert (Map.lookup k model == Just v) ("served a stale version of " <> k)
        pure (if cached then model else Map.insert k v model)
      machine =
        Stateful.Machine
          { initial = pure Map.empty,
            rules = [getRule],
            invariants = [],
            stepCount = Stateful.defaultStepCount
          }
  perRun (Stateful.run machine)

-- * Measuring a shrunk counterexample

data Shrunk = Shrunk {steps :: Int, supplyDraws :: Int, seconds :: Double}

isSupplyDraw :: Note -> Bool
isSupplyDraw n = isDrawn n.kind && ("tick=" `T.isPrefixOf` n.text || "GET /" `T.isPrefixOf` n.text)

measure :: Settings -> Granularity -> IO Shrunk
measure settings granularity = do
  start <- getMonotonicTime
  report <- check settings (cachingProperty granularity)
  end <- getMonotonicTime
  notes <- case report.result of
    Failures (FailureOutcome {failureEvidence = Captured evidence} :| _) -> pure evidence.notes
    other -> fail ("expected a captured failure for " <> show granularity <> ", got: " <> show other)
  let isStep :: Note -> Bool
      isStep n = case n.kind of
        StepHeader _ _ -> True
        _ -> False
  pure
    Shrunk
      { steps = length (filter isStep notes),
        supplyDraws = length (filter isSupplyDraw notes),
        seconds = end - start
      }

spec :: Spec
spec = describe "a stateful machine over a slot-backed fixture" do
  it "shrinks a per-step fixture to two steps that span exactly the TTL" do
    drawn <- viable (cachingProperty PerStep)
    let supplyDrawn = [t | t <- drawn, "tick=" `T.isPrefixOf` t || "GET /" `T.isPrefixOf` t]
    supplyDrawn `shouldBe` ["tick=0", "GET /a=0", "tick=60"]

  it "finds the bug with one supply for the whole run" do
    -- Seed 1 is one where this mode stalls above the minimal two steps.
    report <- check def {seed = Just (SeedFixed 1)} (cachingProperty PerRun)
    showReport <- (== Just "1") <$> lookupEnv "SUPPLY_SPIKE_SHOW"
    when showReport (renderReportRich report >>= TIO.putStrLn)
    report.result `shouldSatisfy` \case
      Failures _ -> True
      _ -> False

  it "shrinks per-step at least as well as per-run across seeds" do
    rows <- for [1 .. 10] \s -> do
      let settings = def {seed = Just (SeedFixed s)}
      perRun <- measure settings PerRun
      perStep <- measure settings PerStep
      pure (s, perRun, perStep)
    showTable <- (== Just "1") <$> lookupEnv "SUPPLY_SPIKE_SHOW"
    when showTable do
      printf "\n%5s | %-22s | %s\n" ("" :: String) ("one supply per run" :: String) ("one supply per step" :: String)
      printf "%5s | %6s %6s %8s | %6s %6s %8s\n" ("seed" :: String) ("steps" :: String) ("draws" :: String) ("seconds" :: String) ("steps" :: String) ("draws" :: String) ("seconds" :: String)
      for_ rows \(s, r, p) ->
        printf "%5d | %6d %6d %8.2f | %6d %6d %8.2f\n" s r.steps r.supplyDraws r.seconds p.steps p.supplyDraws p.seconds
    for_ rows \(s, perRun, perStep) ->
      (s, perStep.steps <= perRun.steps) `shouldBe` (s, True)
