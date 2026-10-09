-- | A concurrent stateful machine whose rules run in two concurrency groups.
--
-- Tellers post deposits and withdrawals to a shared account. Each posting
-- appends to the journal atomically but updates the balance with a separate
-- read and write, so two tellers posting in the same round can lose one
-- update. Auditors never share a round with tellers, so an audit always reads
-- a settled account and checks that its balance matches the journal.
--
-- The log tags every step with its round, worker, and group, and the failure
-- lands on the audit that read the drifted balance. The race depends on
-- thread timing, so the report carries the engine's note on how reliably the
-- failure reproduced.
module Gallery.Bank (scenario) where

import Control.Concurrent (yield)
import Control.Monad (replicateM_)
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Gallery.Scenario
import Hegel (Gen)
import Hegel.Gen qualified as Gen
import Hegel.Property (Property, forAllWithLabel, (===))
import Hegel.Report (FailureEvidence (..), Report, renderValue)
import Hegel.Settings (Settings (..), defaultSettings)
import Hegel.Stateful qualified as Stateful
import Hegel.Stateful.Concurrent qualified as Concurrent
import UnliftIO.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)

scenario :: Scenario
scenario =
  Scenario
    { name = "bank",
      title = "concurrent stateful: tellers race, an auditor notices",
      settings = defaultSettings {testCases = Just 20},
      property = Concurrent.run (Concurrent.fixed 3) machine,
      ascii = False,
      attempts = 3,
      expect = pure . check
    }

data Account = Account
  { balance :: IORef Int,
    journal :: IORef [Int]
  }

newAccount :: Property Account
newAccount = Account <$> newIORef 0 <*> newIORef []

-- | Record a posting in the journal, then apply it to the balance.
--
-- BUG: the balance update reads, then writes after yielding to the other
-- workers a few hundred times, so a concurrent posting between the read and
-- the write is lost. The yields widen that window enough for the race to fire
-- on most attempts without sleeping on a timer.
post :: Account -> Int -> IO ()
post account n = do
  atomicModifyIORef' account.journal \j -> (n : j, ())
  b <- readIORef account.balance
  replicateM_ 200 yield
  writeIORef account.balance (b + n)

amount :: Gen Int
amount = Gen.int & Gen.min 1 & Gen.max 100 & Gen.build

deposit :: Concurrent.Rule Account IO
deposit =
  Concurrent.grouped "deposit" "tellers" \account -> do
    n <- forAllWithLabel "amount" amount
    liftIO (post account n)

-- | Withdraw without an overdraft check, so no teller ever has to reject.
withdraw :: Concurrent.Rule Account IO
withdraw =
  Concurrent.grouped "withdraw" "tellers" \account -> do
    n <- forAllWithLabel "amount" amount
    liftIO (post account (negate n))

audit :: Concurrent.Rule Account IO
audit =
  Concurrent.grouped "audit" "auditors" \account -> do
    b <- liftIO (readIORef account.balance)
    j <- liftIO (readIORef account.journal)
    Stateful.respondShow b
    b === sum j

machine :: Concurrent.Machine Account IO
machine =
  Concurrent.Machine
    { initial = newAccount,
      rules = [deposit, withdraw, audit],
      invariants = [],
      stepCount = 3
    }

check :: Report -> [Text]
check report = case captured report of
  Right [evidence] ->
    let rows = logRows evidence
        failing = [r.call | r <- rows, r.failed]
        groups = mapMaybe (.origin) rows
     in ensureEqual "message" "=== failed, values are not equal" evidence.message
          <> ensure (not (null failing) && all ((== "audit") . T.takeWhile (/= ' ')) failing) ("expected the failure on audits only, got " <> renderValue failing)
          <> ensure (any ("(tellers)" `T.isInfixOf`) groups) "expected a teller step in the log"
  Right evidence -> ["expected one failure, got " <> renderValue (length evidence)]
  Left mismatch -> [mismatch]
