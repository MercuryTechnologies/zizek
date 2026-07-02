-- | Profiling copy of a pool-bearing stateful machine: file handles that
-- live in an @open@ pool and 'Hegel.Pool.transfer' into a @closed@ pool on
-- close, so a failure renders the full composed trace report (verdict list +
-- citation ledger with a transfer lineage). Modelled on the gallery's
-- flagship file-handle scenario (@examples/gallery/Main.hs@).
--
-- The point is coverage of the composed-report machinery: the per-case event stream
-- (recorded only on the final reconstruction replay, 'Silent' otherwise) and
-- the one-shot @Trace.build@ / @Blame.analyze@ / ledger / verdict render.
--
-- 'Fixed' clears a handle's contents on close, so every @read_closed@ sees an
-- empty buffer and the machine passes (the @pool@ scenario — isolates the
-- per-case pool-op overhead). 'Buggy' leaks the buffer, so a handle written
-- then closed then read fails (the @render-trace@ scenario).
module Handles
  ( Bug (..),
    machine,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Hegel.Gen qualified as Gen
import Hegel.Pool (Pool)
import Hegel.Pool qualified as Pool
import Hegel.Property (forAll, (===))
import Hegel.Property.Internal (Env (..), askEnv)
import Hegel.Stateful qualified as Stateful
import UnliftIO.IORef (IORef, modifyIORef', newIORef, readIORef)

-- | Whether 'machine' leaks a closed handle's buffer.
data Bug = Fixed | Buggy

data FileModel = FileModel
  { openHandles :: Pool Int,
    closedHandles :: Pool Int,
    nextHandle :: IORef Int,
    -- | SUT: buffered content per handle.
    contents :: IORef (Map Int Text)
  }

machine :: Bug -> Stateful.Machine FileModel IO
machine bug =
  Stateful.Machine
    { initial = do
        env <- askEnv
        openHandles <- liftIO (Pool.named "h" env.testCase)
        closedHandles <- liftIO (Pool.named "c" env.testCase)
        nextHandle <- newIORef 0
        contents <- newIORef Map.empty
        pure FileModel {openHandles, closedHandles, nextHandle, contents},
      rules =
        [ Stateful.Rule "open" \m -> do
            h <- liftIO do
              h <- readIORef m.nextHandle
              modifyIORef' m.nextHandle (+ 1)
              modifyIORef' m.contents (Map.insert h "")
              pure h
            liftIO (Pool.add m.openHandles h)
            Stateful.respond "ok"
            pure m,
          Stateful.Rule "write" \m -> do
            h <- forAll (Pool.valuesReusable m.openHandles)
            v <- forAll (Gen.text & Gen.minSize 1 & Gen.maxSize 4 & Gen.build)
            liftIO (modifyIORef' m.contents (Map.insert h v))
            Stateful.respond "ok"
            pure m,
          Stateful.Rule "close" \m -> do
            h <- forAll (Pool.transfer m.openHandles m.closedHandles)
            liftIO case bug of
              -- BUG: the buffer is left behind on close.
              Buggy -> pure ()
              Fixed -> modifyIORef' m.contents (Map.insert h "")
            Stateful.respond "ok"
            pure m,
          Stateful.Rule "read_closed" \m -> do
            h <- forAll (Pool.valuesReusable m.closedHandles)
            r <- liftIO (Map.findWithDefault "" h <$> readIORef m.contents)
            Stateful.respondShow r
            r === ""
            pure m
        ],
      invariants = []
    }
