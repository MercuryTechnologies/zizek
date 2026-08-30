-- | Run an action against a fresh 'Collection' with 'with', then iterate
-- with 'more' and optionally reject elements with 'reject'.
--
-- Usage:
--
-- > Collection.with tc minSize maxSize \coll -> do
-- >   let loop acc = do
-- >         keepGoing <- Collection.more coll
-- >         if not keepGoing
-- >           then pure (reverse acc)
-- >           else do
-- >             x <- draw tc elemGen
-- >             loop (x : acc)
-- >   loop []
module Hegel.Collection
  ( -- * Handle
    Collection,

    -- * Scope
    with,

    -- * Operations
    more,
    reject,
  )
where

{- Note [Variable-size mode required for reject]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'reject' only makes forward progress when the underlying collection was
created in /variable-size/ mode, i.e. when max_size > min_size.

The `many` primitive has two paths:

  * Variable size: `more` draws a continue/stop bit via the data stream
    (advancing the cursor) and `reject` decrements the count.
  * Fixed size (min_size == max_size): `more` is a pure `count < min_size`
    comparison consuming no random bytes, and `reject` likewise only updates
    counters.

In fixed-size mode the data cursor never moves between successive
`reject`/`more` calls, so the next element draw reads from the same position
and regenerates the same value — an infinite duplicate loop.

Callers that need to reject duplicates (sets, unique lists, maps with
key-uniqueness) must therefore force variable-size mode by bumping max_size
to at least min_size + 1 and trimming any overshoot from the final result.

The trim only fires when @libhegel@ actually overshoots the declared maximum.
-}

import Control.Exception (bracket)
import Data.Text (Text)
import Foreign (Ptr)
import Hegel.Internal.DataSource (HegelCollection, collectionMore, collectionReject, freeCollection, newCollection)
import Hegel.Internal.TestCase (TestCase)
import UnliftIO.IORef (IORef, newIORef, readIORef, writeIORef)

-- | Handle to a @libhegel@-managed collection, live only for the duration of
-- 'with'.
data Collection = Collection
  { tc :: !TestCase,
    handle :: !(Ptr HegelCollection),
    finished :: !(IORef Bool)
  }

-- | Create a collection scoped to @action@, freeing the native handle on
-- every exit, including an exception.
with :: TestCase -> Int -> Maybe Int -> (Collection -> IO a) -> IO a
with tc minSz maxSz action =
  bracket acquire release action
  where
    acquire :: IO Collection
    acquire = do
      h <- newCollection tc minSz maxSz
      f <- newIORef False
      pure Collection {tc, handle = h, finished = f}
    release :: Collection -> IO ()
    release coll = freeCollection coll.tc coll.handle

-- | Ask @libhegel@ whether it can produce another element.
--
-- Returns 'False' once the collection is complete; subsequent calls return
-- 'False' immediately.
--
-- Throws 'Hegel.Internal.TestCase.TestStopped' when @libhegel@ signals the test should stop.
more :: Collection -> IO Bool
more coll = do
  done <- readIORef coll.finished
  if done
    then pure False
    else do
      result <- collectionMore coll.tc coll.handle
      if result
        then pure True
        else do
          writeIORef coll.finished True
          pure False

-- | Tell @libhegel@ to discard the last element, which will not count towards
-- the size budget.
--
-- No-op if the collection is already finished.
reject :: Collection -> Maybe Text -> IO ()
reject coll why = do
  done <- readIORef coll.finished
  if done
    then pure ()
    else collectionReject coll.tc coll.handle why
