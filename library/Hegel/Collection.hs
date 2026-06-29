-- | Build a 'Collection' with 'new', then iterate with 'more' and optionally
-- reject elements with 'reject'.
--
-- Usage:
--
-- > coll <- Collection.new tc minSize maxSize
-- > let loop acc = do
-- >       keepGoing <- Collection.more coll
-- >       if not keepGoing
-- >         then pure (reverse acc)
-- >         else do
-- >           x <- draw tc elemGen
-- >           loop (x : acc)
-- > loop []
module Hegel.Collection
  ( -- * Handle
    Collection,

    -- * Operations
    new,
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

import Data.Text (Text)
import Hegel.Internal.DataSource (collectionMore, collectionReject, newCollection)
import Hegel.Internal.TestCase (TestCase)
import UnliftIO.IORef (IORef, newIORef, readIORef, writeIORef)

-- | Opaque handle to a @libhegel@-managed collection.
data Collection = Collection
  { tc :: !TestCase,
    minSize :: !Int,
    maxSize :: !(Maybe Int),
    -- | Populated lazily on the first call to 'more'.
    handle :: !(IORef (Maybe Int)),
    finished :: !(IORef Bool)
  }

-- | Create a new collection handle.
new :: TestCase -> Int -> Maybe Int -> IO Collection
new tc minSz maxSz = do
  h <- newIORef Nothing
  f <- newIORef False
  pure Collection {tc, minSize = minSz, maxSize = maxSz, handle = h, finished = f}

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
      cid <- ensureHandle coll
      result <- collectionMore coll.tc cid
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
    else do
      cid <- ensureHandle coll
      collectionReject coll.tc cid why

ensureHandle :: Collection -> IO Int
ensureHandle coll = do
  mh <- readIORef coll.handle
  case mh of
    Just cid -> pure cid
    Nothing -> do
      cid <- newCollection coll.tc coll.minSize coll.maxSize
      writeIORef coll.handle (Just cid)
      pure cid
