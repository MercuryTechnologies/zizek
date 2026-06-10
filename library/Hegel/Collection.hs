-- | Backend-managed collection handle for variable-length compositional generation.
--
-- Build a 'Collection' with 'new', then iterate with 'more' and optionally
-- reject elements with 'reject'. The collection handle is a thin wrapper
-- around the three wire commands in 'Hegel.TestCase'; it does not know about
-- element types, uniqueness predicates, or retry budgets — those live at the
-- call site.
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

The server-side `many` primitive (mirrored from Hypothesis) has two paths:

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
The trim only fires when the server actually overshoots the declared
maximum, which is rare.
-}

import Data.Text (Text)
import Hegel.TestCase
  ( TestCase,
    collectionMore,
    collectionReject,
    newCollection,
  )
import UnliftIO.IORef (IORef, newIORef, readIORef, writeIORef)

-- | Opaque handle to a backend-managed collection.
data Collection = Collection
  { tc :: !TestCase,
    minSize :: !Int,
    maxSize :: !(Maybe Int),
    -- | Populated lazily on the first call to 'more'.
    handle :: !(IORef (Maybe Int)),
    finished :: !(IORef Bool)
  }

-- | Create a new collection handle. Does not contact the server yet; the
-- @new_collection@ wire command is deferred to the first call to 'more'.
new :: TestCase -> Int -> Maybe Int -> IO Collection
new tc minSz maxSz = do
  h <- newIORef Nothing
  f <- newIORef False
  pure Collection {tc, minSize = minSz, maxSize = maxSz, handle = h, finished = f}

-- | Ask the server whether to produce another element. Returns 'False' once
-- the server signals the collection is complete; subsequent calls return
-- 'False' immediately without a round-trip. Throws 'Hegel.TestCase.TestStopped'
-- when the server sends a stop-test signal.
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

-- | Tell the server to discard the last element (does not count towards the
-- size budget). No-op if the collection is already finished.
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
