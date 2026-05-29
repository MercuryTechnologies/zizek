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

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Hegel.TestCase
  ( TestCase,
    collectionMore,
    collectionReject,
    newCollection,
  )

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
