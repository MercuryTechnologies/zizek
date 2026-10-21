-- | UTF-8 string marshalling for the @libhegel@ FFI boundary, built on
-- @hegel_string_t@, a borrowed pointer and byte length.
--
-- Every string @libhegel@ takes or hands back is UTF-8 regardless of the
-- process locale, so these helpers encode and decode explicitly rather than
-- through "Foreign.C.String".
module Hegel.Internal.Foreign.String
  ( HegelString (..),
    withText,
    withBytes,
    withOptText,
    withTexts,
    peekText,
    peekBytes,
    peekOptText,
    peekOptBytes,
  )
where

#include <hegel.h>

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Foreign (Ptr, Storable (..), nullPtr, with, withArrayLen, withMany)
import Foreign.C.String (CString, CStringLen)
import Foreign.C.Types (CSize (..))

-- | Mirrors @hegel_string_t@: @strLen@ bytes of UTF-8 at @strData@.
--
-- An input needs no NUL terminator, and a zero-length input may have a @NULL@
-- @strData@. An output always has a non-@NULL@, NUL-terminated @strData@, but
-- may contain interior NULs, so read exactly @strLen@ bytes.
data HegelString = HegelString
  { strData :: !CString,
    strLen :: !CSize
  }

instance Storable HegelString where
  sizeOf _ = (#size hegel_string_t)
  alignment _ = (#alignment hegel_string_t)
  peek p =
    HegelString
      <$> (#peek hegel_string_t, data) p
      <*> (#peek hegel_string_t, len) p
  poke p v = do
    (#poke hegel_string_t, data) p v.strData
    (#poke hegel_string_t, len) p v.strLen

-- | Pass 'Text' to @libhegel@ as a @hegel_string_t@ valid for the scope of the
-- action.
withText :: Text -> (Ptr HegelString -> IO a) -> IO a
withText = withBytes . TE.encodeUtf8

-- | Pass raw bytes to @libhegel@ as a @hegel_string_t@ valid for the scope of
-- the action. @libhegel@ rejects bytes that are not UTF-8.
withBytes :: ByteString -> (Ptr HegelString -> IO a) -> IO a
withBytes bs k = BS.useAsCStringLen bs \cstr -> with (fromCStringLen cstr) k

-- | Pass an optional string as a nullable @const hegel_string_t *@, where
-- 'Nothing' is @NULL@ and means absent.
withOptText :: Maybe Text -> (Ptr HegelString -> IO a) -> IO a
withOptText Nothing k = k nullPtr
withOptText (Just t) k = withText t k

-- | Pass a list of strings as an array of @hegel_string_t@ and its length.
withTexts :: [Text] -> (Ptr HegelString -> CSize -> IO a) -> IO a
withTexts ts k =
  withMany (BS.useAsCStringLen . TE.encodeUtf8) ts \cstrs ->
    withArrayLen (fromCStringLen <$> cstrs) \n arr -> k arr (fromIntegral n)

fromCStringLen :: CStringLen -> HegelString
fromCStringLen (p, n) = HegelString {strData = p, strLen = fromIntegral n}

-- | Copy a string @libhegel@ wrote into a fresh 'Text'.
--
-- Decoding is lenient, so a malformed byte becomes U+FFFD and reporting a
-- failure can never itself crash the runner.
peekText :: Ptr HegelString -> IO Text
peekText p = TE.decodeUtf8Lenient <$> peekBytes p

-- | Copy the bytes of a string @libhegel@ wrote into a fresh 'ByteString'.
peekBytes :: Ptr HegelString -> IO ByteString
peekBytes p = do
  s <- peek p
  if s.strData == nullPtr
    then pure BS.empty
    else BS.packCStringLen (s.strData, fromIntegral s.strLen)

-- | 'peekText' for an optional output, where a @NULL@ pointer means absent.
peekOptText :: Ptr HegelString -> IO (Maybe Text)
peekOptText p
  | p == nullPtr = pure Nothing
  | otherwise = Just <$> peekText p

-- | 'peekBytes' for an optional output, where a @NULL@ pointer means absent.
peekOptBytes :: Ptr HegelString -> IO (Maybe ByteString)
peekOptBytes p
  | p == nullPtr = pure Nothing
  | otherwise = Just <$> peekBytes p
