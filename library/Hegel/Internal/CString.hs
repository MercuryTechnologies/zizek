-- | UTF-8 C-string marshalling for the @libhegel@ FFI boundary.
module Hegel.Internal.CString
  ( withText,
    withFilePath,
  )
where

import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Foreign.C.String (CString)

-- | Marshal 'Text' to a NUL-terminated UTF-8 'CString' for the scope of the
-- action.
withText :: Text -> (CString -> IO a) -> IO a
withText = BS.useAsCString . encodeUtf8

-- | 'withText' for a 'FilePath'; @libhegel@ requires database paths to be UTF-8.
withFilePath :: FilePath -> (CString -> IO a) -> IO a
withFilePath = withText . T.pack
