-- | URI generators.
--
-- Generate a parsed 'URI':
--
-- > Gen.uri & Gen.build
--
-- Or keep the raw 'Text' when you don't need a structured value:
--
-- > Gen.uriText & Gen.build
--
-- Both builders draw from the same RFC 3986 HTTP\/HTTPS URL generator.
module Hegel.Gen.Uri
  ( -- * Builders
    UriBuilder,
    uri,
    UriTextBuilder,
    uriText,
  )
where

import Control.Exception (throwIO)
import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Gen.Builder (Build (..))
import Hegel.Gen.Internal (Gen (..))
import Hegel.Internal.DataSource (InvariantViolation (..), buildUrlGen, drawString)
import Network.URI (URI, parseURI)
import System.IO.Unsafe (unsafePerformIO)

data UriBuilder = UriBuilder

-- | Generate a random RFC 3986 HTTP\/HTTPS URL, returning a parsed 'URI'.
uri :: UriBuilder
uri = UriBuilder

data UriTextBuilder = UriTextBuilder

-- | Generate a random RFC 3986 HTTP\/HTTPS URL, returning the raw 'Text'.
uriText :: UriTextBuilder
uriText = UriTextBuilder

instance Build UriBuilder URI where
  build _ = Draw drawParsedUri
    where
      genFP = unsafePerformIO buildUrlGen
      {-# NOINLINE genFP #-}
      drawParsedUri tc = do
        t <- drawString tc genFP
        case parseURI (T.unpack t) of
          Just u -> pure u
          Nothing ->
            throwIO InvariantViolation {detail = "libhegel: unparseable URI from a url draw: " <> t}

instance Build UriTextBuilder Text where
  build _ = Draw \tc -> drawString tc genFP
    where
      genFP = unsafePerformIO buildUrlGen
      {-# NOINLINE genFP #-}
