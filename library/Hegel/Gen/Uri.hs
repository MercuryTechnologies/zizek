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
import GHC.Stack (withFrozenCallStack)
import Hegel.Gen.Builder (Build (..))
import Hegel.Gen.Internal.String (stringDraw, stringGen)
import Hegel.Internal.DataSource (InvariantViolation (..), Label (LabelUrl), buildUrlGen, spanLabel)
import Hegel.Internal.TestCase (TestCase)
import Network.URI (URI, parseURI)

-- | Builder for a 'URI' generator.
data UriBuilder = UriBuilder

-- | Generate a random RFC 3986 HTTP\/HTTPS URL, returning a parsed 'URI'.
uri :: UriBuilder
uri = UriBuilder

-- | Builder for a generator of URIs rendered as 'Text'.
data UriTextBuilder = UriTextBuilder

-- | Generate a random RFC 3986 HTTP\/HTTPS URL, returning the raw 'Text'.
uriText :: UriTextBuilder
uriText = UriTextBuilder

instance Build UriBuilder URI where
  build _ = withFrozenCallStack $ stringDraw (spanLabel LabelUrl) buildUrlGen postProcess
    where
      postProcess :: TestCase -> Text -> IO URI
      postProcess _tc t = case parseURI (T.unpack t) of
        Just u -> pure u
        Nothing ->
          throwIO InvariantViolation {detail = "libhegel: unparseable URI from a url draw: " <> t}

instance Build UriTextBuilder Text where
  build _ = withFrozenCallStack $ stringGen (spanLabel LabelUrl) buildUrlGen
