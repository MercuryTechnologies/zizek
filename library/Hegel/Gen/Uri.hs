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
-- Both builders use the same @{\"type\": \"url\"}@ schema and produce RFC 3986
-- HTTP\/HTTPS URLs.
module Hegel.Gen.Uri
  ( -- * Builders
    UriBuilder,
    uri,
    UriTextBuilder,
    uriText,
  )
where

import CBOR.Value (Value)
import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Cbor (ParseError (..), hegelText)
import Hegel.Gen.Builder (Build (..))
import Hegel.Gen.Internal (basic)
import Hegel.Schema qualified as Schema
import Network.URI (URI, parseURI)

data UriBuilder = UriBuilder

-- | Generate a random RFC 3986 HTTP\/HTTPS URL, returning a parsed 'URI'.
uri :: UriBuilder
uri = UriBuilder

data UriTextBuilder = UriTextBuilder

-- | Generate a random RFC 3986 HTTP\/HTTPS URL, returning the raw 'Text'.
uriText :: UriTextBuilder
uriText = UriTextBuilder

instance Build UriBuilder URI where
  build _ = basic Schema.url parseUri

instance Build UriTextBuilder Text where
  build _ = basic Schema.url hegelText

parseUri :: Value -> Either ParseError URI
parseUri v = case hegelText v of
  Left err -> Left err
  Right t -> case parseURI (T.unpack t) of
    Just u -> Right u
    Nothing -> Left ParseError {expected = "URI", got = v}
