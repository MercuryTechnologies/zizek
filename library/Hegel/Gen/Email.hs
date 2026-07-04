-- | Email-address generator.
--
-- > Gen.email & Gen.build
module Hegel.Gen.Email
  ( EmailBuilder,
    email,
  )
where

import Data.Text (Text)
import Hegel.Gen.Builder (Build (..))
import Hegel.Gen.Internal.String (stringGen)
import Hegel.Internal.DataSource (buildEmailGen)

data EmailBuilder = EmailBuilder

-- | Generate a random RFC 5321\/5322 email address, e.g. @alice\@example.com@.
email :: EmailBuilder
email = EmailBuilder

instance Build EmailBuilder Text where
  build _ = stringGen buildEmailGen
