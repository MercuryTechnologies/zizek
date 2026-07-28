-- | 'TimeOfDay' generator.
--
-- Full day by default, midnight to @23:59:59.999999@; narrow with
-- 'Hegel.Gen.Builder.min' and 'Hegel.Gen.Builder.max':
--
-- > Gen.time & Gen.min midnight & Gen.max (TimeOfDay 12 0 0) & Gen.build
module Hegel.Gen.Time
  ( TimeBuilder,
    time,

    -- * Validation
    checkFields,
  )
where

import Control.Exception (throwIO)
import Data.Fixed (Fixed (MkFixed), Pico)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.LocalTime (TimeOfDay (..), midnight)
import Hegel.Gen.Builder (Build (..), HasMax (..), HasMin (..), ValidationError (..), checkOrdered)
import Hegel.Gen.Internal (Gen (..))
import Hegel.Internal.DataSource (drawTime)

data TimeBuilder = TimeBuilder
  { bMin :: Maybe TimeOfDay,
    bMax :: Maybe TimeOfDay
  }

-- | Generate a random time of day, defaulting to the full day: midnight to
-- @23:59:59.999999@.
time :: TimeBuilder
time = TimeBuilder {bMin = Nothing, bMax = Nothing}

instance HasMin TimeBuilder TimeOfDay where
  min lo b = b {bMin = Just lo}

instance HasMax TimeBuilder TimeOfDay where
  max hi b = b {bMax = Just hi}

instance Build TimeBuilder TimeOfDay where
  build b = Draw \tc -> do
    checkFields "Hegel.Gen.Time" lo
    checkFields "Hegel.Gen.Time" hi
    checkOrdered "Hegel.Gen.Time" lo hi
    drawTime tc lo hi
    where
      lo = fromMaybe midnight b.bMin
      hi = fromMaybe (TimeOfDay 23 59 59.999999) b.bMax

-- | Require @hour@ in @[0, 23]@, @minute@\/@second@ in @[0, 59]@, and
-- @second@'s fractional part to be a whole number of microseconds, throwing
-- 'ValidationError' otherwise.
checkFields :: Text -> TimeOfDay -> IO ()
checkFields ctx t
  | t.todHour < 0 || t.todHour > 23 = invalid "hour" t.todHour
  | t.todMin < 0 || t.todMin > 59 = invalid "minute" t.todMin
  | t.todSec < 0 || t.todSec >= 60 = invalid "second" t.todSec
  | not (wholeMicroseconds t.todSec) =
      throwIO
        ValidationError
          { context = ctx,
            detail = "second (" <> T.pack (show t.todSec) <> ") is finer than libhegel's microsecond resolution"
          }
  | otherwise = pure ()
  where
    wholeMicroseconds :: Pico -> Bool
    wholeMicroseconds (MkFixed ps) = ps `mod` 1_000_000 == 0
    invalid :: (Show a) => Text -> a -> IO ()
    invalid field v =
      throwIO
        ValidationError
          { context = ctx,
            detail = field <> " (" <> T.pack (show v) <> ") outside libhegel's range"
          }
