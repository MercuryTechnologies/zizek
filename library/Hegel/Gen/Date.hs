-- | 'Day' generator: proleptic Gregorian calendar dates.
--
-- > Gen.date & Gen.min (fromGregorian 2000 1 1) & Gen.max (fromGregorian 2030 12 31) & Gen.build
--
-- Bound by whole calendar year instead, when the month and day don't matter,
-- with 'Hegel.Gen.Builder.minYear' and 'Hegel.Gen.Builder.maxYear':
--
-- > Gen.date & Gen.minYear 2000 & Gen.maxYear 2030 & Gen.build
--
-- Generating sub-units of time is tricky enough that we don't expose many
-- opinionated combinators here, out of concern that there are edge cases we
-- can't fully anticipate.
--
-- Instead, we recommend generators be customized using modifiers like
-- 'Hegel.Gen.filtered'; for example, to generate dates that only fall on
-- weekends, one could write the following:
--
-- > Gen.date & Gen.build & Gen.filtered (\d -> dayOfWeek d `elem` [Saturday, Sunday])
module Hegel.Gen.Date
  ( DateBuilder,
    date,

    -- * Validation
    checkYearRange,
  )
where

import Control.Exception (throwIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar (Day, fromGregorian, toGregorian)
import Hegel.Gen.Builder (Build (..), GenValidationError (..), HasMax (..), HasMin (..), HasYear (..), checkOrdered)
import Hegel.Gen.Internal (Gen (..))
import Hegel.Internal.DataSource (drawDate)

data DateBuilder = DateBuilder
  { bMin :: Maybe Day,
    bMax :: Maybe Day
  }

-- | Generate a random proleptic Gregorian calendar date, defaulting to
-- @libhegel@'s full representable year range: @-999999@ to @999999@.
date :: DateBuilder
date = DateBuilder {bMin = Nothing, bMax = Nothing}

instance HasMin DateBuilder Day where
  min lo b = b {bMin = Just lo}

instance HasMax DateBuilder Day where
  max hi b = b {bMax = Just hi}

instance HasYear DateBuilder where
  minYear y b = b {bMin = Just (fromGregorian y 1 1)}
  maxYear y b = b {bMax = Just (fromGregorian y 12 31)}

instance Build DateBuilder Day where
  build b = Draw \tc -> do
    checkYearRange "Hegel.Gen.Date" lo
    checkYearRange "Hegel.Gen.Date" hi
    checkOrdered "Hegel.Gen.Date" lo hi
    drawDate tc lo hi
    where
      lo = fromMaybe (fromGregorian (-999999) 1 1) b.bMin
      hi = fromMaybe (fromGregorian 999999 12 31) b.bMax

-- | Require the year to fall in @[-999999, 999999]@, throwing
-- 'GenValidationError' otherwise.
checkYearRange :: Text -> Day -> IO ()
checkYearRange ctx d
  | y < -999999 || y > 999999 =
      throwIO
        GenValidationError
          { context = ctx,
            detail = "year (" <> T.pack (show y) <> ") outside libhegel's [-999999, 999999] range"
          }
  | otherwise = pure ()
  where
    (y, _, _) = toGregorian d
