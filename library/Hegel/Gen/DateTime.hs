-- | 'LocalTime' generator: a naive date and time of day, with no timezone.
--
-- > Gen.datetime & Gen.min (LocalTime (fromGregorian 2000 1 1) midnight) & Gen.build
--
-- 'Hegel.Gen.Builder.min' and 'Hegel.Gen.Builder.max' compare 'LocalTime'
-- lexicographically, by date and then by time of day, not as an independent
-- date range crossed with a time-of-day range.
--
-- For "any day in this range, but only business hours," compose independent
-- date and time generators instead, which also shrinks each component
-- separately:
--
-- > LocalTime
-- >   <$> (Gen.date & Gen.minYear 2024 & Gen.maxYear 2024 & Gen.build)
-- >   <*> (Gen.time & Gen.min (TimeOfDay 9 0 0) & Gen.max (TimeOfDay 17 0 0) & Gen.build)
--
-- Bound by whole calendar year with 'Hegel.Gen.Builder.minYear' and
-- 'Hegel.Gen.Builder.maxYear', or to a single calendar day's full range of
-- times with 'onDay':
--
-- > Gen.datetime & Gen.onDay (fromGregorian 2024 6 15) & Gen.build
module Hegel.Gen.DateTime
  ( DateTimeBuilder,
    datetime,

    -- * Modifiers
    onDay,
  )
where

import Data.Maybe (fromMaybe)
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..), midnight)
import Hegel.Gen.Builder (Build (..), HasMax (..), HasMin (..), HasYear (..), checkOrdered)
import Hegel.Gen.Date (checkYearRange)
import Hegel.Gen.Internal (Gen (..))
import Hegel.Gen.Time (checkFields)
import Hegel.Internal.DataSource (drawDatetime)

data DateTimeBuilder = DateTimeBuilder
  { bMin :: Maybe LocalTime,
    bMax :: Maybe LocalTime
  }

-- | Generate a random naive datetime, defaulting to @libhegel@'s full
-- representable range.
datetime :: DateTimeBuilder
datetime = DateTimeBuilder {bMin = Nothing, bMax = Nothing}

instance HasMin DateTimeBuilder LocalTime where
  min lo b = b {bMin = Just lo}

instance HasMax DateTimeBuilder LocalTime where
  max hi b = b {bMax = Just hi}

instance HasYear DateTimeBuilder where
  minYear y b = b {bMin = Just (LocalTime (fromGregorian y 1 1) midnight)}
  maxYear y b = b {bMax = Just (LocalTime (fromGregorian y 12 31) (TimeOfDay 23 59 59.999999))}

-- | Restrict draws to the given calendar day, spanning its full range of
-- times of day.
onDay :: Day -> DateTimeBuilder -> DateTimeBuilder
onDay d b = b {bMin = Just (LocalTime d midnight), bMax = Just (LocalTime d (TimeOfDay 23 59 59.999999))}

instance Build DateTimeBuilder LocalTime where
  build b = Draw \tc -> do
    checkYearRange "Hegel.Gen.DateTime" lo.localDay
    checkYearRange "Hegel.Gen.DateTime" hi.localDay
    checkFields "Hegel.Gen.DateTime" lo.localTimeOfDay
    checkFields "Hegel.Gen.DateTime" hi.localTimeOfDay
    checkOrdered "Hegel.Gen.DateTime" lo hi
    drawDatetime tc lo hi
    where
      lo = fromMaybe (LocalTime (fromGregorian (-999999) 1 1) midnight) b.bMin
      hi = fromMaybe (LocalTime (fromGregorian 999999 12 31) (TimeOfDay 23 59 59.999999)) b.bMax
