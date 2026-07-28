-- | 'NominalDiffTime' generator: elapsed-time durations.
--
-- Always non-negative, with a default upper bound of about 2 million years;
-- narrow with 'Hegel.Gen.Builder.min' and 'Hegel.Gen.Builder.max'. A bare
-- number is a count of seconds by way of 'NominalDiffTime''s 'Num' instance,
-- with nothing marking the unit at the call site, so prefer an explicit unit
-- constructor for the bound instead:
--
-- > Gen.duration & Gen.min (Gen.seconds 30) & Gen.max (Gen.hours 2) & Gen.build
module Hegel.Gen.Duration
  ( DurationBuilder,
    duration,

    -- * Units
    milliseconds,
    seconds,
    minutes,
    hours,
  )
where

import Control.Exception (throwIO)
import Data.Fixed (Fixed (MkFixed), Pico)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Time.Calendar (diffDays, fromGregorian)
import Data.Time.Clock (NominalDiffTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Hegel.Gen.Builder (Build (..), HasMax (..), HasMin (..), ValidationError (..), checkOrdered)
import Hegel.Gen.Internal (Gen (..))
import Hegel.Internal.DataSource (drawInteger)

data DurationBuilder = DurationBuilder
  { bMin :: Maybe NominalDiffTime,
    bMax :: Maybe NominalDiffTime
  }

-- | Generate a random non-negative elapsed-time duration, defaulting to
-- @[0, ~2 million years]@.
duration :: DurationBuilder
duration = DurationBuilder {bMin = Nothing, bMax = Nothing}

-- | A duration of the given number of milliseconds.
milliseconds :: Pico -> NominalDiffTime
milliseconds n = secondsToNominalDiffTime (n / 1000)

-- | A duration of the given number of seconds.
seconds :: Pico -> NominalDiffTime
seconds = secondsToNominalDiffTime

-- | A duration of the given number of minutes.
minutes :: Pico -> NominalDiffTime
minutes n = secondsToNominalDiffTime (n * 60)

-- | A duration of the given number of hours.
hours :: Pico -> NominalDiffTime
hours n = secondsToNominalDiffTime (n * 3600)

instance HasMin DurationBuilder NominalDiffTime where
  min lo b = b {bMin = Just lo}

instance HasMax DurationBuilder NominalDiffTime where
  max hi b = b {bMax = Just hi}

instance Build DurationBuilder NominalDiffTime where
  build b = Draw \tc -> do
    checkOrdered "Hegel.Gen.Duration" lo hi
    checkNonNegative lo
    fromPicoseconds <$> drawInteger tc (toPicoseconds lo) (toPicoseconds hi)
    where
      lo = fromMaybe 0 b.bMin
      hi = fromMaybe defaultMax b.bMax
      checkNonNegative :: NominalDiffTime -> IO ()
      checkNonNegative n
        | n < 0 =
            throwIO
              ValidationError
                { context = "Hegel.Gen.Duration",
                  detail = "negative duration (" <> T.pack (show n) <> ")"
                }
        | otherwise = pure ()

-- | The default upper bound: the number of seconds between
-- 'Hegel.Gen.Date''s own extreme years, @-999999@ and @999999@.
--
-- 'NominalDiffTime' has no natural bound of its own, so this ties the
-- default to the widest gap this library's own date generators can already
-- produce, rather than an unrelated constant.
defaultMax :: NominalDiffTime
defaultMax =
  fromPicoseconds (diffDays (fromGregorian 999999 12 31) (fromGregorian (-999999) 1 1) * 86400 * 1_000_000_000_000)

-- | 'NominalDiffTime's exact picosecond count.
toPicoseconds :: NominalDiffTime -> Integer
toPicoseconds ndt = ps
  where
    MkFixed ps = nominalDiffTimeToSeconds ndt

-- | Convert an exact picosecond count to a 'NominalDiffTime'.
fromPicoseconds :: Integer -> NominalDiffTime
fromPicoseconds = secondsToNominalDiffTime . MkFixed
