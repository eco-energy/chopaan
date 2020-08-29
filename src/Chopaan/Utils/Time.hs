module Chopaan.Utils.Time where

import Data.Fixed (Fixed(MkFixed), Pico)
import Data.Time.Compat
import Data.Time.LocalTime.Compat
import Data.Time.Clock.POSIX.Compat
import Data.Word

fromPico :: Pico -> Integer
fromPico (MkFixed i) = i


timeToUIntSeconds :: LocalTime -> Word64
timeToUIntSeconds = fromInteger . fromPico . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds . (localTimeToUTC tz)
  where
    tz = TimeZone (round $ 5.5 * 60) False "PK"
