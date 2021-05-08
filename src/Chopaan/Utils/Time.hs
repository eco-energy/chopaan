module Chopaan.Utils.Time where

import Data.Fixed (Fixed(MkFixed), Pico)
import Data.Time.Compat
import Data.Time.LocalTime.Compat
import Data.Time.Clock.POSIX.Compat
import Data.Word
import qualified Data.Text as T

fromPico :: Pico -> Integer
fromPico (MkFixed i) = i

asUTC :: LocalTime -> UTCTime
asUTC = localTimeToUTC tz
  where
    tz = TimeZone (round $ 5.5 * 60) False "PK"

timeToUIntSeconds :: LocalTime -> Word64
timeToUIntSeconds = fromInteger . (\x -> round $ (realToFrac x) / 10e11) . fromPico . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds . asUTC


-- $ converts the millisecond timestamp in the EnergyState to a UTCTime  
utcTimeNow :: Word64 -> UTCTime
utcTimeNow = posixSecondsToUTCTime . fromIntegral

-- $ converts the millisecond timestamp in the EnergyState to a UTCTime  
parseUTCTime :: T.Text -> UTCTime
parseUTCTime = utcTimeNow . read . T.unpack


diffUTC :: UTCTime -> UTCTime -> DiffTime
diffUTC a b = realToFrac $ diffUTCTime a b
