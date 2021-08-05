module Chopaan.Utils.Time where

import Control.Arrow
import Data.Fixed (Fixed(MkFixed), Pico)
import Data.Time.Compat
import Data.Time.LocalTime.Compat
import Data.Time.Clock.POSIX.Compat
import Data.Word
import Data.Ix
import qualified Data.Text as T
import GHC.Read
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec

fromPico :: Pico -> Integer
fromPico (MkFixed i) = i

asUTC :: LocalTime -> UTCTime
asUTC = localTimeToUTC tz
  where
    tz = TimeZone (round $ 5.5 * 60) False "PK"

timeToUIntSeconds :: UTCTime -> Word64
timeToUIntSeconds = fromInteger . (\x -> round $ (realToFrac x) / 10e11) . fromPico . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds


-- $ converts the timestamp (in seconds) in the EnergyState to a UTCTime  
utcTimeNow :: Word64 -> UTCTime
utcTimeNow = posixSecondsToUTCTime . fromIntegral

-- $ converts the millisecond timestamp in the EnergyState to a UTCTime  
parseUTCTime :: T.Text -> UTCTime
parseUTCTime = utcTimeNow . read . T.unpack


diffUTC :: UTCTime -> UTCTime -> DiffTime
diffUTC a b = realToFrac $ diffUTCTime a b

dayRange :: UTCTime -> UTCTime -> [UTCTime]
dayRange start end = r
  where
    r = (flip UTCTime $ 0) <$> range (utctDay start, succ $ utctDay end)


instance Read DiffTime where
  readPrec = do
    t <- readPrec
    _ <- lift $ char 's'
    return $ fromInteger t
