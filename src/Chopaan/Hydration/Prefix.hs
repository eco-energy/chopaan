{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DerivingStrategies, GeneralizedNewtypeDeriving, DerivingVia #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables, BangPatterns, FlexibleContexts #-}
module Chopaan.Hydration.Prefix
  ( Prefix(..)
  , Resolution(..)
  , posthence
  , prefixRange
  , asFileName
  ) where

import GHC.Generics hiding (Prefix)
import Control.DeepSeq
import Dhall
import System.Envy
import Data.Aeson (ToJSON, FromJSON)
import Data.Time (UTCTime)
import Data.Time.Clock.Compat (NominalDiffTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.Int
import Data.Maybe
import Data.List
import Foreign.Storable
import Servant.API (FromHttpApiData(..), ToHttpApiData(..))
import qualified Data.Text as T
import Text.Read (readMaybe)
import qualified Codec.Winery as W
import Control.Monad.IO.Class

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.IsStream as S
import Streamly.Internal.Data.Time.Units
import qualified Streamly.Internal.Data.Time.Clock as Clk

import Chopaan.Utils.API
-- $ Our S3 File Names are epoch-times in milliseconds.
--   In order to iterate over slices of them, we generate streams of
--   prefixes, the longest of which is the finest time granularity we care to track,
--   i.e second, and each subsequent one is a power of ten applied to a second,
--   which gives us a shorter and shorter prefix covering a longer and longer period
--   of time. 

data Resolution = Second | Ten | Ten2 | Ten3 | Ten4 | Ten5 | Ten6 | Ten7 
  deriving (Eq, Ord, Show, Generic, Bounded, Read, Enum, NFData, ToJSON, FromJSON)

newtype Prefix = Prefix { unPrefix :: Int64 }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Enum, Bounded, Num, Real, Integral, Storable)
  deriving (W.Serialise) via (W.WineryRecord (Prefix))


posthence :: (S.IsStream t, S.MonadAsync m) => Resolution -> t m (Prefix)
posthence r = S.delayPre (asSec resDiff)
  $ S.map (forget r . asPast)
  $ S.repeatM (liftIO $ Clk.getTime (Clk.Realtime))
  where
    asPast a = addToAbsTime a (negate resDiff)
    resDiff = magnitude r
    asSec dt = realToFrac . sec $ fromRelTime dt
-- https://vimeo.com/72870861
-- THERE IS A GALOIS CONNECTION BETWEEN PREFIX AND SECONDS
-- THAT IS MEDIATED BY RESOLUTION

-- fromMilli :: TimeUnit a => a -> Resolution -> Prefix
-- fromMilli = sec . toTimeSpec
-- {-# INLINE fromMilli #-}

toSeconds :: Resolution -> (Prefix -> TimeSpec)
toSeconds r = \(Prefix p) ->  TimeSpec { sec = p * (10 ^ fromEnum r), nsec = 0 } 
{-# INLINE toSeconds #-}

magnitude :: Resolution -> RelTime
magnitude r = toRelTime (TimeSpec { sec = 10 ^ (fromEnum r), nsec = 0})

numDigits :: Resolution -> Int
numDigits = fromEnum


-- $ You can forget any UTCTime/Time Unit to a Prefix
-- You can lift any prefix to the time it represents
-- This is an adjunction, not a bijection, since to get
-- a prefix one has to round down the time to a power of ten.

class PrefixAdjunction timeUnit where
  -- $ We just convert our unit the number of seconds and
  --   truncate it to the right resolution.
  forget :: Resolution -> timeUnit -> Prefix
  -- $ The prefix passed to lift is a truncated timestamp
  --   We can just pad its length to 10 to get a number of
  --   seconds and then just convert to the timestamp.
  lift :: Resolution -> Prefix -> timeUnit

epochSecondsToPrefix :: Integral n => Resolution -> n -> Prefix
epochSecondsToPrefix r = Prefix . fromIntegral . unDigits 10 . take (sigDigs r) . digits 10

instance PrefixAdjunction UTCTime where
  forget r = epochSecondsToPrefix r . utcToSeconds
  {-# INLINE forget #-}
  lift r = posixSecondsToUTCTime . fromIntegral . sec . (toSeconds r)   
  {-# INLINE lift #-}

instance PrefixAdjunction AbsTime where
  forget r (AbsTime t) = epochSecondsToPrefix r $ sec t
  {-# INLINE forget #-}
  lift r = AbsTime . toSeconds r
  {-# INLINE lift #-}
  
epochTimeInSecondsHasDigits :: Int
epochTimeInSecondsHasDigits = 10
{-# INLINE epochTimeInSecondsHasDigits #-}

-- $ (sigDigs r) + (unSigDigs r) == epochTimeInSecondsHasDigits  

sigDigs :: Resolution -> Int
sigDigs r = epochTimeInSecondsHasDigits - (numDigits r)
{-# INLINE sigDigs #-}

unSigDigs :: Resolution -> Int
unSigDigs = numDigits
{-# INLINE unSigDigs#-}


utcToSeconds :: UTCTime -> Int64
utcToSeconds = (ceiling @_ @Int64)
  . nominalDiffTimeToSeconds
  . utcTimeToPOSIXSeconds
{-# INLINE utcToSeconds#-}

prefixRange :: Resolution -> UTCTime -> Maybe UTCTime -> [Prefix]
prefixRange !r !t !t' = case t' of
  Nothing -> let
    start = forget r t
    in [start, succ start..]
  Just t'' -> let
    start = forget r t
    end = forget r t''
    in [start..end]


asFileName :: Prefix -> T.Text
asFileName = T.pack . show . unPrefix
{-# INLINE asFileName #-}


instance ToHttpApiData Resolution where
  toUrlPiece = toUrlPieceViaEnum

instance FromHttpApiData Resolution where
  parseUrlPiece = parseUrlPieceViaEnum


instance Var Resolution where
  toVar = show
  fromVar = readMaybe


digits :: Integral n => n -> n -> [n]
digits !n !n' = reverse . fromJust $ mDigitsRev n n'

mDigitsRev :: Integral n
  => n         -- ^ The base to use.
  -> n         -- ^ The number to convert to digit form.
  -> Maybe [n] -- ^ Nothing or Just the digits of the number in list form, in reverse.
mDigitsRev base i = if base < 1
  then Nothing -- We do not support zero or negative bases
  else Just $ dr base i
  where
    dr _ 0 = []
    dr b x = case base of
      1 -> genericTake x $ repeat 1
      _ -> let (rest, lastDigit) = quotRem x b in lastDigit : dr b rest

unDigits :: Integral n
         => n   -- ^ The base to use.
         -> [n] -- ^ The digits of the number in list form.
         -> n   -- ^ The original number.
unDigits base = foldl (\ a b -> a * base + b) 0
