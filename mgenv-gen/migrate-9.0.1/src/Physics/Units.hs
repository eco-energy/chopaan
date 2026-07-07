-- | Physics.Units — pure-Double reimplementation (drops astro/dimensional).
-- Only what the generation path needs: unit synonyms, a geographic coordinate,
-- and the (reverse) haversine used to place feeder nodes radially.
module Physics.Units
  ( R, V, Amp, Ohm, W, Watts, AmpH, Eff, ChargeEfficiency, DischargeEfficiency
  , Meters, MetersPerSecond, MetersSq, OhmMeters, BearingDeg, Temperature
  , EuclideanC, GeoC(..)
  , location, reverseHaversine, haversine, toRadians, toDegrees
  ) where

import Data.Fixed (mod')

type R = Double
type V = R
type Amp = R
type Ohm = R
type W = R
type Watts = W
type AmpH = R
type ChargeEfficiency = R
type DischargeEfficiency = R
type Eff = (ChargeEfficiency, DischargeEfficiency)
type Meters = R
type MetersPerSecond = R
type MetersSq = R
type OhmMeters = R
type BearingDeg = R      -- degrees
type Temperature = R
type EuclideanC = (Meters, Meters)

-- | Geographic coordinate in decimal degrees (lat, long).
data GeoC = GeoC { geoLat :: !Double, geoLong :: !Double }
  deriving (Eq, Show)

location :: Double -> Double -> GeoC
location = GeoC

toRadians :: (RealFrac a, Floating a) => a -> a
toRadians d = (d `mod'` 360) * pi / 180

toDegrees :: Floating a => a -> a
toDegrees r = r * 180 / pi

earthRad :: Double
earthRad = 6371000

haversine :: GeoC -> GeoC -> Meters
haversine (GeoC lat0d lon0d) (GeoC lat1d lon1d) = earthRad * c
  where
    lat0 = toRadians lat0d; lat1 = toRadians lat1d
    dlat = toRadians (lat1d - lat0d); dlon = toRadians (lon1d - lon0d)
    a = sin (dlat/2) ** 2 + cos lat0 * cos lat1 * sin (dlon/2) ** 2
    c = 2 * atan2 (sqrt a) (sqrt (1 - a))

-- | Point a distance @d@ (m) at bearing @theta@ (deg) from an origin.
reverseHaversine :: GeoC -> Meters -> BearingDeg -> GeoC
reverseHaversine (GeoC lat0d long0d) d theta' =
    location (toDegrees lat') (toDegrees long')
  where
    lat'   = asin (sin lat * cos angDist + cos lat * sin angDist * cos theta)
    long'  = long - atan2 (sin theta * sin angDist * cos lat)
                          (cos angDist - sin lat * sin lat')
    angDist = d / earthRad
    theta   = toRadians theta'
    lat     = toRadians lat0d
    long    = toRadians long0d
