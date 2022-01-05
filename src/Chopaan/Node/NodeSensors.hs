{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia, StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances, TypeOperators #-}
module Chopaan.Node.NodeSensors (power, NodeT'(..), fromNodeMessage
                                , NodeSensors, T, I, V, Res, i, v, t, p, e) where

import qualified Prelude as P

import qualified Proto.NodeMessageSchema.NodeMessages as NM
import qualified Proto.NodeMessageSchema.NodeMessages_Fields as NM

import Data.Monoid ( (<>), Monoid(mempty) )

import GHC.Generics ( Generic )

import Numeric.Units.Dimensional.Prelude
      
import Control.DeepSeq ( NFData )
import Data.Aeson (ToJSON, FromJSON)
import Control.Monad ()
import Control.Monad.State ( State )
import Algebra.Graph.Labelled as G ( edges, Graph )
-- Conversions and Accessors
import Data.Time ( UTCTime, diffUTCTime )
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Lens.Micro ( (^.) )
import ConCat.Misc
import qualified Codec.Winery as W

import qualified Streamly.Prelude as S

import Control.Monad.Bayes.Class



deriving via (W.WineryVariant (Quantity DTime s)) instance (W.Serialise s) => W.Serialise (Quantity DTime s)
deriving via (W.WineryVariant (ElectricPotential s)) instance (W.Serialise s) => W.Serialise (ElectricPotential s)
deriving via (W.WineryVariant (ElectricCurrent s)) instance (W.Serialise s) => W.Serialise (ElectricCurrent s)
deriving via (W.WineryVariant (ElectricResistance s)) instance (W.Serialise s) => W.Serialise (ElectricResistance s)

deriving anyclass instance ToJSON s => ToJSON (T s)
deriving anyclass instance FromJSON s => FromJSON (T s)
deriving anyclass instance ToJSON s => ToJSON (I s)
deriving anyclass instance FromJSON s => FromJSON (I s)
deriving anyclass instance ToJSON s => ToJSON (V s)
deriving anyclass instance FromJSON s => FromJSON (V s)
deriving anyclass instance ToJSON s => ToJSON (Res s)
deriving anyclass instance FromJSON s => FromJSON (Res s)


type T s = Quantity DTime s
type I s = ElectricCurrent s
type V s = ElectricPotential s
type Res s = ElectricResistance s

v :: (Num s) => s -> V s
v = (*~ volt)

i :: (Num s) => s -> I s
i = (*~ ampere)

r :: (Num s) => s -> Res s
r = (*~ ohm)

t :: (Num s) => s -> Time s
t = (*~ second)

p :: (Num s) => V s -> I s -> Power s
p i v = i * v

e :: (Num s) => Power s -> T s -> Energy s
e = (*)


type St u v = forall t m. (S.IsStream t, Monad m) => t m (u v)

toDist :: forall m a. (MonadSample m, Double ~ a) => (a, a) -> a -> m a
toDist (mean, std) a = do
  noise <- normal mean std
  return $ a P.+ noise


-- Streams of Sensors
type V' s = St Voltage s
type I' s = St Current s
type P' s = St Power s
type E' s = St Energy s

type Vt t s = V' (t, s)
type It t s = I' (t, s)
type Pt t s = P' (t, s)
type Et t s = E' (t, s)

time :: (Num s) => s -> T s
time = t

volts :: (Num s) => s -> Voltage s
volts = Voltage . v

amps :: (Num s) => s -> Current s
amps = Current . i

power :: (Num s) => I' s -> V' s -> P' s
power = S.zipWith (\(Current i) (Voltage v) -> p v i)

--energy :: (Num s) => P' s -> E' s
--energy = S.postscan (FL.sum P.* FL.product) 

newtype Voltage s = Voltage (V s) deriving (Eq, Ord, Show, Generic)
newtype Current s = Current (I s) deriving (Eq, Ord, Show, Generic)

-- data Bus s where
--   EmptyBus :: Bus s
--   GenBus :: V s -> I s -> Bus s
--   StorageBus :: V s -> Bus s
--   TxBus :: V s -> I s -> Bus s
--   LoadBus :: I s -> V s -> Bus s
--   ComposeBus ::  Bus s -> Bus s -> Bus s
--   ParallelBus :: Bus s :* Bus s -> Bus (s :* s)

-- instance Semigroup (Bus s) where
--   a <> b = ComposeBus a b

-- instance Monoid (Bus s) where
--   mempty = EmptyBus



newtype Circuit s = Circuit {
  runCircuit :: G.Graph (Component (V s) (I s)) (V s, I s)
  } deriving (Generic)

type CircuitM s = State Int (Circuit s)


type Component v i = Unop (v :* i)

-- fromSensors :: forall s. (Num s) => NodeSensors s -> Circuit s
-- fromSensors NodeSensors{..} = Circuit $ G.edges es
--   where
--     es = [(GenBus genVoltage genCurrent, GenBus genVoltage, sv)
--          , (TxBus batteryVoltage gridCurrent, TxBus gridVoltage gridCurrent, sv)
--          , (LoadBus loadCurrent batteryVoltage, LoadBus loadCurrent 0, sv)
--          ]
--       where
--         sv = StorageBus batteryVoltage

data NodeSensors s = NodeSensors
  { batteryVoltage :: !(V s)
  , gridVoltage :: !(V s)
  , loadCurrent :: !(I s)
  , gridCurrent :: !(I s)
  , genCurrent :: !(I s)
  , genVoltage :: !(V s)
  , temperature :: !s
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (W.Serialise) via (W.WineryRecord (NodeSensors s))
  deriving anyclass (ToJSON, FromJSON, NFData)

getBuses :: NodeSensors s -> IO ()
getBuses = undefined

newtype NodeT' s = NodeT' (UTCTime, NodeSensors s)
  deriving (Eq, Ord, Show, Generic)
  deriving W.Serialise via (W.WineryVariant (NodeT' s))
  deriving anyclass (ToJSON, FromJSON, NFData)

-- $ converts an EnergyState autogenerated from a protobuf to a NodeSensors that we control
-- $ Incoming current is negative, outgoing is positive
fromNodeMessage :: NM.EnergyState -> NodeT' R
fromNodeMessage nm = NodeT' (nodeTimeToUTC nm, NodeSensors
                       { batteryVoltage = v $ nm ^. NM.batteryVoltage
                       , gridVoltage = v $ nm ^.  NM.gridVoltage
                       , loadCurrent = i $ nm ^.  NM.batteryToLoadCurrent
                       , gridCurrent = cOut + cIn
                       , genCurrent = i $ nm ^. NM.solarInputCurrent
                       , temperature = nm ^. NM.temperature
                       , genVoltage = genV
                       })
  where
    genV = i (nm ^. NM.solarInputCurrent) * solarRes + v (nm ^. NM.batteryVoltage)
    solarRes :: Res R
    solarRes = r 1
    cOut :: I R
    cOut = i $ nm ^.  NM.batteryToGridCurrent
    cIn :: I R
    cIn = i $ (-1) P.* (nm ^. NM.gridToBatteryCurrent)

{--------------------------------------------------------------------------------------------
                                   Units
--------------------------------------------------------------------------------------------}

newtype Terminal s = Terminal (Voltage s, Current s) deriving (Eq, Ord, Show, Generic)

data BusP s = Gen (Terminal s)
            | Storage (Terminal s)
            | Tx (Terminal s)
            | Load (Terminal s)
            deriving (Eq, Ord, Show, Generic)

-- $ converts the millisecond timestamp in the EnergyState to a UTITime  
nodeTimeToUTC :: NM.EnergyState -> UTCTime
nodeTimeToUTC es = posixSecondsToUTCTime (fromIntegral (es ^. NM.cpuTime))

diffUTC :: forall s. (Fractional s) => UTCTime -> UTCTime -> T s
diffUTC a b = t . realToFrac $ diffUTCTime a b
