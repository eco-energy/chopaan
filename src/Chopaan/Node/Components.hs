{-# LANGUAGE GADTs, TypeOperators, DeriveGeneric, DeriveAnyClass #-}

module Chopaan.Node.Components where

import GHC.Generics
import ConCat.Pair
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON, FromJSON)

data BatteryConf a = BatteryConf
  deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)

data BatteryTop a = ParBC (BatteryConf a) (BatteryConf a)
                  | SeqBC (BatteryConf a) (BatteryConf a)
                  | SingBC (BatteryConf a)
                  deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)

type VI a = Pair a

type EvolveB a = (BatteryConf a -> VI a -> VI a)

data Battery a where
  ParB :: Battery a -> Battery a -> Battery a
  SeqB :: Battery a -> Battery a -> Battery a
  ABattery :: BatteryConf a -> EvolveB a -> Battery a
  deriving (Generic, NFData)


runBB :: (Fractional a) => Battery a -> VI a -> VI a
runBB (ParB a b) (v :# i) = combinePar (runBB a (v :# (i/2))) (runBB b (v :# (i/2)))
runBB (SeqB a b) (v :# i) = combineSeq (runBB a (v/2 :# i)) (runBB b (v/2 :# i))
runBB (ABattery bConf evolve) vi = evolve bConf vi 

combinePar :: VI a -> VI a -> VI a
combinePar = undefined

combineSeq :: VI a -> VI a -> VI a
combineSeq = undefined

data PVConf a = PVConf
  deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)

data PVTop a = ParPC (PVConf a) (PVConf a)
             | SeqPC (PVConf a) (PVConf a)
             | SingPC (PVConf a)
             deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)


data PVEnv a = PVEnv deriving (Eq, Ord, Show, Generic, NFData)

type EvolvePV a = (PVConf a -> PVEnv a -> VI a)

data PV a where
  ParPV :: PV a -> PV a -> PV a
  SeqPV :: PV a -> PV a -> PV a
  APV :: PVConf a -> EvolvePV a -> PV a
  deriving (Generic, NFData)

-- $ PVEnv is time, location and temperature.
-- $ The evolve function must determine what the output voltage and current is
runPV :: (Fractional a) => PV a -> PVEnv a -> VI a
runPV (ParPV a b) p = combinePar (runPV a p) (runPV b p)
runPV (SeqPV a b) p = combineSeq (runPV a p) (runPV b p)
runPV (APV bConf evolve) p = evolve bConf p


data LoadConf a = LoadConf
  deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)

data Load a where
  ParLoad :: Load a -> Load a -> Load a
  ALoad :: LoadConf a -> Load a
  deriving (Generic, NFData, ToJSON, FromJSON)


data LoadTop a = ParLC (LoadConf a) (LoadConf a)
               | SingLC (LoadConf a)
  deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)
  
