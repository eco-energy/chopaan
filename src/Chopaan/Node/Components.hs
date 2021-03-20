{-# LANGUAGE GADTs, TypeOperators #-}

module Chopaan.Node.Components where

import ConCat.Pair


data BatteryConf a = BatteryConf

type VI a = Pair a

type EvolveB a = (BatteryConf a -> VI a -> VI a)

data Battery a where
  ParB :: Battery a -> Battery a -> Battery a
  SeqB :: Battery a -> Battery a -> Battery a
  ABattery :: BatteryConf a -> EvolveB a -> Battery a


runBB :: (Fractional a) => Battery a -> VI a -> VI a
runBB (ParB a b) (v :# i) = combinePar (runBB a (v :# (i/2))) (runBB b (v :# (i/2)))
runBB (SeqB a b) (v :# i) = combineSeq (runBB a (v/2 :# i)) (runBB b (v/2 :# i))
runBB (ABattery bConf evolve) vi = evolve bConf vi 

combinePar :: VI a -> VI a -> VI a
combinePar = undefined

combineSeq :: VI a -> VI a -> VI a
combineSeq = undefined

data PVConf a = PVConf

data PVEnv a = PVEnv

type EvolvePV a = (PVConf a -> PVEnv a -> VI a)

data PV a where
  ParPV :: PV a -> PV a -> PV a
  SeqPV :: PV a -> PV a -> PV a
  APV :: PVConf a -> EvolvePV a -> PV a

-- $ PVEnv is time, location and temperature.
-- $ The evolve function must determine what the output voltage and current is
runPV :: (Fractional a) => PV a -> PVEnv a -> VI a
runPV (ParPV a b) p = combinePar (runPV a p) (runPV b p)
runPV (SeqPV a b) p = combineSeq (runPV a p) (runPV b p)
runPV (APV bConf evolve) p = evolve bConf p
