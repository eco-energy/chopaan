{-# LANGUAGE TypeOperators, TypeApplications, ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE FlexibleInstances, TypeFamilies, DataKinds #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving, DeriveGeneric, DerivingVia, DeriveAnyClass, DerivingStrategies #-}

{-# OPTIONS_GHC -fplugin-opt=ConCat.Plugin:showResiduals #-}

module Chopaan.Kibbutz.Allocate where

import Control.Monad ((>=>))
import ConCat.SMT
import ConCat.Misc
import ConCat.Rebox ()
import ConCat.AltCat (toCcc)
import ConCat.TArr

import qualified ConCat.Rep as Rep
import qualified ConCat.Circuit as G

import GHC.Generics
import GHC.TypeLits (KnownNat)

import Data.Vector.Sized (Vector)
import Data.Monoid


allocate :: (Monad m) => a -> m c
allocate = generate >=> clear

generate :: (Monad m) => a -> m b
generate = undefined

clear :: (Monad m) => b -> m c
clear = undefined

data VI = VI deriving (Eq, Ord, Show)

instance HasFin VI where
  type Card VI = 1

deriving instance (Eq a) => Eq (Arr n a)
deriving instance (Ord a) => Ord (Arr n a)
deriving instance (Show a) => Show (Arr n a)


newtype EnergyF = E { unE :: Ap (Arr VI) (Sum Double) }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num)

instance Rep.HasRep (EnergyF) where
  type Rep (EnergyF) = (Arr VI Double)
  repr = (getSum <$>) . getAp . unE
  abst = E . Ap . (Sum <$>)

instance G.GenBuses (EnergyF) where
  genBuses' = G.genBusesRep'
  ty = G.tyRep @(EnergyF)
  unflattenB' = G.genUnflattenB'

--instance (KnownNat n) => EvalE (Vector n Double)

-- MOVE THIS INSTANCE DEC TO CONCAT
instance EvalE (EnergyF) where
  evalE = undefined


type Reward = Double
{--
predicateValue :: (KnownNat n) => (EnergyF n, EnergyF n) -> Bool :* Reward
predicateValue (E (demand), E (supply)) = (all (\x -> x > 0) servicedDemand, getSum $ foldl (<>) 0 servicedDemand)
  where
    servicedDemand = (-) <$> demand <*> supply

predicate :: (KnownNat n) => (EnergyF n, EnergyF n) :* Reward -> Bool
predicate = predValToPred predicateValue

solution :: (KnownNat n) => [(EnergyF n, EnergyF n) :* Reward]
solution = solveAscending $ toCcc predicate

runSolution :: IO ()
runSolution = print $ solution @10
--}
