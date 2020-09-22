{-# LANGUAGE TypeOperators, TypeApplications, ScopedTypeVariables, RankNTypes #-}
module Chopaan.Kibbutz.Allocate where

import Control.Monad ((>=>))
import ConCat.SMT
import ConCat.Misc
import ConCat.Rebox ()
import ConCat.AltCat (toCcc)

import GHC.Generics
import GHC.TypeLits (KnownNat)

import qualified Data.Map as M
import Data.Vector.Sized (Vector)


allocate :: (Monad m) => a -> m c
allocate = generate >=> clear

generate :: (Monad m) => a -> m b
generate = undefined

clear :: (Monad m) => b -> m c
clear = undefined



type EnergyMap n = Vector n Double

type Reward = Double

predicateValue :: (KnownNat n) => EnergyMap n -> Bool :* Reward
predicateValue = undefined

predicate :: (KnownNat n) => EnergyMap n :* Reward -> Bool
predicate = predValToPred predicateValue

solution :: (KnownNat n) => [EnergyMap n :* Reward]
solution = solveAscending $ toCcc predicate
