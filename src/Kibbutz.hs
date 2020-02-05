{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{--

What is a kibbutz?
- Shared ownership of the means of production.


--}



module Kibbutz where

import Streamly
import Streamly.Prelude
import ConCat.Complex
import ConCat.Free.Affine
import ConCat.Circuit
import Node
import GHC.Generics (Generic)
--import Mgenv


data LCirc l v

data VI v

data AffLegRel v

class (IsStream t, Monad m, Monoid l) => Kibbutz t m l v where
  lCirc :: t m (LCirc l v)
  outputs :: t m (VI v)
  inputs :: t m (VI v)
  blackbox :: LCirc l v -> AffLegRel v



data LCirc'' k v where
  Resistor :: k -> v -> LCirc'' k v
  Inductor :: k -> v -> LCirc'' k v
  Capacitor :: k -> v -> LCirc'' k v
  VoltageSource :: k -> v -> LCirc'' k v
  CurrentSource :: k -> v -> LCirc'' k v
  Series :: (k, v) -> (k', v') -> LCirc'' (k, k') (v, v')
  Parallel :: k -> k -> v -> v -> (LCirc'' k v)


instance HasRep 

instance GenBuses (LCirc'' l v) where
  genBuses' template sources = do
    x <- genBusesRep' template sources
    return x
  

--blackbox :: (HasVector) => LCirc'' -> AffRelu


-- household = ((generationC --:> storageC) --:> consumptionC) --:> txC

