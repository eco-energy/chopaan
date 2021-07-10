{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveDataTypeable, StandaloneDeriving, GADTs #-}
{-# LANGUAGE FlexibleInstances, TypeOperators, TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, QuantifiedConstraints, AllowAmbiguousTypes, UndecidableInstances #-}
{-# LANGUAGE TypeFamilies, RankNTypes, CPP, PackageImports #-}

module Chopaan.Graph.G where

import Prelude hiding (id, (.), curry, uncurry)
import GHC.Generics (Generic)

#ifndef ghcjs_HOST_OS
import ConCat.Category
import ConCat.Misc
#endif


import Control.DeepSeq (NFData)

import Data.Aeson
import Data.Typeable
import Data.Bifunctor


import Chopaan.Node.Folds
import Chopaan.Node.Metrics
import Chopaan.Node.Mesh
import Chopaan.Kibbutz.Transactor
import Chopaan.Graph.Snapshot

#ifndef ghcjs_HOST_OS
import NetSpider.Spider.Config
import Data.Pool
import NetSpider.Spider
#endif




data G k n = Mesh (k n MeshNode RxSignal) -- -> G k n
           | Transactor (k n Stake TxStatus) -- -> G k n 
           | Status (k n SensorR Stake)
           | Flow (k n BatteryR PowerNR)
           deriving (Generic)


deriving instance (forall a b. (Eq a, Eq b) => Eq (k n a b)) => Eq (G k n)

deriving instance (forall a b. (Eq a, Eq b) => Eq (k n a b), forall a b. (Ord a, Ord b) => Ord (k n a b)) => Ord (G k n)

deriving instance (forall a b. (ToJSON a, ToJSON b) => ToJSON (k n a b)) => ToJSON (G k n)

deriving instance (forall a b. (FromJSON a, FromJSON b) => FromJSON (k n a b)) => FromJSON (G k n)

deriving instance (forall a b. (NFData a, NFData b) => NFData (k n a b)) => NFData (G k n)

deriving instance (forall a b. (Show a, Show b) => Show (k n a b)) => Show (G k n)



newtype SG' n v e = SG { unSnapshot :: SnapshotGraph n v e }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, NFData)

type SG n = G SG' n

#ifndef ghcjs_HOST_OS
newtype CG' n v e = CG { unConf :: Config n v e }
  deriving (Generic)

newtype SpoolG' n v e = SpoolG { unSpool :: Pool (Spider n v e) }

type SnapshotG = G' SG'

type SpoolG = G' SpoolG'

type ConfG n = G'' CG' n

type SG'' n = G'' SG' n 

type SpG'' n = G'' SpoolG' n

type SnGr n v e = G' SG' n v e

type CGr n v e = G' CG' n v e

type SpGr n v e = G' SpoolG' n v e

#endif

data G'' k n = G''
  { meshG :: k n MeshNode RxSignal
  , txG :: k n Stake TxStatus
  , statusG :: k n SensorR Stake
  , flowG :: k n BatteryR PowerNR
  } deriving (Generic)




data G' k n v e where
  Mesh' :: k n MeshNode RxSignal -> G' k n MeshNode RxSignal
  Transactor' :: k n Stake TxStatus -> G' k n Stake TxStatus
  Status' :: k n SensorR Stake -> G' k n SensorR Stake
  Flow' :: k n BatteryR PowerNR -> G' k n BatteryR PowerNR
