{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveDataTypeable, StandaloneDeriving, DerivingStrategies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances, TypeOperators, TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, QuantifiedConstraints, AllowAmbiguousTypes, UndecidableInstances, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies, RankNTypes, CPP, PackageImports, GADTs, LambdaCase, OverloadedLabels #-}

module Chopaan.Graph.G where

import Prelude hiding (id, (.), curry, uncurry, const)
import GHC.Generics (Generic)
import Data.Generics.Sum
import Data.Generics.Labels
import qualified Algebra.Graph.Labelled as G

import Control.Arrow
import Control.Lens (preview)

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

-- import Shpadoinkle.Widgets.Types (Humanize)
#ifndef ghcjs_HOST_OS
import NetSpider.Spider.Config
import Data.Pool
import NetSpider.Spider
#endif


data GraphType = MeshG | PlanG | StatusG | FlowG
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Generic, ToJSON, FromJSON, NFData)


data G k n = Mesh (k n MeshNode RxSignal) -- -> G k n
           | Transactor (k n Stake TxStatus) -- -> G k n 
           | Status (k n SensorR Stake)
           | Flow (k n BatteryR PowerNR)
           deriving (Generic)

consMap :: GraphType -> ((forall a b. k n a b) -> G k n)
consMap MeshG = Mesh
consMap PlanG = Transactor
consMap StatusG = Status
consMap FlowG = Flow

--prismMap :: GraphType -> ((forall a b. k n a b) -> G k n)
prismMap MeshG = #_Mesh
prismMap PlanG = #_Transactor
prismMap StatusG = #_Status
prismMap FlowG = #_Flow

mesh :: G k n -> Maybe (k n MeshNode RxSignal)
mesh = preview #_Mesh
transactor :: G k n -> Maybe (k n Stake TxStatus)
transactor = preview #_Transactor
status :: G k n -> Maybe (k n SensorR Stake)
status = preview #_Status
flow :: G k n -> Maybe (k n BatteryR PowerNR)
flow = preview #_Flow


deriving instance (forall a b. (Eq a, Eq b) => Eq (k n a b)) => Eq (G k n)

deriving instance (forall a b. (Eq a, Eq b) => Eq (k n a b), forall a b. (Ord a, Ord b) => Ord (k n a b)) => Ord (G k n)

deriving instance (forall a b. (ToJSON a, ToJSON b) => ToJSON (k n a b)) => ToJSON (G k n)

deriving instance (forall a b. (FromJSON a, FromJSON b) => FromJSON (k n a b)) => FromJSON (G k n)

deriving instance (forall a b. (NFData a, NFData b) => NFData (k n a b)) => NFData (G k n)

deriving instance (forall a b. (Show a, Show b) => Show (k n a b)) => Show (G k n)

newtype SG' n v e = SG { unSnapshot :: SnapshotGraph n v e }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)
  deriving newtype (Semigroup, Monoid)

newtype Gr' n v e = Gr' { unGr' :: G.Graph e (n, v) }

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

type KbtzGraph = G' 

#endif

data G'' k n = G''
  { meshG :: k n MeshNode RxSignal
  , txG :: k n Stake TxStatus
  , statusG :: k n SensorR Stake
  , flowG :: k n BatteryR PowerNR
  } deriving (Generic)




data G' k n v e where
  Id' :: G' k n v v
  Compose' :: G' k n w x -> G' k n x y -> G' k n w y
  Product' :: G' k n w x -> G' k n w y -> G' k n w (x :* y)
  Sum' :: G' k n w x -> G' k n w y -> G' k n w (x :+ y)
  Exl' :: G' k n (x :* y) x
  Exr' :: G' k n (x :* y) y
  Mesh' :: k n MeshNode RxSignal -> G' k n MeshNode RxSignal
  Transactor' :: k n TxStatus Stake  -> G' k n TxStatus Stake
  Status' :: k n SensorR Stake -> G' k n SensorR Stake
  Flow' :: k n BatteryR PowerNR -> G' k n BatteryR PowerNR

instance Category (G' Gr' n) where
  id = Id'
  x . y = Compose' y x


instance ProductCat (G' Gr' n) where
  dup = Product' Id' Id'
  exl = Exl'
  exr = Exr'

instance MonoidalPCat (G' Gr' n) where
  -- dup = Product' Id' Id'
  -- exl = Exl'
  -- exr = Exr'


instance BraidedPCat (G' Gr' n)


evalG :: G' Gr' n v e -> ()
evalG Id' = ()
evalG (Compose' i j) = const () (compositionG i j)
evalG (Product' i j) = const () (prodG i j)
evalG (Sum' i j) = ()
evalG (Exl') = ()
evalG (Exr') = ()
evalG (Mesh' g) = ()
evalG (Transactor' g) = ()
evalG (Status' g) = ()
evalG (Flow' g) = ()

compositionG ::  G' Gr' n w x -> G' Gr' n x y -> G' Gr' n w y
compositionG = undefined

prodG ::  G' Gr' n w x -> G' Gr' n w y -> G' Gr' n w (x :* y)
prodG = undefined

