{-# LANGUAGE GeneralizedNewtypeDeriving, UndecidableInstances, DeriveAnyClass, DerivingStrategies, StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings, NamedFieldPuns, ScopedTypeVariables, TypeApplications, FlexibleContexts, TypeOperators, GADTs #-}
{-# LANGUAGE FlexibleInstances, TypeFamilies, InstanceSigs
, ConstraintKinds, ScopedTypeVariables, QuantifiedConstraints
, RankNTypes, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving, GeneralizedNewtypeDeriving, DerivingStrategies, DerivingVia, DeriveFunctor, DeriveFoldable, DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase, TypeOperators, TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Chopaan.Graph (module Chopaan.Graph, module Chopaan.Graph.G, module Chopaan.Graph.Spider) where

import Prelude hiding ((.), id)

import GHC.Generics (Generic, Generic1)
import qualified Control.Newtype.Generics as N
import Control.DeepSeq (NFData)

import Control.Category
import Data.Aeson
import Data.Typeable
import Data.Bifunctor
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map


import NetSpider.Graph as NG (NodeAttributes(..), LinkAttributes(..), VFoundNode)
import NetSpider.Spider.Config
import NetSpider.Snapshot
import NetSpider.Timestamp (Timestamp(..))
import qualified Algebra.Graph.Labelled as AG


import Chopaan.Node.NodeId
import Chopaan.Node.Mesh
import Chopaan.Node.Folds (SensorR)
import Chopaan.Node.Metrics hiding (Timestamp)
import Chopaan.Node.NodeSensors
import Chopaan.Kibbutz.Transactor (Stake, TxStatus)
import Chopaan.Graph.Spider
import Chopaan.Graph.Kbtz
import Chopaan.Graph.Greskell
import Shpadoinkle.Widgets.Types

import Chopaan.Graph.G

type R = Double


-- $ Constraints for edge labels and nodes
type GrConn f s = (Bounded s, Show s, Ord s, Eq s, Enum s, Show f, Monoid f, Ord f)

-- $ Constraints for edge labels and nodes, along with monad constraints
type GrConnM m f s = (Monad m, GrConn f s)

deriving instance Generic1 (AG.Graph flow)

newtype Gr flow state = Gr { unGr :: (AG.Graph flow state) }
  deriving stock (Eq, Ord, Show, Generic, Generic1)
  deriving newtype (Num, Functor, Bifunctor)


emptyGr :: (GrConn flow state) => Gr flow state
emptyGr = Gr AG.empty


instance N.Newtype (Gr flow state)

-- $ Shpadoinkle Instances
instance (Show state, Show flow) => Humanize (Gr flow state)

fromSnapshot :: forall n l v. (Monoid l, Ord n) => SnapshotGraph n v l -> Gr l (Maybe v)
fromSnapshot g = Gr . AG.edges $ fmap (\(x, (_, y), (_, z)) -> (x, y, z)) $ castLinks g

castLinks :: forall n v l. (Monoid l, Ord n) => SnapshotGraph n v l -> [(l, (n, Maybe v), (n, Maybe v))]
castLinks (nodes, links) = (\l -> (linkAttributes l, sourceAttrs l, destAttrs l)) <$> links
  where
    nmap = Map.fromList $ zip (nodeId <$> nodes) (nodeAttributes <$> nodes)
    sourceAttrs l = (sourceNode l, nmap Map.! (sourceNode l))
    destAttrs l = (destinationNode l, nmap Map.! (destinationNode l))
    
newtype GrNode = GrNode Int
  deriving (Eq, Ord, Typeable, Show)
  deriving newtype (Num)


data GraphType = MeshG | PlanG | StatusG | FlowG
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Generic, ToJSON, FromJSON, NFData, Humanize)



  
