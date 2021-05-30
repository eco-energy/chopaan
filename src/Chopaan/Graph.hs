{-# LANGUAGE GeneralizedNewtypeDeriving, UndecidableInstances, DeriveAnyClass, DerivingStrategies, StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings, NamedFieldPuns, ScopedTypeVariables, TypeApplications, FlexibleContexts, TypeOperators, GADTs #-}
{-# LANGUAGE FlexibleInstances, TypeFamilies, InstanceSigs
, ConstraintKinds, ScopedTypeVariables, QuantifiedConstraints
, RankNTypes, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving, GeneralizedNewtypeDeriving, DerivingStrategies, DerivingVia, DeriveFunctor, DeriveFoldable, DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase, TypeOperators, TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Chopaan.Graph (module Chopaan.Graph, module AG, module NG) where

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
import NetSpider.Snapshot
import NetSpider.Timestamp (Timestamp(..))
import qualified Algebra.Graph.Labelled as AG


import Chopaan.Node.NodeId
import Chopaan.Kibbutz.Mesh
import Chopaan.Node.Folds (SensorS)
import Chopaan.Kibbutz.Transactor (Stake, TransactionStatus)

import Shpadoinkle.Widgets.Types



deriving instance Generic Timestamp
deriving instance NFData Timestamp
deriving instance (NFData n, NFData a) => NFData (SnapshotNode n a)
deriving instance (NFData n, NFData e) => NFData (SnapshotLink n e)


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

fromSnapshot :: forall n l v. (Monoid l, Ord n) => SnapshotGraph n v l -> Gr l v
fromSnapshot g = Gr . AG.edges $ fmap (\(x, (_, y), (_, z)) -> (x, y, z)) $ castLinks g

castLinks :: forall n v l. (Monoid l, Ord n) => SnapshotGraph n v l -> [(l, (n, v), (n, v))]
castLinks (nodes, links) = (\l -> (linkAttributes l, sourceAttrs l, destAttrs l)) <$> links
  where
    nmap = Map.fromList $ zip (nodeId <$> nodes) (nodeAttributes <$> nodes)
    sourceAttrs l = (sourceNode l, fromJust $ nmap Map.! (sourceNode l))
    destAttrs l = (destinationNode l, fromJust $ nmap Map.! (destinationNode l))
    
newtype GrNode = GrNode Int
  deriving (Eq, Ord, Typeable, Show)
  deriving newtype (Num)


data SG where
  MeshSnapshot :: SnapshotGraph NodeMAC MeshNode RxSignal -> SG
  StakeSnapshot :: SnapshotGraph NodeMAC SensorS Stake -> SG
  StatusSnapshot :: SnapshotGraph NodeMAC SensorS TransactionStatus -> SG
  deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)


data GraphType = Mesh | Plan | Status | Power | Hardware
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Generic, ToJSON, FromJSON, NFData, Humanize)
