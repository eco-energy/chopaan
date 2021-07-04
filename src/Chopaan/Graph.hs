{-# LANGUAGE GeneralizedNewtypeDeriving, UndecidableInstances, DeriveAnyClass, DerivingStrategies, StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings, NamedFieldPuns, ScopedTypeVariables, TypeApplications, FlexibleContexts, TypeOperators, GADTs #-}
{-# LANGUAGE FlexibleInstances, TypeFamilies, InstanceSigs
, ConstraintKinds, ScopedTypeVariables, QuantifiedConstraints
, RankNTypes, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving, GeneralizedNewtypeDeriving, DerivingStrategies, DerivingVia, DeriveFunctor, DeriveFoldable, DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase, TypeOperators, TypeApplications, CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Chopaan.Graph ( module Chopaan.Graph
                     , module Chopaan.Graph.G
#ifndef ghcjs_HOST_OS
                     , module Chopaan.Graph.Spider
#endif
                     ) where

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

import Chopaan.Graph.Snapshot
import qualified Algebra.Graph.Labelled as AG

#ifndef ghcjs_HOST_OS
import Chopaan.Graph.Spider
import Chopaan.Graph.Kbtz
#endif

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
castLinks (nodes, links) = (\l -> (_linkAttributes l, sourceAttrs l, destAttrs l)) <$> links
  where
    nmap = Map.fromList $ zip (_nodeId <$> nodes) (_nodeAttributes <$> nodes)
    sourceAttrs l = (_sourceNode l, nmap Map.! (_sourceNode l))
    destAttrs l = (_destinationNode l, nmap Map.! (_destinationNode l))
    
newtype GrNode = GrNode Int
  deriving (Eq, Ord, Typeable, Show)
  deriving newtype (Num)


data GraphType = MeshG | PlanG | StatusG | FlowG
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Generic, ToJSON, FromJSON, NFData, Humanize)
