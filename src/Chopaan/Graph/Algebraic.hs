{-# LANGUAGE ConstraintKinds, ExplicitForAll #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, GeneralizedNewtypeDeriving, DerivingStrategies, StandaloneDeriving, DerivingVia, DeriveFoldable, DeriveTraversable, QuantifiedConstraints, InstanceSigs #-}
module Chopaan.Graph.Algebraic (module AG, GrConn, Gr, fromSnapshot, castLinks) where

import GHC.Generics ( Generic, Generic1, Rep, Rep1 )
import Control.Newtype.Generics as N ( Newtype )
import Data.Bifunctor
import Data.Maybe ( fromJust, isJust )
import Data.Typeable ( Typeable )
import Data.Aeson (ToJSON, FromJSON)
import qualified Codec.Winery as W

import qualified Data.Map as Map
import Chopaan.Graph.Snapshot
    ( SnapshotNode(_nodeId, _nodeAttributes),
      SnapshotLink(_sourceNode, _destinationNode, _linkAttributes),
      SnapshotGraph )
-- import Shpadoinkle.Widgets.Types (Humanize)
import  Algebra.Graph.Labelled as AG
import qualified Algebra.Graph as G


-- $ Constraints for edge labels and nodes
type GrConn f s = (Bounded s, Show s, Ord s, Eq s, Enum s, Show f, Monoid f, Ord f)

-- $ Constraints for edge labels and nodes, along with monad constraints
type GrConnM m f s = (Monad m, GrConn f s)

deriving instance Foldable (G.Graph)
deriving instance Traversable (G.Graph)
deriving instance (FromJSON a) => FromJSON (G.Graph a)
deriving instance (ToJSON a) => ToJSON (G.Graph a)
deriving via (W.WineryVariant (G.Graph a)) instance (W.Serialise a) => W.Serialise (G.Graph a)


deriving instance Generic1 (AG.Graph flow)

deriving instance Foldable (AG.Graph a)
deriving instance Traversable (AG.Graph a)
deriving instance (FromJSON e, FromJSON a) => FromJSON (AG.Graph e a)
deriving instance (ToJSON e, ToJSON a) => ToJSON (AG.Graph e a)
deriving via (W.WineryVariant (AG.Graph e a)) instance (W.Serialise e, W.Serialise a) => W.Serialise (AG.Graph e a)


newtype Gr flow state = Gr { unGr :: (AG.Graph flow state) }
  deriving stock (Eq, Ord, Show, Generic, Generic1, Foldable, Traversable)
  deriving newtype (Num, Functor, Bifunctor, ToJSON, FromJSON, W.Serialise)

emptyGr :: (GrConn flow state) => Gr flow state
emptyGr = Gr AG.empty


instance N.Newtype (Gr flow state)


fromSnapshot :: forall n l v. (Monoid l, Ord n) => SnapshotGraph n v l -> Gr l (n, v)
fromSnapshot = Gr . AG.edges . castLinks

castLinks :: forall n v l. (Monoid l, Ord n) => SnapshotGraph n v l -> [(l, (n, v), (n, v))]
castLinks (nodes, links) = filterJust $ (\l -> (_linkAttributes l, sourceAttrs l, destAttrs l)) <$> links
  where
    filterJust = fmap (\(a, x, y) -> (a, second fromJust x, second fromJust y))
                 . filter (\(_, (_, v), (_, v')) -> (isJust v && isJust v'))
    nmap = Map.fromList $ zip (_nodeId <$> nodes) (_nodeAttributes <$> nodes)
    sourceAttrs l = (_sourceNode l, nmap Map.! (_sourceNode l))
    destAttrs l = (_destinationNode l, nmap Map.! (_destinationNode l))
    
newtype GrNode = GrNode Int
  deriving (Eq, Ord, Typeable, Show)
  deriving newtype (Num)
