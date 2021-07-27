{-# LANGUAGE KindSignatures, TypeOperators, DataKinds, FlexibleContexts, TypeFamilies, FlexibleInstances, LambdaCase, TypeApplications, ScopedTypeVariables, MultiParamTypeClasses, UndecidableInstances, InstanceSigs, RecordWildCards #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving
, DerivingStrategies, DeriveAnyClass, StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints, InstanceSigs #-}
module Chopaan.CRUD where

import GHC.Generics

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON, FromJSON)
import Data.Maybe (fromMaybe)
import Data.Function (on)

import Shpadoinkle (Html, MonadJSM)
import Shpadoinkle.Widgets.Table (Tabular(..), Column, Row, SortCol(..), Sort(..))
import Shpadoinkle.Widgets.Types (Humanize (..), Present(present))

import Streamly (IsStream, adapt)
import Streamly.Internal.Prelude (hoist)

import Control.Monad.Trans.Class
import Chopaan.Node.NodeId
import Chopaan.Node.NodeT
import Chopaan.Kibbutz.KbtzimT
import Chopaan.Kibbutz.KbtzId
import Chopaan.Graph
import Data.Time


class CRUDChopaan m where
  listKibbutzim :: m (KbtzList)
  listNodezim :: KbtzName -> m (NodeList)
  getGraph :: forall t. IsStream t => KbtzName -> GraphType -> UTCTime -> UTCTime -> t m (SG NodeMAC)
  --sensorMonitor :: (IsStream t) => NodeMAC -> t m SensorR

instance (MonadTrans t, Monad m, CRUDChopaan m, Monad (t m)) => CRUDChopaan (t m) where
  listKibbutzim = lift listKibbutzim
  listNodezim = lift . listNodezim
  getGraph :: forall t'. (IsStream t') => KbtzName -> GraphType -> UTCTime -> UTCTime -> t' (t m) (SG NodeMAC)
  getGraph k g t0 t1 = adapt . hoist lift $ getGraph k g t0 t1
  --nodeDetails = lift . nodeDetails
  --sensorMonitor :: (IsStream t') => NodeMAC -> t' (t m) SensorR
  --sensorMonitor = lift . sensorMonitor

newtype NodeList = NodeList { unNodeList :: [Nodezim] }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (NFData, ToJSON, FromJSON)

data instance Column NodeList = NId | NMac | NHW
  deriving (Eq, Ord, Show, Bounded, Enum, Generic, NFData, ToJSON, FromJSON)

newtype instance Row NodeList = NodezimRow { unNodezimRow :: Nodezim }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (NFData)

instance Humanize (Column NodeList) where
  humanize = \case
    NId -> "Node Id"
    NMac -> "MAC Address"
    NHW -> "Hardware Configuration"


instance Tabular NodeList where
  type Effect NodeList m = (MonadJSM m, CRUDChopaan m)
  toRows = (fmap NodezimRow) . unNodeList
  toCell :: forall m. Effect NodeList m
    => NodeList
    -> Row NodeList
    -> Column NodeList
    -> [Html m NodeList]
  toCell _ (NodezimRow Node{..}) = \case
    NId -> present (show <$> _nodeId)
    NMac -> present _nodeMAC
    NHW -> present _hardwareConfig
  sortTable (SortCol c d) = f $ case c of
    NId -> g _nodeId
    NMac -> g _nodeMAC
    NHW -> g _hardwareConfig
    where
      f = case d of
        ASC -> id
        DESC -> flip
      g l = compare `on` l . unNodezimRow


newtype KbtzList = KbtzList { unKbtzList :: [Kbtzim] }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (NFData, ToJSON, FromJSON)

data instance Column KbtzList = KId | KName | KDesc
  deriving (Eq, Ord, Show, Generic, Bounded, Enum, NFData, ToJSON, FromJSON)

newtype instance Row KbtzList = KbtzimRow { unKbtzimRow :: Kbtzim }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (NFData)

instance Humanize (Column KbtzList) where
  humanize = \case
    KId -> "Kibbutz Id"
    KName -> "Name"
    KDesc -> "Description"


instance Tabular KbtzList where
  type Effect KbtzList m = (MonadJSM m)
  toRows = (fmap KbtzimRow) . unKbtzList
  toCell :: forall m. Effect KbtzList m
    => KbtzList
    -> Row KbtzList
    -> Column KbtzList
    -> [Html m KbtzList]
  toCell _ (KbtzimRow Kbtzim {..}) = \case
    KId -> present (show <$> _kbtzId)
    KName -> present _kbtzName
    KDesc -> present . fromMaybe "No Description Available" $ _kbtzDesc
  sortTable (SortCol c d) = f $ case c of
    KId -> g _kbtzId
    where
      f = case d of
        ASC -> id
        DESC -> flip
      g l = compare `on` l . unKbtzimRow
