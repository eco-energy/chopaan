{-# LANGUAGE ScopedTypeVariables, TypeApplications, FlexibleInstances, TypeOperators, TypeFamilies, FlexibleContexts, ConstraintKinds, InstanceSigs #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving,  DerivingStrategies, GeneralizedNewtypeDeriving, DeriveAnyClass, QuantifiedConstraints, OverloadedStrings #-}

module Chopaan.Ui.Tables where

import GHC.Generics

import Control.DeepSeq (NFData)

import Data.Function (on)
import Data.Int (Int64)
import qualified Data.Map.Strict as M
import qualified Data.Text as Txt
import Text.Printf (printf)
import Data.Aeson
import Data.Time (UTCTime(..), formatTime, defaultTimeLocale)

import Shpadoinkle
import qualified Shpadoinkle.Html as H
import qualified Shpadoinkle.Widgets.Table as T
import qualified Shpadoinkle.Widgets.Types as T

import Chopaan.Node.Metrics
import Chopaan.Node.Mesh

newtype MeshTable n = MeshTable { unMeshTable :: (M.Map n (MeshNode, RxSignal)) }
  deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)

data instance T.Column (MeshTable n) = MId
                                     | MTime
                                     | MConnStrength
                                     | MParent
                                     | MIsRoot
                                     | MRouterRSSI
                                     | MUpTime
                                     | MVersion
                                     deriving (Eq, Ord, Show, Generic, Enum, Bounded, NFData, ToJSON, FromJSON)

data instance T.Row (MeshTable n) = MeshRow { unMeshRow :: (n, (MeshNode, RxSignal)) }

instance T.Humanize (T.Column (MeshTable n)) where
  humanize MId = "Node Id"
  humanize MTime = "Node Time"
  humanize MConnStrength = "Parent RSSI"
  humanize MParent = "Parent Id"
  humanize MIsRoot = "Is Root"
  humanize MRouterRSSI = "Router RSSI"
  humanize MVersion = "Software Version"
  humanize MUpTime = "Node Uptime"

instance T.Humanize UTCTime where
  humanize a = Txt.pack $ formatTime defaultTimeLocale "%H:%M:%S %d-%m-%y" a

instance (T.Humanize a) => T.Humanize (Maybe a) where
  humanize Nothing = "Not Available"
  humanize (Just a) = T.humanize a

instance T.Humanize (Double) where
  humanize = Txt.pack . printf "%.2g"

instance T.Humanize (Bool) where
  humanize True = "Yes"
  humanize False = "No"

instance T.Humanize (Int) where
  humanize = Txt.pack . printf "%d"

instance T.Humanize (Int64) where
  humanize = Txt.pack . printf "%d"

instance (T.Humanize n, Ord n) => T.Tabular (MeshTable n) where
  type Effect (MeshTable n) m = (MonadJSM m)
  toRows :: MeshTable n -> [T.Row (MeshTable n)]
  toRows (MeshTable t) = MeshRow <$> (M.toList t)
  toCell :: MeshTable n
                  -> T.Row (MeshTable n)
                  -> T.Column (MeshTable n)
                  -> [H.Html m (MeshTable n)] 
  toCell _ (MeshRow (n, (mnode, sig))) c = case c of
    MId -> humRow n
    MTime -> humRow $ nodeTime mnode
    MConnStrength -> humRow $ strength sig
    MParent -> humRow $ parent sig
    MIsRoot -> humRow $ isRoot mnode
    MRouterRSSI -> humRow $ routerRSSI mnode
    MVersion -> humRow $ version mnode
    MUpTime -> humRow $ uptime mnode
    
  sortTable :: T.SortCol (MeshTable n)
    -> T.Row (MeshTable n)
    -> T.Row (MeshTable n) -> Ordering
  sortTable (T.SortCol c s) = f $ case c of
    MId -> g fst
    MTime -> g $ nodeTime . fst . snd
    MConnStrength -> g $ strength . snd . snd
    MParent -> g $ parent . snd . snd
    MIsRoot -> g $ isRoot . fst . snd
    MRouterRSSI -> g $ routerRSSI . fst . snd
    MVersion -> g $ version . fst . snd
    MUpTime -> g $ uptime . fst . snd
    where
      f = case s of
        T.ASC -> flip
        T.DESC -> flip
      g l = compare `on` l . unMeshRow

humRow :: (T.Humanize a) => a -> [Html m b]
humRow = pure . H.text . T.humanize
