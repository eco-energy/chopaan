{-# LANGUAGE KindSignatures, TypeOperators, DataKinds, FlexibleContexts, TypeFamilies, FlexibleInstances, LambdaCase, TypeApplications, ScopedTypeVariables, MultiParamTypeClasses, UndecidableInstances, InstanceSigs, RecordWildCards, CPP #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving
, DerivingStrategies, DeriveAnyClass, StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses, TypeFamilies, FunctionalDependencies #-}

module Chopaan.UiTypes where

import GHC.Generics

import Control.DeepSeq (NFData)

import Data.Text ()
import Data.Aeson (ToJSON, FromJSON)

import Data.Proxy (Proxy (Proxy))

import Servant.API (Capture, Get, JSON
                   , Post, Raw
                   , (:<|>) (..), (:>))


import Shpadoinkle.Router (HasRouter ((:>>))
                          , Redirect (Redirect)
                          , Routed (..), View)
import Shpadoinkle.Widgets.Types (Input
                                 , Search
                                 , Status (Edit)
                                 )

import Shpadoinkle.Widgets.Table (SortCol(..))

import Chopaan.Node.NodeId
import Chopaan.Node.NodeT
import Chopaan.Kibbutz.KbtzId
import Chopaan.Kibbutz.KbtzimT ()
import Chopaan.CRUD
import Chopaan.Graph
import qualified Data.Time as Ti

type API = "api" :> "kibbutzim" :> Get '[JSON] KbtzList
      :<|> "api" :> "kibbutz" :> Capture "id" KbtzName :> Get '[JSON] NodeList

data GView = GView
  { _whichK :: KbtzName
  , _whichG :: GraphType
  , _startTime :: Ti.UTCTime
  , _endTime :: Ti.UTCTime
  , _availableHistory :: (Ti.UTCTime, Ti.UTCTime)
  , _currentG :: Maybe (SG NodeMAC)
  } deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)

data Frontend = MHomePage RosterKbtzim
              | MKibbutzim RosterKbtzim
              | MKibbutz RosterNodezim
              | MGraph GView
              | MAddNode (KbtzName) (Maybe NodeMAC) (NodeUpdate 'Edit)
              deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)


type SPA m = View m Frontend
        :<|> "app" :> "kibbutzim" :> View m Frontend
        :<|> "app" :> "kibbutz" :> Capture "id" KbtzName :> View m Frontend
        :<|> "app" :> "kibbutz" :> Capture "id" KbtzName :> "addNode" :> View m Frontend
        :<|> "app" :> "graph" :> Capture "id" KbtzName :> View m Frontend



data Route
  = RHomePage
  | RKibbutzim
  | RGraph KbtzName
  | RKibbutz KbtzName
  | RAddNode KbtzName
  deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)

routes :: SPA m :>> Route
routes =
  RHomePage
  :<|> RKibbutzim
  :<|> RKibbutz
  :<|> RAddNode
  :<|> RGraph

instance Routed (SPA m) Route where
  redirect = \case
    RHomePage -> Redirect (Proxy @(View m Frontend)) id
    RKibbutzim -> Redirect (Proxy @("app" :> "kibbutzim" :> View m Frontend)) id
    RKibbutz k -> Redirect (Proxy @("app" :> "kibbutz" :> Capture "id" KbtzName :> View m Frontend)) ($ k)
    RAddNode k -> Redirect (Proxy @("app" :> "kibbutz" :> Capture "id" KbtzName :> "addNode" :> View m Frontend)) ($ k)
    RGraph k -> Redirect (Proxy @("app" :> "graph" :> Capture "id" KbtzName :> View m Frontend)) ($ k)


{------------------ TABALS -----------}

data RosterKbtzim = RosterKbtzim
  { _sortK :: SortCol KbtzList
  , _searchK :: Input Search
  , _tableK :: KbtzList
  } deriving (Generic, Eq, Ord, Show, NFData, ToJSON, FromJSON)


data RosterNodezim = RosterNodezim
  { _sortN :: SortCol NodeList
  , _searchN :: Input Search
  , _tableN :: NodeList
  } deriving (Generic, Eq, Ord, Show, NFData, ToJSON, FromJSON)
