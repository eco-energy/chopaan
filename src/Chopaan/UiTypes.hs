{-# LANGUAGE KindSignatures, TypeOperators, DataKinds, FlexibleContexts, TypeFamilies, FlexibleInstances, LambdaCase, TypeApplications, ScopedTypeVariables, MultiParamTypeClasses, UndecidableInstances, InstanceSigs, RecordWildCards #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving
, DerivingStrategies, DeriveAnyClass, StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Chopaan.UiTypes where

import GHC.Generics

import Control.Lens (makePrisms)
import Control.DeepSeq (NFData)

import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON)

import Data.Proxy (Proxy (Proxy))


import Database.Beam (Beamable, Columnar
                     , Database, DatabaseSettings
                     , Nullable, Table (..), TableEntity)


import Servant.API (Capture, Delete
                   , FromHttpApiData, Get, JSON
                   , Post, Put, QueryParam, Raw
                   , ReqBody, ToHttpApiData
                   , (:<|>) (..), (:>))

import Shpadoinkle (Html, MonadJSM)
import qualified Shpadoinkle.Html as H
import Shpadoinkle.Router (HasRouter ((:>>))
                          , Redirect (Redirect)
                          , Routed (..), View, navigate)
import Shpadoinkle.Widgets.Types (Field, Humanize (..)
                                 , Hygiene (Clean)
                                 , Input (Input, _value)
                                 , Pick (AtleastOne, One)
                                 , Present (present)
                                 , Search (Search)
                                 , Status (Edit, Errors, Valid)
                                 , Validate (rules)
                                 , fullOptions, fullOptionsMin)

import Shpadoinkle.Widgets.Table (SortCol(..))

import Chopaan.Node.NodeId
import Chopaan.Node.NodeT
import Chopaan.Kibbutz.KbtzId
import Chopaan.Kibbutz.KbtzimT ()
import Chopaan.CRUD



type API = "api" :> "kibbutzim" :> Get '[JSON] KbtzList
      :<|> "api" :> "kibbutz" :> Capture "id" KbtzName :> Get '[JSON] NodeList


data Frontend = MEcho (Maybe Text)
              | MKibbutzim (RosterKbtzim)
              | MKibbutz (RosterNodezim)
              | MAddNode (KbtzName) (Maybe NodeMAC) (NodeUpdate 'Edit)
              deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)

type SPA m = "app" :> "echo" :> QueryParam "echo" Text :> View m Text
        :<|> "app" :> "kibbutzim" :> View m Frontend
        :<|> "app" :> "kibbutz" :> Capture "id" KbtzName :> View m Frontend
        :<|> "app" :> "kibbutz" :> Capture "id" KbtzName :> "addNode" :> View m Frontend
        -- :<|> "app" :> QueryParam "search" Search :> View m Frontend
        :<|> Raw


data Route
  = REcho (Maybe Text)
  | RKibbutzim
  | RKibbutz (KbtzName)
  | RAddNode (KbtzName)
  -- | RSearch (Input Search)
  deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)

routes :: SPA m :>> Route
routes =
  REcho
  :<|> RKibbutzim
  :<|> RKibbutz
  :<|> RAddNode
  -- :<|> RSearch . Input Clean . fromMaybe ""
  :<|> RKibbutzim

instance Routed (SPA m) Route where
  redirect = \case
    REcho t -> Redirect (Proxy @("app" :> "echo" :> QueryParam "echo" Text :> View m Text)) ($ t)
    RKibbutzim -> Redirect (Proxy @("app" :> "kibbutzim" :> View m Frontend)) id
    RKibbutz k -> Redirect (Proxy @("app" :> "kibbutz" :> Capture "id" KbtzName :> View m Frontend)) ($ k)
    RAddNode k -> Redirect (Proxy @("app" :> "kibbutz" :> Capture "id" KbtzName :> "addNode" :> View m Frontend)) ($ k)
    --RSearch s -> Redirect (Proxy @("app" :> QueryParam "search" Search :> View m Frontend)) ($ Just (_value s))





deriving newtype instance ToHttpApiData Search
deriving newtype instance FromHttpApiData Search




{------------------ TABALS -----------}

data RosterKbtzim = RosterKbtzim
  { _sortK :: SortCol KbtzList
  , _searchK :: Input Search
  , _tableK :: KbtzList
  }
deriving instance Eq (RosterKbtzim)
deriving instance Ord (RosterKbtzim)
deriving instance Show (RosterKbtzim)
deriving instance Generic (RosterKbtzim)

instance NFData (RosterKbtzim)
instance ToJSON (RosterKbtzim)
instance FromJSON (RosterKbtzim)

data RosterNodezim = RosterNodezim
  { _sortN :: SortCol NodeList
  , _searchN :: Input Search
  , _tableN :: NodeList
  }
deriving instance Eq (RosterNodezim)
deriving instance Ord (RosterNodezim)
deriving instance Show (RosterNodezim)
deriving instance Generic (RosterNodezim)

instance NFData (RosterNodezim)
instance ToJSON (RosterNodezim)
instance FromJSON (RosterNodezim)


{-------------- DB Stuff ----------------}
 


makePrisms ''Frontend
