{-# LANGUAGE MultiParamTypeClasses, RankNTypes, QuantifiedConstraints, DataKinds, TypeOperators, TypeApplications, TypeSynonymInstances, FlexibleInstances, ConstraintKinds, ScopedTypeVariables, GADTs, FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DeriveAnyClass, StandaloneDeriving, DerivingStrategies, DerivingVia #-}
module Chopaan.API.History where

import GHC.Generics

import Control.DeepSeq (NFData)
import Control.Monad.IO.Class

import Data.Greskell (FromGraphSON)

import NetSpider.Spider (getSnapshot, withSpider)
import NetSpider.Spider.Config (Config(..))
import NetSpider.Graph (LinkAttributes(..), NodeAttributes(..))
import NetSpider.Timestamp (fromUTCTime)
import NetSpider.Query
import NetSpider.Snapshot


import qualified Data.Text as Text
import Data.Time (UTCTime)

import Chopaan.Comm.Address
import Chopaan.Kibbutz.KbtzId
import Chopaan.Node.NodeId
import Chopaan.Kibbutz.Mesh
import Chopaan.Node.Folds (SensorS)
import Chopaan.Kibbutz.Transactor (Stake, TransactionStatus)
import Chopaan.Kibbutz (stakeConfig, meshConfig, statusConfig, getGridRoot)


import Servant (Server, Get, Capture, Proxy(..), (:>)
               , JSON, FromHttpApiData(..), ToHttpApiData(..), hoistServer)


import Data.Aeson (ToJSON, FromJSON)



type IsoGConn n a e = (Address n
                      , LinkAttributes e
                      , NodeAttributes a
                      , Eq n
                      , FromGraphSON n
                      )


type HistoryConn n a e =
  ( ToHttpApiData n, FromHttpApiData n, ToJSON n, FromJSON n
  , ToJSON e, FromJSON e
  , ToJSON a, FromJSON a
  , FromGraphSON n, IsoGConn n a e)

type HistoryAPI = "history"
  :> (Capture "graphType" GraphType)
  :> (Capture "graphId" KbtzName)
  :> (Capture "startTime" UTCTime)
  :> (Capture "endTime" UTCTime)
  :> Get '[JSON] (SG)


data GraphType = Mesh | Plan | Status
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Generic, ToJSON, FromJSON, NFData)

genericToUrlPieceViaShow :: Show a =>  a -> Text.Text
genericToUrlPieceViaShow = Text.pack . show

instance ToHttpApiData GraphType where
  toUrlPiece = genericToUrlPieceViaShow

instance FromHttpApiData GraphType where
  parseUrlPiece = read . Text.unpack


serveHistoryApi :: Server (HistoryAPI)
serveHistoryApi = hoistServer (Proxy @ HistoryAPI) liftIO getHistoryForGraph

  
data SG where
  MeshSnapshot :: SnapshotGraph NodeMAC MeshNode RxSignal -> SG
  StakeSnapshot :: SnapshotGraph NodeMAC SensorS Stake -> SG
  StatusSnapshot :: SnapshotGraph NodeMAC SensorS TransactionStatus -> SG
  deriving (Generic, ToJSON, FromJSON)

getHistoryForGraph :: forall m. (MonadIO m)
  => GraphType
  -> KbtzName
  -> UTCTime
  -> UTCTime
  -> m (SG)
getHistoryForGraph g kn t0 t1 = case g of
  Mesh -> MeshSnapshot <$> getHistory meshConfig kn t0 t1
  Plan -> StakeSnapshot <$> getHistory stakeConfig kn t0 t1
  Status -> StatusSnapshot <$> getHistory statusConfig kn t0 t1

getHistory :: forall m a e. (MonadIO m, IsoGConn NodeMAC a e)
  => Config NodeMAC a e
  -> KbtzName
  -> UTCTime
  -> UTCTime
  -> m (SnapshotGraph NodeMAC a e)
getHistory c k t t' = query c q
  where
    q :: Query NodeMAC a e e
    q = (defQuery [getGridRoot k])
      { timeInterval =
        Finite (fromUTCTime t)
        <=..<=
        Finite (fromUTCTime t')
      }

query :: (MonadIO m, IsoGConn n a e)
  => Config n a e
  -> Query n a e e
  -> m (SnapshotGraph n a e)
query c q = liftIO $ withSpider c (\sp -> liftIO $ getSnapshot sp q)
