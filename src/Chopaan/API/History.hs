{-# LANGUAGE MultiParamTypeClasses, RankNTypes, QuantifiedConstraints, DataKinds, TypeOperators, TypeApplications, TypeSynonymInstances, FlexibleInstances, ConstraintKinds, ScopedTypeVariables, GADTs, FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DeriveAnyClass, StandaloneDeriving, DerivingStrategies, DerivingVia #-}

module Chopaan.API.History where

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

import Chopaan.Graph
import Chopaan.Graph.Spider (stakeConfig, meshConfig, statusConfig, getGridRoot)

import Servant (Server, Get, Capture, Proxy(..), (:>)
               , JSON, FromHttpApiData(..), ToHttpApiData(..), hoistServer, serve)


import Data.Aeson (ToJSON, FromJSON)
import Network.Wai (Application)



type HistoryAPI = "history"
  :> (Capture "kbtzId" KbtzName)
  :> (Capture "graphType" GraphType)
  :> (Capture "startTime" UTCTime)
  :> (Capture "endTime" UTCTime)
  :> Get '[JSON] (SG NodeMAC)


toUrlPieceViaEnum :: Enum a => a -> Text.Text
toUrlPieceViaEnum = Text.pack . show . fromEnum

parseUrlPieceViaEnum :: Enum a => Text.Text -> Either Text.Text a
parseUrlPieceViaEnum = Right . toEnum . read . Text.unpack

instance ToHttpApiData GraphType where
  toUrlPiece = toUrlPieceViaEnum

instance FromHttpApiData GraphType where
  parseUrlPiece = parseUrlPieceViaEnum


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


serveHistoryAPI :: Server (HistoryAPI)
serveHistoryAPI = hoistServer (Proxy @ HistoryAPI) liftIO getHistoryForGraph

historyApp :: Application
historyApp = serve (Proxy :: Proxy HistoryAPI) serveHistoryAPI


getHistoryForGraph :: forall m. (MonadIO m)
  => KbtzName
  -> GraphType
  -> UTCTime
  -> UTCTime
  -> m (SG NodeMAC)
getHistoryForGraph kn g t0 t1 = case g of
  Mesh -> MeshG <$> getHistory meshConfig kn t0 t1
  Plan -> StakeG <$> getHistory stakeConfig kn t0 t1
  Status -> StatusG <$> getHistory statusConfig kn t0 t1

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
