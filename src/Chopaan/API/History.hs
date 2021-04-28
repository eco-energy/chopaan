{-# LANGUAGE MultiParamTypeClasses, RankNTypes, QuantifiedConstraints, DataKinds, TypeOperators, TypeApplications, TypeSynonymInstances, FlexibleInstances, ConstraintKinds, ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DeriveAnyClass, StandaloneDeriving, DerivingStrategies, DerivingVia #-}
module Chopaan.API.History where

import GHC.Generics

import Control.Monad.Trans.Reader

import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.IO.Class

import Data.Greskell (FromGraphSON)

import NetSpider.Spider (Spider, getSnapshot)
import NetSpider.Spider.Config (Config(..))
import NetSpider.Graph (LinkAttributes(..), NodeAttributes(..))
import NetSpider.Timestamp (fromUTCTime)
import NetSpider.Query
import NetSpider.Snapshot
--import qualified NetSpider.Snapshot as Sn


import qualified Data.Text as Text
import Data.Time (UTCTime)

import Chopaan.Comm.Address
import Streamly()


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


type HistoryAPI n e a = "history"
  :> (Capture "graphType" GraphType)
  :> (Capture "graphId" n)
  :> (Capture "startTime" UTCTime)
  :> (Capture "endTime" UTCTime)
  :> Get '[JSON] (SnapshotGraph n e a)


data GraphType = Mesh | Plan | BilledReality | HWConfig
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, Bounded, Enum)

genericToUrlPieceViaShow :: Show a =>  a -> Text.Text
genericToUrlPieceViaShow = Text.pack . show

instance ToHttpApiData GraphType where
  toUrlPiece = genericToUrlPieceViaShow

instance FromHttpApiData GraphType where
  parseUrlPiece = read . Text.unpack

newtype GraphApp r a = GraphApp { runGraphApp :: ReaderT r IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader r)

toHandler :: MonadIO m => r -> GraphApp r a -> m a
toHandler r a = liftIO $ runReaderT (runGraphApp a) r


serveApi :: forall n a e. (HistoryConn n a e) => Server (HistoryAPI n a e)
serveApi = hoistServer (Proxy @ (HistoryAPI n a e)) (toHandler s) getHistory
  where
    s :: Spider n a e
    s = undefined

getHistory :: forall n a e. (IsoGConn n a e)
  => GraphType
  -> n
  -> UTCTime
  -> UTCTime
  -> GraphApp (Spider n a e) (SnapshotGraph n a e)
getHistory g n t t' =
  GraphApp $ (\d -> query d q)
  =<< ask
  where
    q :: Query n a e e
    q = (defQuery [n])
      { timeInterval =
        Finite (fromUTCTime t)
        <=..<=
        Finite (fromUTCTime t')
      }

query :: (MonadIO m, IsoGConn n a e)
  => Spider n a e
  -> Query n a e e
  -> m (SnapshotGraph n a e)
query s q = liftIO $ getSnapshot s q
