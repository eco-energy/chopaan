{-# LANGUAGE MultiParamTypeClasses, RankNTypes, QuantifiedConstraints, DataKinds, TypeOperators, TypeApplications, TypeSynonymInstances, FlexibleInstances, ConstraintKinds, ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DeriveAnyClass, StandaloneDeriving, DerivingStrategies, DerivingVia #-}
module Chopaan.Graph (Spider, SnapshotGraph, connectWS, close, addFoundNode, getSnapshot
                     , module Chopaan.Graph
                     , module Algebra.Graph.Labelled
                     )
where
import GHC.Generics

import Control.Applicative
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.IO.Class

import Data.Greskell (newBind, gProperty, lookupAs, Key, pMapToFail, FromGraphSON)
import Data.Greskell.Extra (writeKeyValues, (<=:>))
import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(..))
import NetSpider.Spider
  (Spider, connectWS, close, addFoundNode, clearAll, getSnapshot)
import NetSpider.Graph (LinkAttributes(..), EFinds, NodeAttributes(..), VFoundNode)
import NetSpider.Timestamp (Timestamp, fromUTCTime)
import NetSpider.Query
import NetSpider.Snapshot
--import qualified NetSpider.Snapshot as Sn


import Algebra.Graph.Labelled
import qualified Data.Text as Text
import Data.Time (UTCTime)

import Chopaan.Comm.Address
import Streamly


import           Servant (Server, Get, Handler, Capture, Proxy(..), (:<|>)(..), (:>)
                         , serve, JSON, FromHttpApiData(..), ToHttpApiData(..), hoistServer)


import Data.Aeson (ToJSON, FromJSON)


-- $ This has two obvious instances.
-- On the frontend, a servant api call
-- On the backend, a greskell query

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

newtype GraphApp r a = GraphApp { runApp :: ReaderT r IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader r)

toHandler :: MonadIO m => r -> GraphApp r a -> m a
toHandler r a = liftIO $ runReaderT (runApp a) r

type HistoryConn n a e =
  ( ToHttpApiData n, FromHttpApiData n, ToJSON n, FromJSON n
  , ToJSON e, FromJSON e
  , ToJSON a, FromJSON a
  , FromGraphSON n, IsoGConn n a e)

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




type IsoGConn n a e = (Address n
                      , LinkAttributes e
                      , Monoid e
                      , NodeAttributes a
                      , Monoid a
                      , Eq n
                      , FromGraphSON n
                      )
