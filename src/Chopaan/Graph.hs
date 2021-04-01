{-# LANGUAGE MultiParamTypeClasses, RankNTypes, QuantifiedConstraints, DataKinds, TypeOperators, TypeApplications, TypeSynonymInstances, FlexibleInstances, ConstraintKinds, ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DeriveAnyClass, StandaloneDeriving, DerivingStrategies #-}
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

import Data.Time (UTCTime)

import Chopaan.Comm.Address
import Streamly


import           Servant (Server, Get, Handler, Capture, Proxy(..), (:<|>)(..), (:>)
                         , serve, JSON, FromHttpApiData(..), ToHttpApiData(..), hoistServer)
import           Servant.API.WebSocket (WebSocket (..))

import Data.Aeson (ToJSON, FromJSON)
data Backend


-- $ This has two obvious instances.
-- On the frontend, a servant api call
-- On the backend, a greskell query

type HistoryAPI n e a = "history"
  :> (Capture "graphType" GraphType)
  :> (Capture "graphId" n)
  :> (Capture "startTime" UTCTime)
  :> (Capture "endTime" UTCTime)
  :> Get '[JSON] (SnapshotGraph n e a)


class (Monad m, Address n, LinkAttributes e, NodeAttributes a) => PersistedGraph m n e a where
  fetch :: (IsStream t) => Backend -> UTCTime -> UTCTime -> t m (n, UTCTime, Graph e a)
  save :: Backend -> Query n a e e -> n -> UTCTime -> Graph e a -> m ()

data GraphType = Mesh | Plan | BilledReality | HWConfig
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

instance FromHttpApiData GraphType
instance ToHttpApiData GraphType

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

--toGS :: SnapshotGraph n e a -> GraphS n e a
--toGS (es, ns) = GraphS $ foldr overlay mempty (edge <$> es) --, isoA <$> ns)
--  where
--    isoE = undefined
--    isoA = undefined

class Iso a b where
  fwd :: a -> b
  rev :: b -> a


newtype GraphS n a e = GraphS (n, Graph e a)


type IsoGConn n a e = (Address n
                      , LinkAttributes e
                      , Monoid e
                      , NodeAttributes a
                      , Monoid a
                      , Eq n
                      , FromGraphSON n
                      )
