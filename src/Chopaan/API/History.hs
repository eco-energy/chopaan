{-# LANGUAGE MultiParamTypeClasses, RankNTypes, QuantifiedConstraints, DataKinds, TypeOperators, TypeApplications, TypeSynonymInstances, FlexibleInstances, ConstraintKinds, ScopedTypeVariables, GADTs, FlexibleContexts, NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DeriveAnyClass, StandaloneDeriving, DerivingStrategies, DerivingVia, UndecidableInstances, OverloadedStrings #-}

module Chopaan.API.History where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader hiding (ask)
import Control.Monad.Reader.Class
import Control.Monad.Catch

import Data.Greskell (FromGraphSON, ToGreskell(..))

import NetSpider.Spider (Spider)
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


import Network.Greskell.WebSocket (Client)
import Chopaan.Graph.Kbtz (getKbtzim, addHHToKbtz, getKbtzNodes, kbtzPool, KbtzPool)
import Chopaan.Graph hiding (toLink)
import qualified Chopaan.Graph.G as G
import Chopaan.Graph.Spider ( Spools
                            , SpiderM
                            , mkSpool
                            , runSpider
                            , mkConfG
                            , meshNodesSnapshot
                            , txNodesSnapshot
                            , statusNodesSnapshot
                            , flowNodesSnapshot
                            )

import Servant (Server, Get, Capture, Proxy(..), (:>)
               , JSON, FromHttpApiData(..), ToHttpApiData(..), hoistServer, serve)

import Data.Pool
import Data.Aeson (ToJSON, FromJSON)
import Network.Wai (Application)
import Control.Monad.Base
import Control.Monad.Trans.Control
import Chopaan.CRUD
import Chopaan.Kibbutz.KbtzimT
import Chopaan.Node.NodeT
import Servant.Links


data SpiderOpts = SpiderOpts String Int

newtype HistoryApp a = HistoryApp { runHistoryApp :: ReaderT (DBPools) IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (DBPools),
                    MonadBase IO, MonadBaseControl IO, MonadThrow, MonadCatch)


type HistoryAPI = "history"
  :> (Capture "kbtzId" KbtzName)
  :> (Capture "graphType" GraphType)
  :> (Capture "startTime" UTCTime)
  :> (Capture "endTime" UTCTime)
  :> Get '[JSON] (G.SG NodeMAC)


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

toHandlerH :: MonadIO m => String -> Int -> HistoryApp ~> m
toHandlerH h p a = liftIO $ runReaderT (runHistoryApp a) =<< (mkDBPools h p)

mkDBPools :: MonadIO m => String -> Int -> m (DBPools)
mkDBPools h p = do
  kp <- liftIO $ kbtzPool h p
  spools <- liftIO $ mkSpool $ mkConfG (h, p)
  return $ DBPools spools kp

serveHistoryAPI :: String -> Int -> Server (HistoryAPI)
serveHistoryAPI h p = hoistServer (Proxy @ HistoryAPI) (toHandlerH h p) getHistoryForGraph 

historyApp :: String -> Int -> Application
historyApp h p = serve (Proxy :: Proxy HistoryAPI) $ serveHistoryAPI h p


data DBPools = DBPools
  { spools :: Spools
  , gremlinPool :: KbtzPool
  }


withKbtzPool :: (Client -> HistoryApp a) -> HistoryApp a
withKbtzPool f = do
    (DBPools _ kp) <- ask
    withResource kp f
    
defKbtz :: Int -> KbtzName -> Kbtzim
defKbtz i k = Kbtzim (KbtzId i) k Nothing

instance CRUDChopaan (HistoryApp) where
  listKibbutzim = do
    ks <- withKbtzPool getKbtzim
    return . KbtzList $ (uncurry defKbtz) <$> (zip [1..] ks)  
  listNodezim k = do
    ns <- withKbtzPool ((flip getKbtzNodes) k)
    return . NodeList $ (undefined) <$> (zip [1..] ns)
  getGraph = getHistoryForGraph

getHistoryForGraph :: KbtzName
  -> GraphType
  -> UTCTime
  -> UTCTime
  -> HistoryApp (G.SG NodeMAC)
getHistoryForGraph kn g t0 t1 = do
  DBPools{gremlinPool, spools} <- ask  
  ns <- withResource gremlinPool ((flip getKbtzNodes) kn)
  let
    spoolSnap :: SpiderM ~> HistoryApp
    spoolSnap = liftIO . (runSpider spools)
  case g of
    MeshG -> (G.Mesh . G.SG) <$> (spoolSnap $ meshNodesSnapshot ns t0 t1)
    PlanG -> (G.Transactor . G.SG) <$> (spoolSnap $ txNodesSnapshot ns t0 t1)
    StatusG -> (G.Status . G.SG)  <$> (spoolSnap $ statusNodesSnapshot ns t0 t1)
    FlowG -> (G.Flow . G.SG) <$> (spoolSnap $ flowNodesSnapshot ns t0 t1)
