{-# LANGUAGE MultiParamTypeClasses, RankNTypes, QuantifiedConstraints, DataKinds, TypeOperators, TypeApplications, TypeSynonymInstances, FlexibleInstances, ConstraintKinds, ScopedTypeVariables, GADTs, FlexibleContexts, NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DeriveAnyClass, StandaloneDeriving, DerivingStrategies, DerivingVia, UndecidableInstances, OverloadedStrings, CPP #-}

module Chopaan.API.History where

import GHC.Generics

import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Aeson (ToJSON, FromJSON)
import Data.Greskell (FromGraphSON)


import qualified Data.Text as Text
import Data.Time (UTCTime)


import Chopaan.Kibbutz.KbtzId
import Chopaan.Node.NodeId

import qualified Chopaan.Graph.G as G


import Servant.API.Modifiers
import Servant.API.QueryParam
import Chopaan.CRUD
import Chopaan.Graph

#ifndef ghcjs_HOST_OS
import Servant (Server, Get, Capture, QueryParam, Proxy(..), (:>)
               , JSON, FromHttpApiData(..), ToHttpApiData(..), hoistServer, serve)
import NetSpider.Graph (LinkAttributes(..), NodeAttributes(..))
import Chopaan.Graph.Kbtz (getKbtzim, addHHToKbtz, getKbtzNodes, kbtzPool, KbtzPool)
import Chopaan.Graph.Spider ( meshNodesSnapshot
                            , txNodesSnapshot
                            , statusNodesSnapshot
                            , flowNodesSnapshot
                            )
import Chopaan.Comm.Address
import Data.Pool

import Network.Wai (Application)

import Chopaan.Kibbutz.KbtzimT
import qualified System.Envy as E
import Options.Applicative
#else
import Servant.API
#endif


type HistoryAPI = "history"
  :> (QueryParamR "kbtzId" KbtzName)
  :> (QueryParamR "graphType" GraphType)
  :> (QueryParamR "startTime" UTCTime)
  :> (QueryParamR "endTime" UTCTime)
  :> Get '[JSON] (G.SG NodeMAC)

type QueryParamR = QueryParam' '[Required, Strict]


toUrlPieceViaEnum :: Enum a => a -> Text.Text
toUrlPieceViaEnum = Text.pack . show . fromEnum

parseUrlPieceViaEnum :: Enum a => Text.Text -> Either Text.Text a
parseUrlPieceViaEnum = Right . toEnum . read . Text.unpack

instance ToHttpApiData GraphType where
  toUrlPiece = toUrlPieceViaEnum

instance FromHttpApiData GraphType where
  parseUrlPiece = parseUrlPieceViaEnum


#ifndef ghcjs_HOST_OS
data TinkerConf = TinkerConf
  { janusHost :: String
  , janusPort :: Int
  } deriving (Generic, E.FromEnv)


tkParser :: Parser TinkerConf
tkParser = TinkerConf
  <$> strOption   (long "tinkerHost" <> metavar "TINKERHOST")
  <*> option auto (long "tinkerPort" <> metavar "TINKERPORT" <> showDefault <> value 8182)

tkOptions :: ParserInfo TinkerConf
tkOptions = info (tkParser <**> helper) $
    fullDesc <> progDesc "Chopaan"
             <> header "Control and Monitor Kbtzim"



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

    
defKbtz :: Int -> KbtzName -> Kbtzim
defKbtz i k = Kbtzim (KbtzId i) k Nothing

instance CRUDChopaan (GraphM) where
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
  -> GraphM (G.SG NodeMAC)
getHistoryForGraph kn g t0 t1 = do
  DBPools{gremlinPool, spools} <- ask  
  ns <- withResource gremlinPool ((flip getKbtzNodes) kn)
  case g of
    MeshG -> (G.Mesh . G.SG) <$> (withSpider $ meshNodesSnapshot ns t0 t1)
    PlanG -> (G.Transactor . G.SG) <$> (withSpider $ txNodesSnapshot ns t0 t1)
    StatusG -> (G.Status . G.SG)  <$> (withSpider $ statusNodesSnapshot ns t0 t1)
    FlowG -> (G.Flow . G.SG) <$> (withSpider $ flowNodesSnapshot ns t0 t1)

serveHistoryAPI :: String -> Int -> Server (HistoryAPI)
serveHistoryAPI h p = hoistServer (Proxy @ HistoryAPI) (toHandlerH h p) getHistoryForGraph 

historyApp :: String -> Int -> Application
historyApp h p = serve (Proxy :: Proxy HistoryAPI) $ serveHistoryAPI h p
#endif



