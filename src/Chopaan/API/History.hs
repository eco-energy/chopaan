{-# LANGUAGE MultiParamTypeClasses, RankNTypes, QuantifiedConstraints, DataKinds, TypeOperators, TypeApplications, TypeSynonymInstances, FlexibleInstances, ConstraintKinds, ScopedTypeVariables, GADTs, FlexibleContexts, NamedFieldPuns, KindSignatures, PolyKinds #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DeriveAnyClass, StandaloneDeriving, DerivingStrategies, DerivingVia, UndecidableInstances, OverloadedStrings, CPP, InstanceSigs #-}

module Chopaan.API.History where

import GHC.Generics

import Control.Arrow
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Aeson (ToJSON, FromJSON)
import Data.Greskell (FromGraphSON)


import qualified Data.Text as Text
import Data.Time (UTCTime(..), diffUTCTime, addUTCTime)


import Chopaan.Kibbutz.KbtzId
import Chopaan.Node.NodeId
import qualified Chopaan.Graph.G as G
import Chopaan.CRUD
import Chopaan.Graph
import Chopaan.Types
import Servant.API.Modifiers
import Servant.API.QueryParam

import Servant.Streamly
import Streamly.Prelude (IsStream, MonadAsync, AsyncT, adapt)
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.IsStream as S
import Servant.API.Stream


#ifndef ghcjs_HOST_OS
import Servant (Server, Get, Capture, QueryParam, Proxy(..), (:>)
               , JSON, FromHttpApiData(..), ToHttpApiData(..), hoistServer, serve)
import NetSpider.Graph (LinkAttributes(..), NodeAttributes(..))
import Chopaan.Graph.Kbtz (getKbtzim
                          , getKbtzNodes
                          , kbtzPool
                          , addKbtz
                          , addNodeToKbtz
                          , KbtzPool)
import Chopaan.Graph.Spider ( meshNodesSnapshot
                            , txNodesSnapshot
                            , statusNodesSnapshot
                            , flowNodesSnapshot
                            )
import Chopaan.Comm.Comm
import Chopaan.Comm.Address
import Data.Pool

import Network.Wai (Application)

import Chopaan.Kibbutz (KbtzC(..))
--import Chopaan.Hydrate (hydrateKbtz, Hydration)
import Chopaan.Utils.Time (dayRange)

#else
import Servant.API
#endif


type HistoryAPI t = "history"
  :> (QueryParamR "kbtzId" KbtzName)
  :> (QueryParamR "graphType" GraphType)
  :> (QueryParamR "resolution" Resolution)
  :> (QueryParamR "startTime" UTCTime)
  :> (QueryParamR "endTime" UTCTime)
  :> StreamGet NewlineFraming JSON (t IO (G.SG NodeMAC))

type QueryParamR = QueryParam' '[Required, Strict]



instance ToHttpApiData GraphType where
  toUrlPiece = toUrlPieceViaEnum

instance FromHttpApiData GraphType where
  parseUrlPiece = parseUrlPieceViaEnum


#ifndef ghcjs_HOST_OS
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


instance CRUDChopaan (GraphM) where
  listKibbutzim = do
    ks <- withKbtzPool getKbtzim
    return $ KbtzList ks  
  listNodezim k = do
    ns <- withKbtzPool ((flip getKbtzNodes) k)
    return . NodeList $ ns
  getGraph :: (IsStream t) => KbtzName -> GraphType -> Resolution -> UTCTime -> UTCTime -> t GraphM (SG NodeMAC)
  getGraph k g r t t' = S.concatM (getHistoryForGraph k g r t t')
  --createKbtz k ns t t' = S.concatM (createKbtzWithHydration k ns t t')

daytimeRange s e = zip r (tail r)
  where
    r = dayRange s e

getHistoryForGraph :: forall t. (IsStream t)
  => KbtzName
  -> GraphType
  -> Resolution
  -> UTCTime
  -> UTCTime
  -> GraphM (t GraphM (G.SG NodeMAC))
getHistoryForGraph kn g r t0 t1 = do
  ns <- withKbtzPool ((flip getKbtzNodes) kn)
  let
    ts = S.fromList $ daytimeRange t0 t1
  case g of
    MeshG ->
      return $ streamQ G.Mesh meshNodesSnapshot ns ts
    PlanG ->
      return $ streamQ G.Transactor txNodesSnapshot ns ts
    StatusG ->
      return $ streamQ G.Status statusNodesSnapshot ns ts
    FlowG ->
      return $ streamQ G.Flow flowNodesSnapshot ns ts
  where
    streamQ wr q ns ts = S.mapM (\(t, t') -> (wr . G.SG) <$> (withSpider $ q ns t t')) $ ts


-- createKbtzWithHydration :: forall t. (IsStream t)
--   => KbtzName
--   -> [NodeMAC]
--   -> UTCTime
--   -> UTCTime
--   -> GraphM (t GraphM Hydration)
-- createKbtzWithHydration k ns t t' = do
--   withKbtzPool (\c -> do
--                      addKbtz c k
--                      mapM_ (addNodeToKbtz c k) ns
--                  )
--   b <- s3Bucket <$> ask
--   hydrateKbtz undefined (KbtzC k ns undefined (Just b))

serveHistoryAPI :: forall t. (IsStream t)
  => DBPools
  -> Server (HistoryAPI t)
serveHistoryAPI poo = history
  where
    history k g r t t' = (runGraphWithDB poo) $ do
      (hoistG poo) <$> getHistoryForGraph @t k g r t t' 

historyApp :: DBPools
           -> Application
historyApp = serve (Proxy :: Proxy (HistoryAPI S.AheadT)) . serveHistoryAPI
#endif



