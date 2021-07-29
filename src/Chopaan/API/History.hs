{-# LANGUAGE MultiParamTypeClasses, RankNTypes, QuantifiedConstraints, DataKinds, TypeOperators, TypeApplications, TypeSynonymInstances, FlexibleInstances, ConstraintKinds, ScopedTypeVariables, GADTs, FlexibleContexts, NamedFieldPuns, KindSignatures, PolyKinds #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DeriveAnyClass, StandaloneDeriving, DerivingStrategies, DerivingVia, UndecidableInstances, OverloadedStrings, CPP, InstanceSigs #-}

module Chopaan.API.History where

import GHC.Generics

import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Aeson (ToJSON, FromJSON)
import Data.Greskell (FromGraphSON)


import qualified Data.Text as Text
import Data.Time (UTCTime, diffUTCTime, addUTCTime
                 , nominalDay)


import Chopaan.Kibbutz.KbtzId
import Chopaan.Node.NodeId

import qualified Chopaan.Graph.G as G


import Servant.API.Modifiers
import Servant.API.QueryParam
import Chopaan.CRUD
import Chopaan.Graph
import Servant.Streamly
import Streamly (IsStream, MonadAsync, AsyncT, adapt)
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Prelude as S
import Servant.API.Stream

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


type HistoryAPI t = "history"
  :> (QueryParamR "kbtzId" KbtzName)
  :> (QueryParamR "graphType" GraphType)
  :> (QueryParamR "startTime" UTCTime)
  :> (QueryParamR "endTime" UTCTime)
  :> StreamGet NewlineFraming JSON (t IO (G.SG NodeMAC))

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


instance CRUDChopaan (GraphM) where
  listKibbutzim = do
    ks <- withKbtzPool getKbtzim
    return $ KbtzList ks  
  listNodezim k = do
    ns <- withKbtzPool ((flip getKbtzNodes) k)
    return . NodeList $ ns
  getGraph :: (IsStream t) => KbtzName -> GraphType -> UTCTime -> UTCTime -> t GraphM (SG NodeMAC)
  getGraph k g t t' = S.concatM (getHistoryForGraph k g t t')


getHistoryForGraph :: forall t. (IsStream t)
  => KbtzName
  -> GraphType
  -> UTCTime
  -> UTCTime
  -> GraphM (t GraphM (G.SG NodeMAC))
getHistoryForGraph kn g t0 t1 = do
  ns <- (\p -> withResource (gremlinPool p) ((flip getKbtzNodes) kn)) =<< ask
  let
    ts = S.fromList $ dayRange t0 t1
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

dayRange :: UTCTime -> UTCTime -> [(UTCTime, UTCTime)]
dayRange start end = case (diff < nominalDay) of
  True -> [(start, end)]
  False ->
    scanl (\(_, e) s ->
             (e, addUTCTime s e))
                      (start, addUTCTime nominalDay start) (take days $ repeat nominalDay)
    where
      days = ceiling (diff / nominalDay)
  where
    diff = (diffUTCTime end start)

hoistS :: forall t m. (IsStream t, MonadAsync m) => String -> Int -> (t GraphM) ~> (t m) 
hoistS h p = adapt . S.hoist (runGraphM h p) . adapt
 

serveHistoryAPI :: forall t. (IsStream t)
  => String
  -> Int
  -> Server (HistoryAPI t)
serveHistoryAPI h p = history
  where
    history k g t t' = (runGraphM h p) $ do
      (hoistS @t @IO h p) <$> getHistoryForGraph @t k g t t' 

historyApp :: String
           -> Int
           -> Application
historyApp h p = serve (Proxy :: Proxy (HistoryAPI AsyncT)) $ serveHistoryAPI h p
#endif



