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

import Servant.API.Modifiers
import Servant.API.QueryParam

import Servant.Streamly
import Streamly (IsStream, MonadAsync, AsyncT, adapt)
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

import Chopaan.Kibbutz (hydrateKbtz, KbtzC(..), Hydration)
import Chopaan.Kibbutz.KbtzimT
import Chopaan.Utils.Time (dayRange)
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
  createKbtz k ns t t' = S.concatM (createKbtzWithHydration k ns t t')

daytimeRange s e = zip r (tail r)
  where
    r = dayRange s e

getHistoryForGraph :: forall t. (IsStream t)
  => KbtzName
  -> GraphType
  -> UTCTime
  -> UTCTime
  -> GraphM (t GraphM (G.SG NodeMAC))
getHistoryForGraph kn g t0 t1 = do
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


createKbtzWithHydration :: forall t. (IsStream t)
  => KbtzName
  -> [NodeMAC]
  -> UTCTime
  -> UTCTime
  -> GraphM (t GraphM Hydration)
createKbtzWithHydration k ns t t' = do
  withKbtzPool (\c -> do
                     addKbtz c k
                     mapM_ (addNodeToKbtz c k) ns
                 )
  b <- s3Bucket <$> ask
  hydrateKbtz (KbtzC k ns Nothing (Just b)) (t, t')

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



