{-# LANGUAGE MultiParamTypeClasses, RankNTypes, QuantifiedConstraints, DataKinds, TypeOperators, TypeApplications, TypeSynonymInstances, FlexibleInstances, ConstraintKinds, ScopedTypeVariables, GADTs, FlexibleContexts, NamedFieldPuns, KindSignatures, PolyKinds #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DeriveAnyClass, StandaloneDeriving, DerivingStrategies, DerivingVia, UndecidableInstances, OverloadedStrings, CPP, InstanceSigs #-}

module Chopaan.API.Kbtz where

import Chopaan.Graph.Kbtz

import Control.DeepSeq
import Control.Monad.Reader.Class
import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Aeson (ToJSON, FromJSON)


import qualified Data.Text as T
import Data.Time (UTCTime(..), diffUTCTime, addUTCTime)


import Chopaan.Kibbutz.KbtzId
import Chopaan.Node.NodeId
import Chopaan.Node.HW

import Servant.API.Modifiers
import Servant.API.QueryParam

import Servant.Streamly
import Streamly.Prelude (IsStream, MonadAsync, AsyncT, adapt)
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.IsStream as S
import Servant.API.Stream


import Servant.API.Generic
import Servant.Server.Generic
import Servant.Client.Generic

import Servant.Server
import Servant (Server, Get, Put, Post, Capture, QueryParam, Proxy(..), (:>)
               , JSON, FromHttpApiData(..), ToHttpApiData(..), hoistServer, serve
               , AsLink, Link, allFieldLinks, ReqBody, throwError, err404)
import Servant.Client

import qualified Algebra.Graph.Labelled as G
import qualified Chopaan.Graph.Kbtz as K

import Chopaan.Graph (GraphM, withKbtzPool, runGraphWithDB, DBPools)

import Data.Pool

import Network.Wai (Application)

type QPR = QueryParam' '[Required, Strict]

newtype R3 = R3 (Double, Double, Double)
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)


data APINode = APINode { nodeId :: NodeMAC
                       , nodeLocation :: R3
                       , nodeConfig :: HW Double
                       } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

deriving instance (ToJSON a, ToJSON b) => ToJSON (G.Graph a b)
deriving instance (FromJSON a, FromJSON b) => FromJSON (G.Graph a b)

newtype APIKbtz = APIKbtz (G.Graph Double APINode)
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data KbtzAPI route = KbtzAPI
  { _addKbtz :: route
                :- QPR "name" KbtzName
                :> Put '[JSON] ()
  , _addNode :: route
                :- QPR "kbtzId" KbtzName
                :> ReqBody '[JSON] (APINode)
                :> Post '[JSON] ()
  , _reassociateNode :: route
                       :- QPR "originKbtz" KbtzName
                       :> QPR "targetKbtz" KbtzName
                       :> QPR "nodeId" NodeMAC
                       :> Post '[JSON] Bool
  , _reassociateNodeMAC :: route
                          :- QPR "originNodeId" NodeMAC
                          :> QPR "newNodeId" NodeMAC
                          :> Post '[JSON] ()
  , _removeNodeFromKbtz :: route
                          :- QPR "originKbtz" KbtzName
                          :> QPR "nodeId" NodeMAC
                          :> Post '[JSON] ()
  , _getKbtzim :: route :- Get '[JSON] [KbtzName]
  , _getKbtz :: route
               :- QPR "kbtzId" KbtzName
               :> Get '[JSON] (APIKbtz)
  , _getNode :: route
               :- QPR "kbtzId" KbtzName
               :> QPR "nodeId" NodeMAC
               :> Get '[JSON] (APINode) 
  } deriving (Generic)


api :: Proxy (ToServantApi KbtzAPI)
api = genericApi (Proxy :: Proxy KbtzAPI)

links :: KbtzAPI (AsLink Link)
links = allFieldLinks

client :: ClientEnv -> KbtzAPI (AsClientT IO)
client env = genericClientHoist (\x -> runClientM x env >>= either throwIO return)


record :: KbtzAPI (AsServerT GraphM)
record = KbtzAPI
  { _addKbtz = \k -> withKbtzPool (flip K.addKbtz k)
  , _addNode = \k n -> withKbtzPool $ \c -> do
      K.addNodeToKbtz c k (nodeId n)
      K.addHWToHH c (nodeId n) (nodeConfig n)
  , _reassociateNode = \k k' n -> do
      ns <- withKbtzPool $ \c -> K.getNode c n
      case length ns of
        0 -> return False
        1 -> withKbtzPool $ \c -> do
          K.removeNodeFromKbtz c k n
          K.addNodeToKbtz c k' n
          return True
        _ -> return False
  , _reassociateNodeMAC = \n n' -> do
      withKbtzPool $ \c -> K.updateNodeMAC c n n'
  , _removeNodeFromKbtz = \k n -> withKbtzPool $ \c -> do
      K.removeNodeFromKbtz c k n
  , _getKbtzim = withKbtzPool $ \c -> do
      K.getKbtzim c
  , _getKbtz = \k -> withKbtzPool $ \c -> do
      ns <- K.getKbtzNodes c k
      nsHw <- sequenceA (K.getNodeHW c <$> ns)
      let vs = zip ns nsHw
          g = undefined
      return $ g
  , _getNode = \k n -> do
      nx <- withKbtzPool (\c -> K.getNode c n)
      -- case length nx of
      --   0 ->
      return undefined
  }

kbtzApp :: DBPools -> Application
kbtzApp p = genericServeT (runGraphWithDB p) record
