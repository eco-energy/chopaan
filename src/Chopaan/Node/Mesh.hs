{-# LANGUAGE DeriveGeneric
, DeriveFunctor
, GeneralizedNewtypeDeriving
, DeriveFoldable
, DeriveTraversable
, DerivingStrategies
, DeriveGeneric
, DeriveAnyClass
, DerivingVia
, StandaloneDeriving
#-}
{-# LANGUAGE ScopedTypeVariables
, TypeOperators
, TypeApplications
, RankNTypes
, FlexibleContexts
, InstanceSigs
, OverloadedStrings
, NamedFieldPuns
#-}
module Chopaan.Node.Mesh where

import Prelude

import Control.DeepSeq
import Control.Monad.IO.Class
import Lens.Micro
import qualified Proto.NodeMessageSchema.NodeMessages as N
import qualified Proto.NodeMessageSchema.NodeMessages_Fields as N
import GHC.Generics

import Data.Monoid (Sum(..), Last(..))
import Data.Maybe

import Data.ProtoLens
import Data.Time (DiffTime(..), UTCTime(..), Day(..))
import Data.Text
import Data.Binary
import Data.Aeson (FromJSON, ToJSON)
import Chopaan.Comm.Comm (Address(..))
import Chopaan.Node.NodeId
import Chopaan.Utils.Time (utcTimeNow)
import qualified Streamly.Prelude as S
import Streamly.Prelude (IsStream, MonadAsync, adapt)
import qualified Streamly.Internal.Data.Fold as FL

import Data.Greskell (newBind, gProperty, lookupAs, lookupAs', Key, pMapToFail)
import Data.Greskell.Extra (writeKeyValues, (<=:>))
import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(..))
import NetSpider.Spider
  (Spider, addFoundNode, withSpider)
import NetSpider.Spider.Config (Config(..))
import NetSpider.Graph (LinkAttributes(..), EFinds, NodeAttributes(..), VFoundNode)
import NetSpider.Timestamp (fromUTCTime)


newtype RxSignal = RxSignal (Maybe Double)
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON, NFData, Binary)
  deriving (Semigroup, Monoid) via (Last Double)

instance LinkAttributes RxSignal where
  writeLinkAttributes (RxSignal s) = do
    sv <- newBind s
    return $ gProperty "rx_signal" sv
  parseLinkAttributes props =
    pMapToFail $ RxSignal <$> lookupAs' ("rx_signal" :: Key EFinds (Maybe Double)) props


data MeshLink = MeshLink
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON, NFData)


data MeshNode = MeshNode
  { isRoot :: Bool
  , uptime :: Int
  , routerRSSI :: Int
  , version :: Maybe Text
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, NFData)


instance Semigroup MeshNode where
  a <> b = if uptime a >= uptime b then a else b
  
instance Monoid MeshNode where
  mempty = MeshNode False 0 0 Nothing

rootKey :: Key VFoundNode Bool
rootKey = "isRoot"

uptimeKey :: Key VFoundNode Int
uptimeKey = "uptime"

routerRSSIKey :: Key VFoundNode Int
routerRSSIKey = "routerRSSI"

versionKey :: Key VFoundNode (Maybe Text)
versionKey = "version"

instance NodeAttributes MeshNode where
  writeNodeAttributes n = fmap writeKeyValues $
                          sequence $
                          [ rootKey <=:> isRoot n
                          , uptimeKey <=:> (uptime n)
                          , routerRSSIKey <=:> routerRSSI n
                          , versionKey
                            <=:> (version n)
                          ]
  parseNodeAttributes props =
    pMapToFail (MeshNode
                 <$> lookupAs rootKey props
                 <*> (lookupAs uptimeKey props)
                 <*> lookupAs routerRSSIKey props
                 <*> lookupAs' versionKey props
               )


addRTS :: (MonadIO m)
        => Spider NodeMAC MeshNode RxSignal
        -> (NodeMAC, N.RuntimeStats)
        -> m ()
addRTS spider (n, rts) = liftIO $ addFoundNode spider $ rsToFN (n, rts)


rsToFN :: (NodeMAC, N.RuntimeStats) -> FoundNode NodeMAC MeshNode RxSignal
rsToFN (n, rts) = let
  finding = FoundNode { subjectNode = n
                      , foundAt = fromUTCTime . utcTimeNow $ rts ^. N.cpuTime 
                      , neighborLinks = [link]
                      , nodeAttributes = parseRTSToNode rts
                      }
    
  link = FoundLink { targetNode = NodeId (rts ^. N.parent ^. N.macAddr)
                   , linkState=LinkToSubject
                   , linkAttributes = parseRxSignal rts
                   }
  in finding


parseRTSToNode :: N.RuntimeStats -> MeshNode
parseRTSToNode rts = MeshNode
  { isRoot = (rts ^. N.isRoot)
  , uptime = (fromIntegral $ rts ^. N.uptime)
  , routerRSSI = (fromIntegral $ rts ^. N.wifiStrength)
  , version = (Just $ rts ^. N.version) }

parseRxSignal :: N.RuntimeStats -> RxSignal
parseRxSignal rts = RxSignal . Just . fromIntegral $ rts ^. N.meshParentStrength

nodeLinkPair :: N.RuntimeStats -> (MeshNode, RxSignal)
nodeLinkPair rts = (parseRTSToNode rts, parseRxSignal rts)

meshF :: forall m. (Applicative m) => FL.Fold m (N.RuntimeStats) (MeshNode, RxSignal)
meshF = FL.Fold step i o
  where
    step :: (MeshNode, RxSignal)
      -> N.RuntimeStats
      -> m (FL.Step (MeshNode, RxSignal) (MeshNode, RxSignal))
    step _ r = let p = (nodeLinkPair r)
                   in pure . FL.Done $ p 
    i = pure (mempty, mempty)
    o = pure
