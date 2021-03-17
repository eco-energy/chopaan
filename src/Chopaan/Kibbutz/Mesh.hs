{-# LANGUAGE DeriveGeneric, DeriveFunctor, GeneralizedNewtypeDeriving, DeriveFoldable, DeriveTraversable, DerivingStrategies, NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables
, TypeOperators
, TypeApplications
, RankNTypes
, FlexibleContexts
, InstanceSigs
, OverloadedStrings
#-}
module Chopaan.Kibbutz.Mesh where

import Prelude

import Control.Monad.IO.Class
import Data.ProtoLens
import Lens.Micro
import qualified Proto.NodeMessageSchema.NodeMessages as N
import qualified Proto.NodeMessageSchema.NodeMessages_Fields as N
import GHC.Generics

import Data.Time (DiffTime, LocalTime)
import Data.Text
import Data.Aeson (FromJSON, ToJSON)
import Chopaan.Comm.Comm (Address(..))
import Chopaan.Node.NodeId
import Chopaan.Utils.Time (utcTimeNow)

import qualified Streamly.Prelude as S
import Streamly
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as FL


import Data.Greskell (newBind, gProperty, lookupAs, Key, pMapToFail)
import Data.Greskell.Extra (writeKeyValues, (<=:>))
import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(..))
import NetSpider.Spider
  (Spider, connectWS, close, addFoundNode, clearAll, getSnapshotSimple)
import NetSpider.Graph (LinkAttributes(..), EFinds, NodeAttributes(..), VFoundNode)
import NetSpider.Timestamp (Timestamp, fromUTCTime)
import NetSpider.Snapshot (nodeId, nodeTimestamp)
import qualified NetSpider.Snapshot as Sn

{--
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.isRoot' @:: Lens' RuntimeStats Prelude.Bool@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.connectedChildren' @:: Lens' RuntimeStats Data.Word.Word32@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.wifiStrength' @:: Lens' RuntimeStats Data.Word.Word32@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.meshParentStrength' @:: Lens' RuntimeStats Data.Word.Word32@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.version' @:: Lens' RuntimeStats Data.Text.Text@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.uptime' @:: Lens' RuntimeStats Data.Word.Word64@
 -}

newtype RxSignal = RxSignal Double

instance LinkAttributes RxSignal where
  writeLinkAttributes (RxSignal s) = do
    sv <- newBind s
    return $ gProperty "rx_signal" sv
  parseLinkAttributes props =
    pMapToFail $ RxSignal <$> lookupAs ("rx_signal" :: Key EFinds Double) props


data MeshLink = MeshLink deriving (Eq, Show, Ord)

data MeshNode = MeshNode
  { isRoot :: Bool
  , uptime :: DiffTime
  , routerRSSI :: Int
  , version :: Text
  
  } deriving (Eq, Ord, Show, Generic)

rootKey :: Key VFoundNode Bool
rootKey = "isRoot"

uptimeKey :: Key VFoundNode Int
uptimeKey = "uptime"

routerRSSIKey :: Key VFoundNode Int
routerRSSIKey = "routerRSSI"

versionKey :: Key VFoundNode Text
versionKey = "version"

instance NodeAttributes MeshNode where
  writeNodeAttributes n = fmap writeKeyValues $
                          sequence $
                          [ rootKey <=:> isRoot n
                          , uptimeKey <=:> (truncate $ uptime n)
                          , routerRSSIKey <=:> routerRSSI n
                          , versionKey <=:> version n
                          ]
  parseNodeAttributes props =
    pMapToFail (MeshNode
                 <$> lookupAs rootKey props
                 <*> (fromIntegral <$> lookupAs uptimeKey props)
                 <*> lookupAs routerRSSIKey props
                 <*> lookupAs versionKey props
               )
spiderStream :: forall t m n a c d. (IsStream t, MonadAsync m, Address n)
  => (Spider n c d -> (n, a) -> m ())
  -> Spider n c d
  -> t m (n, a)
  -> m ()
spiderStream save spider xs = S.drain
  $ adapt
  $ S.sequence
  $ fmap (save spider) xs


rsStream :: (IsStream t, MonadAsync m) => Spider NodeMAC MeshNode RxSignal
  -> t m (NodeMAC, N.RuntimeStats) -> m ()
rsStream = (spiderStream fromRTS)


fromRTS :: (MonadIO m)
        => Spider NodeMAC MeshNode RxSignal
        -> (NodeMAC, N.RuntimeStats)
        -> m ()
fromRTS spider (n, rts) = liftIO $ addFoundNode spider finding
  where
    finding = FoundNode { subjectNode= n
                        , foundAt = fromUTCTime . utcTimeNow $ rts ^. N.cpuTime
                        , neighborLinks = [link]
                        , nodeAttributes = parseRTSToNode rts
                        }
    
    link = FoundLink { targetNode = NodeId (rts ^. N.parent ^. N.macAddr)
                     , linkState=LinkToSubject
                     , linkAttributes = RxSignal (fromIntegral $ rts ^. N.meshParentStrength)
                     }

parseRTSToNode :: N.RuntimeStats -> MeshNode
parseRTSToNode rts = MeshNode
  { uptime = (fromIntegral $ rts ^. N.uptime)
  , isRoot = (rts ^. N.isRoot)
  , routerRSSI = (fromIntegral $ rts ^. N.wifiStrength)
  , version = (rts ^. N.version) }
