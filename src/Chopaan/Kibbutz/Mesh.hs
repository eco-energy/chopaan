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
module Chopaan.Kibbutz.Mesh where

import Prelude

import Control.DeepSeq
import Control.Monad.IO.Class
import Lens.Micro
import qualified Proto.NodeMessageSchema.NodeMessages as N
import qualified Proto.NodeMessageSchema.NodeMessages_Fields as N
import GHC.Generics

import Data.Time (DiffTime(..))
import Data.Text
import Data.Binary
import Data.Aeson (FromJSON, ToJSON)
import Chopaan.Comm.Comm (Address(..))
import Chopaan.Node.NodeId
import Chopaan.Utils.Time (utcTimeNow)
import Data.Monoid (Sum(..))
import qualified Streamly.Prelude as S
import Streamly.Prelude (IsStream, MonadAsync, adapt)

import Data.Greskell (newBind, gProperty, lookupAs, Key, pMapToFail)
import Data.Greskell.Extra (writeKeyValues, (<=:>))
import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(..))
import NetSpider.Spider
  (Spider, addFoundNode)
import NetSpider.Graph (LinkAttributes(..), EFinds, NodeAttributes(..), VFoundNode)
import NetSpider.Timestamp (fromUTCTime)


newtype RxSignal = RxSignal Double
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON, NFData, Binary)
  deriving (Semigroup, Monoid) via (Sum Double)

instance LinkAttributes RxSignal where
  writeLinkAttributes (RxSignal s) = do
    sv <- newBind s
    return $ gProperty "rx_signal" sv
  parseLinkAttributes props =
    pMapToFail $ RxSignal <$> lookupAs ("rx_signal" :: Key EFinds Double) props


data MeshLink = MeshLink
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON, NFData)

--deriving instance Generic DiffTime
--instance Binary DiffTime

data MeshNode = MeshNode
  { isRoot :: Bool
  , uptime :: Integer
  , routerRSSI :: Int
  , version :: Text
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, NFData)
  deriving anyclass (Binary) 


rootKey :: Key VFoundNode Bool
rootKey = "isRoot"

uptimeKey :: Key VFoundNode Integer
uptimeKey = "uptime"

routerRSSIKey :: Key VFoundNode Int
routerRSSIKey = "routerRSSI"

versionKey :: Key VFoundNode Text
versionKey = "version"

instance NodeAttributes MeshNode where
  writeNodeAttributes n = fmap writeKeyValues $
                          sequence $
                          [ rootKey <=:> isRoot n
                          , uptimeKey <=:> (uptime n)
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


addRTS :: (MonadIO m)
        => Spider NodeMAC MeshNode RxSignal
        -> (NodeMAC, N.RuntimeStats)
        -> m ()
addRTS spider (n, rts) = liftIO $ addFoundNode spider finding
  where
    finding = FoundNode { subjectNode = n
                        , foundAt = fromUTCTime . utcTimeNow $ rts ^. N.cpuTime
                        , neighborLinks = [link]
                        , nodeAttributes = parseRTSToNode rts
                        }
    
    link = FoundLink { targetNode = NodeId (rts ^. N.parent ^. N.macAddr)
                     , linkState=LinkToSubject
                     , linkAttributes = parseRxSignal rts
                     }

parseRTSToNode :: N.RuntimeStats -> MeshNode
parseRTSToNode rts = MeshNode
  { uptime = (fromIntegral $ rts ^. N.uptime)
  , isRoot = (rts ^. N.isRoot)
  , routerRSSI = (fromIntegral $ rts ^. N.wifiStrength)
  , version = (rts ^. N.version) }

parseRxSignal :: N.RuntimeStats -> RxSignal
parseRxSignal rts = RxSignal . fromIntegral $ rts ^. N.meshParentStrength

nodeLinkPair :: N.RuntimeStats -> (MeshNode, RxSignal)
nodeLinkPair rts = (parseRTSToNode rts, parseRxSignal rts)
