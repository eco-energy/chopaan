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
, CPP
#-}
module Chopaan.Node.Mesh where

import Prelude

import Control.DeepSeq
import Control.Monad.IO.Class
import Lens.Micro
import qualified Proto.NodeMessageSchema.NodeMessages as N
import qualified Proto.NodeMessageSchema.NodeMessages_Fields as N
import GHC.Generics

import Data.Monoid (Last(..))
import Data.Int
import Data.Text as T
import Data.Binary
import Data.Aeson (FromJSON(..), ToJSON)
import Data.Maybe (fromJust)
import qualified Streamly.Internal.Data.Fold as FL

import Data.Greskell (newBind, gProperty, lookupAs, lookupAs', Key, pMapToFail, FromGraphSON(..))
import Data.Greskell.Extra (writeKeyValues, (<=:>), (<=?>))
import Shpadoinkle.Widgets.Types (Humanize(..))

import Data.Time (UTCTime(..), fromGregorian)
--import Foreign.Storable.Generic

#ifndef ghcjs_HOST_OS
import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(..))
import NetSpider.Spider
  (Spider, addFoundNode)
import NetSpider.Graph (LinkAttributes(..), EFinds, NodeAttributes(..), VFoundNode)
import NetSpider.Timestamp (fromUTCTime)
import Chopaan.Graph.Greskell ()
#endif

import Chopaan.Node.NodeId
import Chopaan.Utils.Time (utcTimeNow)

data RxSignal = RxSignal
  { strength :: (Maybe Double)
  , parent :: Maybe (NodeMAC)
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData, Binary)
  --deriving (Semigroup, Monoid) via (Last Double)
  deriving anyclass (Humanize)

noSignal = RxSignal Nothing Nothing

#ifndef ghcjs_HOST_OS
sigKey :: Key n (Maybe Double)
sigKey = "signalStrength"

parentKey :: Key n (Maybe NodeMAC)
parentKey = "connParent"

instance LinkAttributes RxSignal where
  writeLinkAttributes (RxSignal s p) = fmap writeKeyValues $
                          sequence $
                          [ sigKey <=?> s
                          , parentKey <=?> p
                          ]
  parseLinkAttributes props =
    pMapToFail $ RxSignal
    <$> lookupAs' sigKey props
    <*> lookupAs' parentKey props
#endif

data MeshLink = MeshLink
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON, NFData)

newtype NodeVersion = NodeVersion (Text)
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, NFData, FromGraphSON, Humanize)


data MeshNode = MeshNode
  { isRoot :: Maybe Bool
  , uptime :: Int64
  , routerRSSI :: Int
  , version :: Maybe NodeVersion
  , nodeTime :: UTCTime
  }
  deriving (Eq, Ord, Show, Generic, NFData, Humanize)


instance ToJSON MeshNode where
  
instance FromJSON MeshNode where

initMeshNode :: MeshNode
initMeshNode = MeshNode Nothing 0 0 Nothing t
  where
    t = UTCTime (fromGregorian 1900 1 1) 0
{-# INLINE initMeshNode #-}


parseRTSToNode :: N.RuntimeStats -> MeshNode
parseRTSToNode rts = m
  where
    {-# INLINE m #-}
    m = MeshNode
      { isRoot = (rts ^? N.isRoot)
      , uptime = (fromIntegral $ rts ^. N.uptime)
      , routerRSSI = (fromIntegral $ rts ^. N.wifiStrength)
      , version = (NodeVersion <$> rts ^? N.version)
      , nodeTime = utcTimeNow $ rts ^. N.cpuTime
  }
{-# INLINE parseRTSToNode #-}

parseRxSignal :: N.RuntimeStats -> RxSignal
parseRxSignal rts = RxSignal
                    (fromIntegral <$> rts ^? N.meshParentStrength)
                    (NodeId <$> rts ^? N.parent . N.macAddr)
{-# INLINE parseRxSignal #-}

meshNodeLink :: NodeMAC -> N.RuntimeStats -> (MeshNode, RxSignal)
meshNodeLink kn rts = (parseRTSToNode rts, rx')
  where
    rx = parseRxSignal rts
    rx' = case parent rx of
      Nothing -> rx { parent = Just kn }
      Just (NodeId "") -> rx { parent = Just kn }
      Just (NodeId _) -> rx
{-# INLINE meshNodeLink #-}


meshF :: forall m. (Monad m) => NodeMAC -> FL.Fold m (N.RuntimeStats) (MeshNode, RxSignal)
meshF n = FL.mkFold_ (\_ r -> FL.Partial $ meshNodeLink n $ r) (FL.Partial (initMeshNode, noSignal))
{-# INLINE meshF #-}



#ifndef ghcjs_HOST_OS

rootKey :: Key VFoundNode (Maybe Bool)
rootKey = "isRoot"

uptimeKey :: Key VFoundNode Text
uptimeKey = "uptime"

routerRSSIKey :: Key VFoundNode Int
routerRSSIKey = "routerRSSI"

versionKey :: Key VFoundNode (Maybe NodeVersion)
versionKey = "version"

nodeTimeKey :: Key VFoundNode (UTCTime)
nodeTimeKey = "version"

--instance FromGraphSON UTCTime where
  

instance NodeAttributes MeshNode where
  writeNodeAttributes n = fmap writeKeyValues $
                          sequence $
                          [ rootKey <=?> isRoot n
                          , uptimeKey <=:> (T.pack . show . uptime $ n)
                          , routerRSSIKey <=:> routerRSSI n
                          , versionKey <=?> (version n)
                          , nodeTimeKey <=:> (nodeTime n)
                          ]
  parseNodeAttributes props = pMapToFail (MeshNode
                 <$> lookupAs' rootKey props
                 <*> (onE (lookupAs uptimeKey props))
                 <*> lookupAs routerRSSIKey props
                 <*> lookupAs' versionKey props
                 <*> lookupAs nodeTimeKey props
               )
    where
      onE (Left a) = (Left a)
      onE (Right a) = Right . read . T.unpack $ a

addRTS :: (MonadIO m)
        => Spider NodeMAC MeshNode RxSignal
        -> (NodeMAC, N.RuntimeStats)
        -> m ()
addRTS spider (n, rts) = liftIO $ addFoundNode spider $ rsToFN (n, rts)
{-# INLINE addRTS #-}

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
{-# INLINE rsToFN #-}

sigToFN :: (NodeMAC, (MeshNode, RxSignal)) -> FoundNode NodeMAC MeshNode RxSignal
sigToFN (n, (v, e)) = let
  finding = FoundNode { subjectNode = n
                      , foundAt = fromUTCTime . nodeTime $ v 
                      , neighborLinks = [link]
                      , nodeAttributes = v
                      }
  link = FoundLink { targetNode = fromJust (parent e)
                   , linkState = LinkToSubject
                   , linkAttributes = e
                   }
  in finding
{-# INLINE sigToFN #-}
#endif
