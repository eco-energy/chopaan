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

import Data.Monoid (Sum(..), Last(..))
import Data.Maybe
import Data.Either
import Data.Int
import Data.ProtoLens
import Data.Time (DiffTime, UTCTime(..), Day(..))
import Data.Text as T
import Data.Binary
import Data.Aeson (FromJSON(..), ToJSON)
import Chopaan.Node.NodeId
import Chopaan.Utils.Time (utcTimeNow)
import qualified Streamly.Prelude as S
import Streamly (IsStream, MonadAsync, adapt)
import qualified Streamly.Internal.Data.Fold as FL

import Data.Greskell.GraphSON.GValue (unwrapOne)
import Data.Greskell (newBind, gProperty, lookupAs, lookupAs', Key, pMapToFail, FromGraphSON(..))
import Data.Greskell.Extra (writeKeyValues, (<=:>), (<=?>))

#ifndef ghcjs_HOST_OS
import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(..))
import NetSpider.Spider
  (Spider, addFoundNode, withSpider)

import NetSpider.Graph (LinkAttributes(..), EFinds, NodeAttributes(..), VFoundNode)
import NetSpider.Timestamp (fromUTCTime)
#endif

newtype RxSignal = RxSignal (Maybe Double)
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON, NFData, Binary)
  deriving (Semigroup, Monoid) via (Last Double)

#ifndef ghcjs_HOST_OS
instance LinkAttributes RxSignal where
  writeLinkAttributes (RxSignal s) = do
    sv <- newBind s
    return $ gProperty "rx_signal" sv
  parseLinkAttributes props =
    pMapToFail $ RxSignal <$> lookupAs' ("rx_signal" :: Key EFinds (Maybe Double)) props
#endif

data MeshLink = MeshLink
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON, NFData)


newtype NodeVersion = NodeVersion (Text)
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, NFData, FromGraphSON)

-- instance FromGraphSON NodeVersion where
--   parseGraphSON = parseJSON . unwrapOne

data MeshNode = MeshNode
  { isRoot :: Maybe Bool
  , uptime :: Int64
  , routerRSSI :: Int
  , version :: Maybe NodeVersion
  }
  deriving (Eq, Ord, Show, Generic, NFData)


instance ToJSON MeshNode where
  
instance FromJSON MeshNode where
  

initMeshNode :: MeshNode
initMeshNode = MeshNode Nothing 0 0 Nothing
{-# INLINE initMeshNode #-}


parseRTSToNode :: N.RuntimeStats -> MeshNode
parseRTSToNode rts = m
  where
    {-# INLINE m #-}
    m = MeshNode
      { isRoot = (rts ^? N.isRoot)
      , uptime = (fromIntegral $ rts ^. N.uptime)
      , routerRSSI = (fromIntegral $ rts ^. N.wifiStrength)
      , version = (Just . NodeVersion $ rts ^. N.version)
  }
{-# INLINE parseRTSToNode #-}

parseRxSignal :: N.RuntimeStats -> RxSignal
parseRxSignal rts = RxSignal . Just . fromIntegral $ rts ^. N.meshParentStrength
{-# INLINE parseRxSignal #-}

nodeLinkPair :: N.RuntimeStats -> (MeshNode, RxSignal)
nodeLinkPair rts = (parseRTSToNode rts, parseRxSignal rts)
{-# INLINE nodeLinkPair #-}


meshF :: forall m. (Applicative m) => FL.Fold m (N.RuntimeStats) (MeshNode, RxSignal)
meshF = FL.Fold (\_ r -> pure . nodeLinkPair $ r) (pure (initMeshNode, mempty)) pure
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

instance NodeAttributes MeshNode where
  writeNodeAttributes n = fmap writeKeyValues $
                          sequence $
                          [ rootKey <=?> isRoot n
                          , uptimeKey <=:> (T.pack . show . uptime $ n)
                          , routerRSSIKey <=:> routerRSSI n
                          , versionKey <=?> (version n)
                          ]
  parseNodeAttributes props = pMapToFail (MeshNode
                 <$> lookupAs' rootKey props
                 <*> (onE (lookupAs uptimeKey props))
                 <*> lookupAs routerRSSIKey props
                 <*> lookupAs' versionKey props
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

#endif
