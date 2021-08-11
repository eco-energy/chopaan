{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving, GeneralizedNewtypeDeriving, DerivingStrategies, DerivingVia, DeriveFunctor, DeriveFoldable, DeriveDataTypeable #-}

{-# LANGUAGE OverloadedStrings, NamedFieldPuns, ScopedTypeVariables, TypeApplications, FlexibleContexts, TypeOperators, UndecidableInstances #-}
module Chopaan.Graph.Kbtz where

import Prelude hiding ((.), id)

import GHC.Generics

import Control.Category
import Control.DeepSeq (NFData)
import Control.Monad
import Control.Monad.IO.Class

import Control.Exception (bracket)

import Data.Function ((&))
import qualified Data.Text as T hiding (zip)
import Data.Aeson (ToJSON(..), FromJSON(..), encode)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)
import qualified Data.Vector as V
import Data.Monoid
import Data.Either
import Data.Greskell.Greskell (Greskell, string)
import Data.Greskell.Graph (AVertex, AEdge, ElementData, Element, Vertex, Edge, Key, Keys(..))
import Data.Greskell.GraphSON (FromGraphSON(..), GValue)
import Data.Greskell.Binder
import Data.Greskell.GTraversal
  ( GTraversal, Walk, Transform, SideEffect, Filter, gAddV, gAddE, gOut, gOutE, gId, gIn, gInV, gInE, gHasLabel, gProperty,
    source, sV, sV', gV, ($.), (&.), unsafeCastStart, unsafeCastEnd, (<*.>), sAddV, gHas2, liftWalk, gFrom, gTo, gSideEffect, gValueMap )
import Data.Greskell.Extra (writeKeyValues, (<=:>), gWhenEmptyInput)
import Data.Greskell.PMap
  ( PMap, Multi, Single, PMapLookupException,
    lookupAs, lookupAs', pMapToFail
  )

import Network.Greskell.WebSocket
  ( connect, close, submitPair,
    slurpResults, drainResults, Client
  )


import NetSpider.Graph (writeNodeAttributes)

import Chopaan.Utils.Retry
import Chopaan.Kibbutz.KbtzId
import Chopaan.Node.NodeId
import Chopaan.Node.HW

import Data.Pool

-- $ Actual DB interactions

type KbtzPool = (Pool Client)

getKbtzim :: MonadIO m => Client -> m [KbtzName]
getKbtzim c = (fmap (fmap akId)) $ fetchResult c (allKbtzim)

getKbtzNodes :: MonadIO m => Client -> KbtzName -> m [NodeMAC]
getKbtzNodes c k = (fmap (fmap anId)) $ (fetchResult c (getKbtzNodes' k))

addHWToHH :: MonadIO m => Client -> NodeMAC -> HW Double -> m ()
addHWToHH c n hw = runTraversal c (addHWToHH' n hw)

getNodeHW :: MonadIO m => Client -> NodeMAC -> m [HW Double]
getNodeHW c n = fetchResult c (getNodeHW' n)

getNodeLastSync :: MonadIO m => Client -> NodeMAC -> m [T.Text]
getNodeLastSync c n = (pure . rights . (fmap parseLS)) =<< fetchResult c (getNodeLastSync' n)

addLastSyncToHH :: MonadIO m => Client -> NodeMAC -> T.Text -> m ()
addLastSyncToHH c n ls = runTraversal c (addLastSyncToHH' n ls)

addKbtz :: MonadIO m => Client -> KbtzName -> m ()
addKbtz c k = runTraversal c (addKbtz' (AKbtz k))

addHHToKbtz :: MonadIO m => Client -> KbtzName -> ANode -> m ()
addHHToKbtz c k n = runTraversal c (addHHToKbtz' k n)

addNodeToKbtz :: MonadIO m => Client -> KbtzName -> NodeMAC -> m ()
addNodeToKbtz c k n = addHHToKbtz c k (ANode n)

kbtzPool :: String -> Int -> IO (KbtzPool)
kbtzPool host port = createPool ((recoverC "retrying kbtz janusgraph connection" 100) (connect host port)) close 10 100 10

fetchResult c = (pure . V.toList) <=< (liftIO . slurpResults) <=< (liftIO . submitPair c . runBinder)
runTraversal c = (liftIO . drainResults) <=< (liftIO . submitPair c . runBinder)



newtype VKbtz = VKbtz AVertex
  deriving (Eq, Show)
  deriving newtype (FromGraphSON, ElementData, Element, Vertex)

newtype EKbtzIncludes = EKbtzIncludes AEdge
  deriving (Eq, Show)
  deriving newtype (FromGraphSON, ElementData, Element, Edge)

newtype VHH = VHH AVertex
  deriving (Eq, Show)
  deriving newtype (FromGraphSON, ElementData, Element, Vertex)



kbtzIncludesTar :: Walk Transform VKbtz VHH
kbtzIncludesTar = gOut ["kbtzIncludes"]

kbtzIncludesSrc :: Walk Transform VHH VKbtz
kbtzIncludesSrc = gIn ["kbtzIncludes"]

-- A Kbtz is not a node, it's a graph where the vertices are households
-- and the edges are dunno. But AKbtz is a hypergraph node.
data AKbtz = AKbtz
  { akId :: KbtzName
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, NFData)

parseAKbtz :: PMap Multi GValue -> Either PMapLookupException AKbtz
parseAKbtz pm = AKbtz <$> (lookupAs akKey pm)
  where
    akKey :: Key VKbtz KbtzName
    akKey = "@kbtz_id"

instance FromGraphSON AKbtz where
  parseGraphSON gv = (pMapToFail . parseAKbtz) =<< parseGraphSON gv

data ANode = ANode
  { anId :: NodeMAC
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, NFData)


parseANode :: PMap Multi GValue -> Either PMapLookupException ANode
parseANode pm = ANode <$> (lookupAs anKey pm)
  where
    anKey :: Key VHH NodeMAC
    anKey = "@hh_id"

instance FromGraphSON ANode where
  parseGraphSON gv = (pMapToFail . parseANode) =<< parseGraphSON gv


addKbtz' :: AKbtz -> Binder (GTraversal SideEffect () VKbtz)
addKbtz' k = do
  kid <- newBind $ akId k
  let
    addId :: Binder (Walk SideEffect VKbtz VKbtz)
    addId = return $ gProperty "@kbtz_id" kid
    addType :: Binder (Walk SideEffect VKbtz VKbtz)
    addType = return $ gProperty "@node_type" (string ("k" :: T.Text))
  addType <*.> addId <*.> (pure $ sAddV "kbtz" $ source "g")



addHH' :: ANode -> Binder (GTraversal SideEffect () VHH)
addHH' nx = do
  n <- newBind $ anId nx
  let addId = return $ gProperty "@hh_id" n
      addType = return $ gProperty "@node_type" (string ("h" :: T.Text))
  addType <*.> addId <*.> (pure $ sAddV "hh" $ source "g")


addHHToKbtz' :: KbtzName -> ANode -> Binder (GTraversal SideEffect () EKbtzIncludes) 
addHHToKbtz' k n = (addBelongsToE k) <*.> (addHH' n)
  where
    addBelongsToE :: KbtzName -> Binder (Walk SideEffect VHH EKbtzIncludes)
    addBelongsToE (KbtzId x) = do
      k' <- newBind x
      return $
        gAddE "kbtzIncludes" (gFrom (gV @VHH [] >>> gHas2 "@kbtz_id" k'))



allV :: GTraversal Transform () AVertex
allV = source "g" & sV []

isKbtz :: Walk Filter VKbtz VKbtz
isKbtz = gHas2 "@node_type" (string "k") >>> gHasLabel "kbtz"

isHH :: Walk Filter VHH VHH
isHH = gHas2 "@node_type" (string "h") >>> gHasLabel "hh"

allKbtz :: GTraversal Transform () VKbtz
allKbtz =  source "g" & sV [] &. (liftWalk isKbtz)

allHH :: GTraversal Transform () VHH
allHH = source "g" & sV [] &. (liftWalk isHH)

getHHById' :: Greskell NodeMAC -> Walk Filter VHH VHH
getHHById' n = gHas2 "@hh_id" n

getVHHById :: Greskell NodeMAC -> GTraversal Transform () VHH
getVHHById n = allHH &. (liftWalk $ getHHById' n)

getHHById :: NodeMAC -> Binder (GTraversal Transform () ANode)
getHHById n = do
  n' <- newBind n
  return $ allHH &. (liftWalk $ getHHById' n') &. toANode


toANode :: Walk Transform VHH ANode
toANode = unsafeCastEnd aNodeProps
  where
    aNodeProps = gValueMap KeysNil

allKbtzim :: Binder (GTraversal Transform () AKbtz)
allKbtzim = pure $ allKbtz &. (liftWalk toAKbtz)

getKbtzById'' :: KbtzName -> Binder (Walk Filter VKbtz VKbtz)
getKbtzById'' k = do
  k' <- newBind k
  return $ gHas2 "@kbtz_id" k'

getKbtzVById' :: KbtzName -> Binder (GTraversal Transform () VKbtz)
getKbtzVById' k = do
  k' <- getKbtzById'' k
  return $ allKbtz &. (liftWalk k')

getKbtzById' :: KbtzName -> Binder (GTraversal Transform () AKbtz)
getKbtzById' k = do
  k' <- getKbtzVById' k
  return $ k' &. toAKbtz

toAKbtz :: Walk Transform VKbtz AKbtz
toAKbtz = unsafeCastEnd aKbtzProps
  where
    aKbtzProps = gValueMap KeysNil


getKbtzNodes'' :: Walk Transform VKbtz ANode
getKbtzNodes'' = toANode <<< kbtzIncludesTar

getKbtzNodes' :: KbtzName -> Binder (GTraversal Transform () ANode)
getKbtzNodes' k = do
  k' <- getKbtzVById' k
  return $ k' &. getKbtzNodes''



newtype VHW = VHW AVertex
  deriving (Eq, Show)
  deriving newtype (FromGraphSON, ElementData, Element, Vertex)


addHW' :: HW Double -> Binder (GTraversal SideEffect () VHW)
addHW' hw = (addProps hw) <*.> (pure $ sAddV "hwConfig" $ source "g")
  where
    addProps :: HW Double -> Binder (Walk SideEffect VHW VHW)
    addProps hw = (unsafeCastStart . unsafeCastEnd) <$> (writeNodeAttributes hw)
    addHWProps :: HW Double -> Binder (Walk SideEffect VHW VHW)
    addHWProps hw' = fmap writeKeyValues $ sequence $
      [ storagKey <=:> (storage hw')
      , generatioKey <=:> (generation hw')
      , loaKey <=:> (loads hw')
      ]
    textS :: ToJSON a => a -> T.Text
    textS = decodeUtf8 . toStrict . encode . toJSON

--storageKey :: (Num a) => Key VHW (BatteryTop a)
storagKey = "hw_storage"
    --generationKey :: (Num a) => Key VHW (PVTop a)
generatioKey = "hw_generation"
--loadKey :: (Num a) => Key VHW (LoadTop a)
loaKey = "hw_load"    


toHW :: Walk Transform VHW (HW Double)
toHW = unsafeCastEnd aHWProps
  where
    aHWProps = gValueMap KeysNil


newtype EHHHasHW = EHasHW AEdge
  deriving (Eq, Show)
  deriving newtype (FromGraphSON, ElementData, Element, Edge)

newtype HHLastSynced = HHLastSynced AEdge
  deriving (Eq, Show)
  deriving newtype (FromGraphSON, ElementData, Element, Edge)

newtype VLastSync = VLastSync AVertex
  deriving (Eq, Show)
  deriving newtype (FromGraphSON, ElementData, Element, Vertex)


addLastSyncToHH' :: NodeMAC -> T.Text -> Binder (GTraversal SideEffect () VLastSync) 
addLastSyncToHH' n ls = do
  n' <- newBind n
  l' <- newBind ls
  return $ (upsert n') &. (gProperty "lastSynced" l')
  where
    upsert :: Greskell NodeMAC -> GTraversal SideEffect () VLastSync 
    upsert n = (liftWalk (getNodeLSWalk n) &. gWhenEmptyInput ((liftWalk gInV)
                                                               <<< (hhEdge n)
                                                               <<< insert))
    insert :: Walk SideEffect [VLastSync] VLastSync
    insert = (gAddV "hhLastSync")
    hhEdge :: Greskell NodeMAC -> Walk SideEffect VLastSync HHLastSynced
    hhEdge n' = gAddE "hasLS" (gFrom (gV @VLastSync [] >>> gHas2 "@hh_id" n'))


hhHasLS :: Walk Transform VHH VLastSync
hhHasLS = gOut ["hasLS"]

hhHasHW :: Walk Transform VHH VHW
hhHasHW = gOut ["hasHW"]

-- hhHasStorage :: Walk Transform VHH VStorage
-- hhHasStorage = gOut ["hasStorage"]

-- hhHasGeneration :: Walk Transform VHH VGeneration
-- hhHasGeneration = gOut ["hasGeneration"]

-- hhHasLoad :: Walk Transform VHH VLoad
-- hhHasLoad = gOut ["hasLoad"]


addHWToHH' :: NodeMAC -> (HW Double) -> Binder (GTraversal SideEffect () EHHHasHW) 
addHWToHH' n hw = (hhHas) <*.> (addHW' hw)
  where
    hhHas = do
      n' <- newBind n
      return $
        gAddE "hasHW" (gFrom (gV @VHW [] >>> gHas2 "@hh_id" n'))


getNodeHWPM :: NodeMAC -> Binder (GTraversal Transform () (PMap Multi GValue))
getNodeHWPM n = do
  n' <- newBind n
  return $ (getVHHById n') &. ((gValueMap KeysNil) <<< hhHasHW)

parseHW :: PMap Multi GValue -> Either PMapLookupException (HW Double)
parseHW pm = HW
             <$> (lookupAs storagKey pm)
             <*> (lookupAs generatioKey pm)
             <*> (lookupAs loaKey pm)


parseLS :: PMap Multi GValue -> Either PMapLookupException (T.Text)
parseLS pm = lookupAs lsKey pm
  where
    lsKey :: Key VLastSync T.Text
    lsKey = "lastSynced"

getNodeLSWalk :: Greskell NodeMAC -> GTraversal Transform () VLastSync
getNodeLSWalk n = getVHHById n &. hhHasLS

getNodeLastSync' :: NodeMAC -> Binder (GTraversal Transform () (PMap Multi GValue))
getNodeLastSync' n = (\n' -> return $ (getVHHById n') &. ((gValueMap KeysNil) <<< hhHasLS))
  =<< (newBind n)


getNodeHW' :: NodeMAC -> Binder (GTraversal Transform () (HW Double))
getNodeHW' n = do
  n' <- newBind n
  return $ getVHHById n' &. (toHW <<< hhHasHW)




{--

newtype VPerson = VPerson AVertex
  deriving (Eq, Show)
  deriving newtype (FromGraphSON, ElementData, Element, Vertex)

newtype EOwnsHW = EOwnsHW AEdge
  deriving (Eq, Show)
  deriving newtype (FromGraphSON, ElementData, Element, Edge)

gOutHasHW :: Walk Transform VHH VHW
gOutHasHW = gOut ["hasHW"]

gInOwnsHW :: Walk Transform VHW VPerson
gInOwnsHW = gIn ["ownsHW"]




addHWConfig :: KbtzName -> NodeMAC -> (HW Double) -> Binder (GTraversal SideEffect () VHW)
addHWConfig k n s = (writeHWConfig s)
  <*.> (pure $ sAddV "hwConfig" $ source "g")


--}
