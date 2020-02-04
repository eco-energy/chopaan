{-# LANGUAGE DeriveGeneric #-}
module StateMonitor where

import qualified StmContainers.Map as SMap

import Node (defNodeS, NodeId(..), NodeS)

import Control.Concurrent.STM
import Data.Hashable

import GHC.Generics (Generic)

import Data.Maybe (fromJust)

import qualified Data.Text as Text

{-------------------------------------------------------------------------------

             A KibbutzMonitor is an STM protected map that is used
             as a bridge between streams of data and rendering functions
             that always require a view.


--------------------------------------------------------------------------------}

runStateMonitor = undefined

newtype KibbutzMonitor k v = KM { runKM :: SMap.Map k v } deriving (Generic)

initKM :: (Eq k, Hashable k, Eq v) => [k] -> v -> STM (KibbutzMonitor k v)
initKM ns def = do
    m <-  SMap.new
    mapM_ (\n -> SMap.insert def n m) ns
    return $ KM m

updateKM :: (Eq k, Hashable k) => KibbutzMonitor k v -> k -> v ->  STM ()
updateKM m n v = do
  SMap.insert v n (runKM m)

readKM :: (Eq k, Hashable k, Eq v) => KibbutzMonitor k v -> [k] -> STM [(k, v)]
readKM (KM km) ns = do
  ns' <- mapM (flip SMap.lookup km) ns
  let
    ns''' = map (fmap fromJust) (filter (\(x, i)-> i /= Nothing) $ zip ns ns')
  return $ ns'''

lookupKM :: (Eq k, Hashable k) => KibbutzMonitor k v -> k -> STM (Maybe v)
lookupKM = (flip SMap.lookup) . runKM

type ThingName = Text.Text

type NodeT = NodeId ThingName

type KMState = KibbutzMonitor NodeT NodeS

initKMS :: [NodeT] -> STM (KMState)
initKMS ns = initKM ns defNodeS


type KConnM = KibbutzMonitor NodeT Int

initKMConn :: [NodeT] -> STM KConnM
initKMConn ns = initKM ns 0
