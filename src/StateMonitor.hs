{-# LANGUAGE DeriveGeneric #-}
module StateMonitor where

import qualified StmContainers.Map as SMap

import Node (defaultES, EnergyState, defNodeS, NodeId(..), NodeS)

import Control.Concurrent.STM
import Data.Hashable

import GHC.Generics (Generic)

import Data.Maybe (fromMaybe, fromJust)

import qualified Data.Text as Text

{-------------------------------------------------------------------------------

             A KibbutzMonitor is an STM protected map that is used
             as a bridge between streams of data and rendering functions
             that always require a view.


--------------------------------------------------------------------------------}


newtype KibbutzMonitor k v = KM { runKM :: SMap.Map k v } deriving (Generic)

initKM :: (Eq k, Hashable k) => [k] -> v -> STM (KibbutzMonitor k v)
initKM ns def = do
    m <-  SMap.new
    --mapM_ (\n -> SMap.insert def n m) ns
    return $ KM m

updateKM :: (Eq k, Hashable k) => KibbutzMonitor k v -> k -> v ->  STM ()
updateKM m n v = do
  SMap.insert v n (runKM m)

readKM :: (Eq k, Hashable k, Eq v) => KibbutzMonitor k v -> [k] -> STM [(k, v)]
readKM (KM km) ns = do
  ns' <- mapM (flip SMap.lookup km) ns
  let
    ns''' = map (fmap fromJust) (filter (\(_, i)-> i /= Nothing) $ zip ns ns')
  return $ ns'''

lookupKM :: (Eq k, Hashable k) => KibbutzMonitor k v -> k -> STM (Maybe v)
lookupKM = (flip SMap.lookup) . runKM

type ThingName = Text.Text

type NodeT = NodeId ThingName

type KMState = KibbutzMonitor NodeT NodeS

initKMS :: [NodeT] -> STM (KMState)
initKMS ns = initKM ns defNodeS

type KMSensor = KibbutzMonitor NodeT EnergyState

initKSensorM :: [NodeT] -> STM (KMSensor)
initKSensorM ns = initKM ns defaultES

type KConnM = KibbutzMonitor NodeT Int

initKMConn :: [NodeT] -> STM KConnM
initKMConn ns = initKM ns 0


type MonitorAtT a b c = ([(a, b)], [(a, c)])

getMonitorState :: (Hashable a, Eq a, Eq b, Eq c) => KibbutzMonitor a b -> KibbutzMonitor a c -> [a] -> STM (MonitorAtT a b c)
getMonitorState nodeStates connStates nodes = do
  currentNodeStates <- readKM nodeStates nodes
  currentConnectionCounts <- readKM connStates nodes
  return $ (currentNodeStates, currentConnectionCounts)

--  KMState -> KConnM -> (NodeT, NodeS)
updateMonitorState :: (Hashable a, Eq a, Num c) => KibbutzMonitor a b -> KibbutzMonitor a c -> (a, b) -> STM ()
updateMonitorState kmState kConnM (n, ns) = do
  updateKM kmState n ns
  c <- lookupKM kConnM n
  updateKM kConnM n ((fromMaybe 0 c) + 1)
