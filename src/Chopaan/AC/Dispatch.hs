{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Production AC Power Flow Dispatcher
--
-- Integrates neural network dispatch with async validation and Streamly streams.
--
-- Architecture:
--
-- @
-- ┌─────────────────────────────────────────────────────────────────┐
-- │                        Production Flow                          │
-- └─────────────────────────────────────────────────────────────────┘
--
--               ┌──────────────┐
--               │  Telemetry   │
--               │  (1000/sec)  │
--               └──────┬───────┘
--                      │
--                      ▼
--               ┌──────────────┐
--               │   Streamly   │
--               │   Aggregate  │
--               └──────┬───────┘
--                      │
--                      ▼
--          ┌───────────────────────┐
--          │                       │
--          ▼                       ▼
-- ┌─────────────────┐    ┌─────────────────┐
-- │  Neural Net     │    │  Async Validator│
-- │  (~1ms)         │    │  (SBV/dReal)    │
-- │                 │    │  (~10s)         │
-- └────────┬────────┘    └────────┬────────┘
--          │                      │
--          │                      │ every N dispatches
--          ▼                      ▼
-- ┌─────────────────┐    ┌─────────────────┐
-- │  Dispatch Cmd   │    │  Retrain if     │
-- │  to Inverters   │    │  error > thresh │
-- └─────────────────┘    └─────────────────┘
-- @

module Chopaan.AC.Dispatch
  ( -- * Configuration
    DispatchConfig(..)
  , defaultDispatchConfig
    -- * State
  , DispatchState
  , initDispatchState
    -- * Commands
  , DispatchCmd(..)
  , KibbutzState(..)
    -- * Streaming Dispatcher
  , runDispatcher
  , dispatchOne
    -- * Utilities
  , stateToACProblem
  ) where

#ifndef ghcjs_HOST_OS

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM
import Control.Monad (when, void)
import GHC.Generics
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON, FromJSON)
import Data.IORef

import qualified Streamly.Prelude as S
import Streamly.Prelude (IsStream, MonadAsync)

import Chopaan.AC.Solver
import Chopaan.AC.NeuralDispatch

-- | Configuration for the dispatcher
data DispatchConfig = DispatchConfig
  { dcModelPath       :: FilePath   -- ^ Path to trained neural network
  , dcNumNodes        :: Int        -- ^ Number of nodes in the network
  , dcValidateEvery   :: Int        -- ^ Validate every N dispatches
  , dcErrorThreshold  :: Double     -- ^ Trigger retrain if error exceeds
  , dcRetrainPath     :: FilePath   -- ^ Where to save samples for retraining
  , dcVMin            :: Double     -- ^ Minimum voltage (160V)
  , dcVMax            :: Double     -- ^ Maximum voltage (270V)
  } deriving (Show, Eq, Generic, NFData)

-- | Default configuration for Pakistan residential grid
defaultDispatchConfig :: FilePath -> Int -> DispatchConfig
defaultDispatchConfig modelPath nNodes = DispatchConfig
  { dcModelPath = modelPath
  , dcNumNodes = nNodes
  , dcValidateEvery = 100
  , dcErrorThreshold = 5.0  -- 5V total error threshold
  , dcRetrainPath = "retrain_samples.csv"
  , dcVMin = 160
  , dcVMax = 270
  }

-- | Runtime state for the dispatcher
data DispatchState = DispatchState
  { dsNet              :: DispatchNet
  , dsDispatchCount    :: TVar Int
  , dsErrorAccum       :: TVar Double
  , dsPendingValidations :: TVar Int
  , dsRetrainNeeded    :: TVar Bool
  }

-- | Initialize dispatcher state
initDispatchState :: DispatchConfig -> IO DispatchState
initDispatchState DispatchConfig{..} = do
  net <- loadNet dcModelPath dcNumNodes
  dispatchCount <- newTVarIO 0
  errorAccum <- newTVarIO 0
  pendingValidations <- newTVarIO 0
  retrainNeeded <- newTVarIO False

  return DispatchState
    { dsNet = net
    , dsDispatchCount = dispatchCount
    , dsErrorAccum = errorAccum
    , dsPendingValidations = pendingValidations
    , dsRetrainNeeded = retrainNeeded
    }

-- | Kibbutz (grid segment) state for dispatch
data KibbutzState = KibbutzState
  { ksNodes     :: [NodeState]     -- ^ State of each node
  , ksEdges     :: [(Int, Int)]    -- ^ Edge connectivity
  , ksEdgeParams :: Map (Int, Int) EdgeParams  -- ^ Edge parameters
  , ksTimestamp :: Double          -- ^ Timestamp (epoch seconds)
  } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)

-- | Dispatch command to send to inverters
data DispatchCmd = DispatchCmd
  { cmdVoltageSetpoints :: [Double]  -- ^ Target voltages for each node
  , cmdAngleSetpoints   :: [Double]  -- ^ Target phase angles for each node
  , cmdTimestamp        :: Double    -- ^ Command timestamp
  , cmdConfidence       :: Double    -- ^ Neural network confidence
  } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)

-- | Convert KibbutzState to ACProblem for validation
stateToACProblem :: DispatchConfig -> KibbutzState -> ACProblem
stateToACProblem DispatchConfig{..} KibbutzState{..} = ACProblem
  { acNodes = [NodeId i | i <- [0..length ksNodes - 1]]
  , acSlackNode = NodeId 0
  , acNodeState = Map.fromList [(NodeId i, s) | (i, s) <- zip [0..] ksNodes]
  , acEdges = [(NodeId i, NodeId j) | (i, j) <- ksEdges]
  , acEdgeParams = Map.mapKeys (\(i, j) -> (NodeId i, NodeId j)) ksEdgeParams
  , acVMin = dcVMin
  , acVMax = dcVMax
  }

-- | Run the streaming dispatcher
--
-- Takes a stream of KibbutzState updates and produces DispatchCmd outputs
runDispatcher :: (IsStream t, MonadAsync m)
              => DispatchConfig
              -> DispatchState
              -> t m KibbutzState
              -> t m DispatchCmd
runDispatcher cfg state = S.mapM (dispatchOne cfg state)

-- | Dispatch a single state update
--
-- Expected latency: ~1ms for neural network, ~10s for async validation
dispatchOne :: DispatchConfig -> DispatchState -> KibbutzState -> IO DispatchCmd
dispatchOne cfg@DispatchConfig{..} DispatchState{..} state = do
  let pGen = map nodePGen (ksNodes state)
      pLoad = map nodePLoad (ksNodes state)

  -- Fast neural dispatch (~1ms)
  result <- dispatch dsNet pGen pLoad

  -- Increment dispatch count
  count <- atomically $ do
    c <- readTVar dsDispatchCount
    writeTVar dsDispatchCount (c + 1)
    return c

  -- Async validation every N dispatches
  when (count `mod` dcValidateEvery == 0) $ do
    atomically $ modifyTVar' dsPendingValidations (+1)

    void $ async $ do
      let prob = stateToACProblem cfg state
      validationResult <- validateDispatch prob result

      atomically $ do
        modifyTVar' dsPendingValidations (subtract 1)
        modifyTVar' dsErrorAccum (+ vrTotalError validationResult)

        when (not $ vrAcceptable validationResult) $ do
          totalErr <- readTVar dsErrorAccum
          let avgErr = totalErr / fromIntegral (count + 1)
          when (avgErr > dcErrorThreshold) $
            writeTVar dsRetrainNeeded True

      -- Log validation failure
      when (not $ vrAcceptable validationResult) $
        putStrLn $ "Validation failed at dispatch " ++ show count ++
                   ", error: " ++ show (vrTotalError validationResult)

  return DispatchCmd
    { cmdVoltageSetpoints = 220 : drVoltages result  -- Add slack node voltage
    , cmdAngleSetpoints = 0 : drAngles result        -- Add slack node angle (0)
    , cmdTimestamp = ksTimestamp state
    , cmdConfidence = drConfidence result
    }

#else

-- GHCJS stub - Production dispatcher requires native code
import GHC.Generics
import Data.Aeson
import Control.DeepSeq (NFData)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Chopaan.AC.Solver (NodeState(..), EdgeParams(..), NodeId(..), ACProblem(..))
import Chopaan.AC.NeuralDispatch (DispatchNet(..), DispatchResult(..))

data DispatchConfig = DispatchConfig
  { dcModelPath :: FilePath
  , dcNumNodes :: Int
  , dcValidateEvery :: Int
  , dcErrorThreshold :: Double
  , dcRetrainPath :: FilePath
  , dcVMin :: Double
  , dcVMax :: Double
  } deriving (Show, Eq, Generic, NFData)

defaultDispatchConfig :: FilePath -> Int -> DispatchConfig
defaultDispatchConfig modelPath nNodes = DispatchConfig
  { dcModelPath = modelPath
  , dcNumNodes = nNodes
  , dcValidateEvery = 100
  , dcErrorThreshold = 5.0
  , dcRetrainPath = "retrain_samples.csv"
  , dcVMin = 160
  , dcVMax = 270
  }

data DispatchState = DispatchState

data KibbutzState = KibbutzState
  { ksNodes :: [NodeState]
  , ksEdges :: [(Int, Int)]
  , ksEdgeParams :: Map (Int, Int) EdgeParams
  , ksTimestamp :: Double
  } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)

data DispatchCmd = DispatchCmd
  { cmdVoltageSetpoints :: [Double]
  , cmdAngleSetpoints :: [Double]
  , cmdTimestamp :: Double
  , cmdConfidence :: Double
  } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)

initDispatchState :: DispatchConfig -> IO DispatchState
initDispatchState _ = error "Dispatcher not available in GHCJS"

stateToACProblem :: DispatchConfig -> KibbutzState -> ACProblem
stateToACProblem _ _ = error "Dispatcher not available in GHCJS"

dispatchOne :: DispatchConfig -> DispatchState -> KibbutzState -> IO DispatchCmd
dispatchOne _ _ _ = error "Dispatcher not available in GHCJS"

#endif
