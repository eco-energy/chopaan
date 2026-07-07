{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- | Production AC Power Flow Dispatcher with WAPDA Sync
--
-- Integrates neural network dispatch with:
--   * Async validation against IPOPT solver
--   * WAPDA grid synchronization
--   * Islanding detection and anti-islanding protection
--   * Frequency and voltage monitoring
--   * Reconnection sequence management
--
-- Grid Sync Requirements (Pakistan WAPDA):
--   * Frequency: 50 Hz ± 0.5 Hz
--   * Voltage: 220V ± 10% (198-242V normal, 160-270V extreme)
--   * Phase sequence: L1-L2-L3 (correct order)
--   * RoCoF: < 1 Hz/s (Rate of Change of Frequency)

module Chopaan.AC.Dispatch
  ( -- * Configuration
    DispatchConfig(..)
  , GridSyncConfig(..)
  , defaultDispatchConfig
  , defaultGridSyncConfig
    -- * State
  , DispatchState
  , GridState(..)
  , GridStatus(..)
  , initDispatchState
    -- * Commands
  , DispatchCmd(..)
  , InverterCmd(..)
  , KibbutzState(..)
    -- * Grid Sync
  , checkGridSync
  , detectIslanding
  , reconnectionSequence
    -- * Streaming Dispatcher
  , runDispatcher
  , dispatchOne
  ) where

#ifndef ghcjs_HOST_OS

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM
import Control.Monad (when, void, unless)
import GHC.Generics
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON, FromJSON)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Text.Printf (printf)

import qualified Streamly.Prelude as S
import Streamly.Prelude (IsStream, MonadAsync)

import Chopaan.AC.Solver
import Chopaan.AC.NeuralDispatch

-- | Grid synchronization configuration
data GridSyncConfig = GridSyncConfig
  { gscFreqNominal    :: Double   -- ^ Nominal frequency (50 Hz)
  , gscFreqMin        :: Double   -- ^ Minimum frequency (49.5 Hz)
  , gscFreqMax        :: Double   -- ^ Maximum frequency (50.5 Hz)
  , gscVoltageNominal :: Double   -- ^ Nominal voltage (220 V)
  , gscVoltageMin     :: Double   -- ^ Minimum voltage (160 V)
  , gscVoltageMax     :: Double   -- ^ Maximum voltage (270 V)
  , gscRoCoFLimit     :: Double   -- ^ RoCoF limit (1.0 Hz/s)
  , gscPhaseSeq       :: [Int]    -- ^ Expected phase sequence [1,2,3]
  , gscReconnectDelay :: Double   -- ^ Delay before reconnection (s)
  , gscReconnectRamp  :: Double   -- ^ Power ramp rate on reconnect (kW/s)
  } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)

-- | Default grid sync config for Pakistan WAPDA
defaultGridSyncConfig :: GridSyncConfig
defaultGridSyncConfig = GridSyncConfig
  { gscFreqNominal = 50.0
  , gscFreqMin = 49.5
  , gscFreqMax = 50.5
  , gscVoltageNominal = 220.0
  , gscVoltageMin = 160.0
  , gscVoltageMax = 270.0
  , gscRoCoFLimit = 1.0
  , gscPhaseSeq = [1, 2, 3]
  , gscReconnectDelay = 60.0  -- 60 seconds
  , gscReconnectRamp = 1.0    -- 1 kW/s ramp
  }

-- | Configuration for the dispatcher
data DispatchConfig = DispatchConfig
  { dcModelPath        :: FilePath
  , dcNumNodes         :: Int
  , dcNumPVNodes       :: Int
  , dcMCSamples        :: Int       -- ^ MC Dropout samples for uncertainty
  , dcValidateEvery    :: Int
  , dcErrorThreshold   :: Double
  , dcUncertaintyThreshold :: Double  -- ^ Fall back to solver if uncertainty high
  , dcGridConfig       :: GridSyncConfig
  } deriving (Show, Eq, Generic, NFData)

-- | Default configuration
defaultDispatchConfig :: FilePath -> Int -> Int -> DispatchConfig
defaultDispatchConfig modelPath nNodes nPV = DispatchConfig
  { dcModelPath = modelPath
  , dcNumNodes = nNodes
  , dcNumPVNodes = nPV
  , dcMCSamples = 20
  , dcValidateEvery = 100
  , dcErrorThreshold = 1.0  -- 1 kW total error
  , dcUncertaintyThreshold = 2.0  -- 2 kW uncertainty triggers fallback
  , dcGridConfig = defaultGridSyncConfig
  }

-- | Grid status
data GridStatus
  = GridConnected       -- ^ Normal operation, synced to WAPDA
  | GridIslanded        -- ^ Detected island, anti-islanding active
  | GridReconnecting    -- ^ In reconnection sequence
  | GridFault           -- ^ Fault detected, inverters disabled
  deriving (Show, Eq, Ord, Generic, NFData, ToJSON, FromJSON)

-- | Grid electrical state from measurements
data GridState = GridState
  { gsFrequency     :: Double        -- ^ Measured frequency (Hz)
  , gsVoltage       :: Double        -- ^ Measured voltage magnitude (V)
  , gsPhaseAngle    :: Double        -- ^ Phase angle relative to reference (rad)
  , gsPhaseSequence :: [Int]         -- ^ Detected phase sequence
  , gsRoCoF         :: Double        -- ^ Rate of change of frequency (Hz/s)
  , gsTimestamp     :: UTCTime       -- ^ Measurement timestamp
  , gsStatus        :: GridStatus    -- ^ Current grid status
  } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)

-- | Runtime state for the dispatcher
data DispatchState = DispatchState
  { dsServer           :: InferenceServer
  , dsConfig           :: DispatchConfig
  , dsDispatchCount    :: TVar Int
  , dsErrorAccum       :: TVar Double
  , dsGridState        :: TVar GridState
  , dsReconnectStart   :: TVar (Maybe UTCTime)
  , dsLastDispatch     :: TVar (Maybe DispatchResult)
  }

-- | Initialize dispatcher state
initDispatchState :: DispatchConfig -> IO DispatchState
initDispatchState cfg@DispatchConfig{..} = do
  let netConfig = DispatchNet dcModelPath dcNumNodes dcNumPVNodes dcMCSamples
  server <- startInferenceServer netConfig

  dispatchCount <- newTVarIO 0
  errorAccum <- newTVarIO 0
  now <- getCurrentTime
  gridState <- newTVarIO GridState
    { gsFrequency = 50.0
    , gsVoltage = 220.0
    , gsPhaseAngle = 0
    , gsPhaseSequence = [1,2,3]
    , gsRoCoF = 0
    , gsTimestamp = now
    , gsStatus = GridConnected
    }
  reconnectStart <- newTVarIO Nothing
  lastDispatch <- newTVarIO Nothing

  return DispatchState
    { dsServer = server
    , dsConfig = cfg
    , dsDispatchCount = dispatchCount
    , dsErrorAccum = errorAccum
    , dsGridState = gridState
    , dsReconnectStart = reconnectStart
    , dsLastDispatch = lastDispatch
    }

-- | Kibbutz (grid segment) state for dispatch
data KibbutzState = KibbutzState
  { ksNodes       :: [NodeState]
  , ksEdges       :: [(Int, Int)]
  , ksEdgeParams  :: Map (Int, Int) EdgeParams
  , ksTimestamp   :: Double
  , ksGridState   :: GridState         -- ^ Current grid measurements
  } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)

-- | Command for a single inverter
data InverterCmd = InverterCmd
  { icNodeId     :: Int
  , icPSetpoint  :: Double     -- ^ Real power setpoint (kW)
  , icQSetpoint  :: Double     -- ^ Reactive power setpoint (kVAR)
  , icEnabled    :: Bool       -- ^ Inverter enable/disable
  } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)

-- | Dispatch command to send to inverters
data DispatchCmd = DispatchCmd
  { cmdInverters    :: [InverterCmd]
  , cmdTimestamp    :: Double
  , cmdConfidence   :: Double
  , cmdUncertainty  :: Double
  , cmdGridStatus   :: GridStatus
  , cmdFallbackUsed :: Bool      -- ^ True if fell back to IPOPT
  } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)

-- | Check grid synchronization status
checkGridSync :: GridSyncConfig -> GridState -> (Bool, [String])
checkGridSync GridSyncConfig{..} GridState{..} =
  let checks =
        [ (gsFrequency >= gscFreqMin && gsFrequency <= gscFreqMax,
           printf "Frequency %.2f Hz out of range [%.1f, %.1f]" gsFrequency gscFreqMin gscFreqMax)
        , (gsVoltage >= gscVoltageMin && gsVoltage <= gscVoltageMax,
           printf "Voltage %.1f V out of range [%.0f, %.0f]" gsVoltage gscVoltageMin gscVoltageMax)
        , (abs gsRoCoF <= gscRoCoFLimit,
           printf "RoCoF %.3f Hz/s exceeds limit %.1f" gsRoCoF gscRoCoFLimit)
        , (gsPhaseSequence == gscPhaseSeq,
           printf "Phase sequence %s incorrect, expected %s" (show gsPhaseSequence) (show gscPhaseSeq))
        ]
      failures = [msg | (ok, msg) <- checks, not ok]
      allOk = null failures
  in (allOk, failures)

-- | Detect islanding condition
--
-- Uses multiple indicators:
--   * Frequency deviation > ±0.5 Hz
--   * RoCoF > 1 Hz/s
--   * Voltage deviation > ±15%
detectIslanding :: GridSyncConfig -> GridState -> Bool
detectIslanding GridSyncConfig{..} GridState{..} =
  let freqDev = abs (gsFrequency - gscFreqNominal)
      voltDev = abs (gsVoltage - gscVoltageNominal) / gscVoltageNominal
      roCoFExceeded = abs gsRoCoF > gscRoCoFLimit

      -- Island detection: any two of three indicators
      indicators = [freqDev > 0.5, voltDev > 0.15, roCoFExceeded]
      numTriggered = length $ filter id indicators
  in numTriggered >= 2

-- | Reconnection sequence state machine
reconnectionSequence :: GridSyncConfig
                     -> GridState
                     -> Maybe UTCTime    -- ^ Reconnect sequence start time
                     -> UTCTime          -- ^ Current time
                     -> (GridStatus, Double)  -- ^ (New status, power ramp factor 0-1)
reconnectionSequence GridSyncConfig{..} gs startTime now =
  case startTime of
    Nothing -> (GridIslanded, 0)  -- Not yet started
    Just start ->
      let elapsed = realToFrac $ diffUTCTime now start
          (syncOk, _) = checkGridSync defaultGridSyncConfig gs
      in if not syncOk
         then (GridReconnecting, 0)  -- Wait for stable grid
         else if elapsed < gscReconnectDelay
         then (GridReconnecting, 0)  -- In delay period
         else
           -- Ramp up power
           let rampTime = gscReconnectDelay + 10  -- 10 second ramp
               rampFactor = min 1.0 $ (elapsed - gscReconnectDelay) / 10
           in if rampFactor >= 1.0
              then (GridConnected, 1.0)
              else (GridReconnecting, rampFactor)

-- | Run the streaming dispatcher
runDispatcher :: (IsStream t, MonadAsync m)
              => DispatchConfig
              -> DispatchState
              -> t m KibbutzState
              -> t m DispatchCmd
runDispatcher cfg state = S.mapM (dispatchOne cfg state)

-- | Dispatch a single state update
dispatchOne :: DispatchConfig -> DispatchState -> KibbutzState -> IO DispatchCmd
dispatchOne cfg@DispatchConfig{..} DispatchState{..} state = do
  now <- getCurrentTime

  -- Update grid state
  atomically $ writeTVar dsGridState (ksGridState state)

  -- Check grid sync and islanding
  let (syncOk, syncErrors) = checkGridSync dcGridConfig (ksGridState state)
      isIslanded = detectIslanding dcGridConfig (ksGridState state)

  -- Handle grid status transitions
  currentStatus <- atomically $ gsStatus <$> readTVar dsGridState
  reconnectStart <- atomically $ readTVar dsReconnectStart

  (newStatus, rampFactor) <- case currentStatus of
    GridConnected ->
      if isIslanded then do
        putStrLn "ISLANDING DETECTED - disabling inverters"
        atomically $ writeTVar dsReconnectStart Nothing
        return (GridIslanded, 0)
      else if not syncOk then do
        mapM_ putStrLn syncErrors
        return (GridFault, 0)
      else
        return (GridConnected, 1.0)

    GridIslanded ->
      if not isIslanded && syncOk then do
        putStrLn "Grid stable - starting reconnection sequence"
        atomically $ writeTVar dsReconnectStart (Just now)
        return (GridReconnecting, 0)
      else
        return (GridIslanded, 0)

    GridReconnecting -> do
      let (status, ramp) = reconnectionSequence dcGridConfig (ksGridState state) reconnectStart now
      when (status == GridConnected) $
        putStrLn "Reconnection complete - normal operation resumed"
      return (status, ramp)

    GridFault ->
      if syncOk && not isIslanded then
        return (GridConnected, 1.0)
      else
        return (GridFault, 0)

  -- Update grid status
  atomically $ modifyTVar' dsGridState $ \gs -> gs { gsStatus = newStatus }

  -- If not connected, return disabled command
  if newStatus /= GridConnected && newStatus /= GridReconnecting
    then return DispatchCmd
      { cmdInverters = [InverterCmd i 0 0 False | i <- pvNodeIds]
      , cmdTimestamp = ksTimestamp state
      , cmdConfidence = 0
      , cmdUncertainty = 0
      , cmdGridStatus = newStatus
      , cmdFallbackUsed = False
      }
    else do
      -- Extract inputs for neural network
      let hour = ksTimestamp state / 3600  -- Convert to hours
          pMax = [nodePMax ns | ns <- ksNodes state, nodeType ns == PVBus]
          pLoad = map nodePLoad (ksNodes state)
          qLoad = map nodeQLoad (ksNodes state)

      -- Run neural network with uncertainty
      result <- dispatchWithUncertainty dsServer hour pMax pLoad qLoad True

      -- Check if uncertainty is too high - fall back to IPOPT
      (finalResult, usedFallback) <-
        if drUncertainty result > dcUncertaintyThreshold then do
          putStrLn $ printf "Uncertainty %.2f > %.2f, falling back to IPOPT"
                           (drUncertainty result) dcUncertaintyThreshold
          let prob = stateToACProblem cfg state
          optSol <- solveACOPF prob
          case optSol of
            Just sol -> return (dispatchToResult (solDispatch sol), True)
            Nothing -> return (result, True)  -- Keep neural result if IPOPT fails
        else
          return (result, False)

      -- Apply ramp factor for reconnection
      let rampedP = Map.map (* rampFactor) (drPSetpoints finalResult)
          rampedQ = Map.map (* rampFactor) (drQSetpoints finalResult)

      -- Increment dispatch count
      count <- atomically $ do
        c <- readTVar dsDispatchCount
        writeTVar dsDispatchCount (c + 1)
        writeTVar dsLastDispatch (Just finalResult)
        return c

      -- Async validation
      when (count `mod` dcValidateEvery == 0) $ void $ async $ do
        let prob = stateToACProblem cfg state
        vr <- validateDispatch prob finalResult
        unless (vrAcceptable vr) $
          putStrLn $ printf "Validation failed at dispatch %d: P_err=%.3f Q_err=%.3f"
                           count (vrPError vr) (vrQError vr)

      -- Build inverter commands
      let inverterCmds = zipWith3 mkInverterCmd pvNodeIds
            [Map.findWithDefault 0 (show i) rampedP | i <- [0..]]
            [Map.findWithDefault 0 (show i) rampedQ | i <- [0..]]

      return DispatchCmd
        { cmdInverters = inverterCmds
        , cmdTimestamp = ksTimestamp state
        , cmdConfidence = drConfidence finalResult
        , cmdUncertainty = drUncertainty finalResult
        , cmdGridStatus = newStatus
        , cmdFallbackUsed = usedFallback
        }
  where
    pvNodeIds = [i | i <- [1..dcNumNodes-1], i `mod` 3 /= 2]

    mkInverterCmd nodeId p q = InverterCmd
      { icNodeId = nodeId
      , icPSetpoint = p
      , icQSetpoint = q
      , icEnabled = True
      }

    dispatchToResult OptimalDispatch{..} = DispatchResult
      { drPSetpoints = Map.mapKeys show $ Map.mapKeys (\(NodeId i) -> i) odPSetpoints
      , drQSetpoints = Map.mapKeys show $ Map.mapKeys (\(NodeId i) -> i) odQSetpoints
      , drUncertainty = 0
      , drConfidence = 1.0
      , drKirchhoffSatisfied = True  -- IPOPT solution satisfies power balance
      }

-- | Convert KibbutzState to ACProblem
stateToACProblem :: DispatchConfig -> KibbutzState -> ACProblem
stateToACProblem DispatchConfig{..} KibbutzState{..} = ACProblem
  { acNodes = [NodeId i | i <- [0..length ksNodes - 1]]
  , acSlackNode = NodeId 0
  , acNodeState = Map.fromList [(NodeId i, s) | (i, s) <- zip [0..] ksNodes]
  , acEdges = [(NodeId i, NodeId j) | (i, j) <- ksEdges]
  , acEdgeParams = Map.mapKeys (\(i, j) -> (NodeId i, NodeId j)) ksEdgeParams
  , acVMin = gscVoltageMin dcGridConfig
  , acVMax = gscVoltageMax dcGridConfig
  , acFreqNominal = gscFreqNominal dcGridConfig
  , acFreqMin = gscFreqMin dcGridConfig
  , acFreqMax = gscFreqMax dcGridConfig
  , acCurtailPenalty = 10.0
  }

#else

-- GHCJS stub
import GHC.Generics
import Data.Aeson
import Control.DeepSeq (NFData)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Time.Clock (UTCTime)

import Chopaan.AC.Solver (NodeState(..), EdgeParams(..), NodeId(..), ACProblem(..), NodeType(..))
import Chopaan.AC.NeuralDispatch (DispatchNet(..), DispatchResult(..), InferenceServer)

data GridSyncConfig = GridSyncConfig
  { gscFreqNominal :: Double, gscFreqMin :: Double, gscFreqMax :: Double
  , gscVoltageNominal :: Double, gscVoltageMin :: Double, gscVoltageMax :: Double
  , gscRoCoFLimit :: Double, gscPhaseSeq :: [Int]
  , gscReconnectDelay :: Double, gscReconnectRamp :: Double
  } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)

defaultGridSyncConfig :: GridSyncConfig
defaultGridSyncConfig = GridSyncConfig 50 49.5 50.5 220 160 270 1.0 [1,2,3] 60 1

data DispatchConfig = DispatchConfig
  { dcModelPath :: FilePath, dcNumNodes :: Int, dcNumPVNodes :: Int
  , dcMCSamples :: Int, dcValidateEvery :: Int
  , dcErrorThreshold :: Double, dcUncertaintyThreshold :: Double
  , dcGridConfig :: GridSyncConfig
  } deriving (Show, Eq, Generic, NFData)

defaultDispatchConfig :: FilePath -> Int -> Int -> DispatchConfig
defaultDispatchConfig mp n npv = DispatchConfig mp n npv 20 100 1.0 2.0 defaultGridSyncConfig

data GridStatus = GridConnected | GridIslanded | GridReconnecting | GridFault
  deriving (Show, Eq, Ord, Generic, NFData, ToJSON, FromJSON)

data GridState = GridState
  { gsFrequency :: Double, gsVoltage :: Double, gsPhaseAngle :: Double
  , gsPhaseSequence :: [Int], gsRoCoF :: Double, gsTimestamp :: UTCTime
  , gsStatus :: GridStatus
  } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)

data DispatchState = DispatchState

data KibbutzState = KibbutzState
  { ksNodes :: [NodeState], ksEdges :: [(Int, Int)]
  , ksEdgeParams :: Map (Int, Int) EdgeParams
  , ksTimestamp :: Double, ksGridState :: GridState
  } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)

data InverterCmd = InverterCmd
  { icNodeId :: Int, icPSetpoint :: Double, icQSetpoint :: Double, icEnabled :: Bool
  } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)

data DispatchCmd = DispatchCmd
  { cmdInverters :: [InverterCmd], cmdTimestamp :: Double
  , cmdConfidence :: Double, cmdUncertainty :: Double
  , cmdGridStatus :: GridStatus, cmdFallbackUsed :: Bool
  } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)

initDispatchState :: DispatchConfig -> IO DispatchState
initDispatchState _ = error "Not available in GHCJS"
checkGridSync :: GridSyncConfig -> GridState -> (Bool, [String])
checkGridSync _ _ = (False, [])
detectIslanding :: GridSyncConfig -> GridState -> Bool
detectIslanding _ _ = False
reconnectionSequence :: GridSyncConfig -> GridState -> Maybe UTCTime -> UTCTime -> (GridStatus, Double)
reconnectionSequence _ _ _ _ = (GridFault, 0)
dispatchOne :: DispatchConfig -> DispatchState -> KibbutzState -> IO DispatchCmd
dispatchOne _ _ _ = error "Not available in GHCJS"

#endif
