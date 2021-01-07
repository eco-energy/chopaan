{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Chopaan.Node.Node (
  -- scans
  nodeS, SensorS, energyS, powerS, timeS
  -- folds
  , sensorFold, energyFold, powerFold, timeFold
  ) where



import qualified Data.Text as T
import Data.Int

import Proto.NodeMessageSchema.NodeMessages

import Streamly
import qualified Streamly.Prelude as S

import Control.Monad.State.Lazy

import Chopaan.Node.Folds
import Chopaan.Node.Metrics


import qualified System.Metrics.Gauge as G
import System.Metrics

{--------------------------------------------------------------------------------------------------------------

                                          Streams of Folds
---------------------------------------------------------------------------------------------------------------}


energyS :: (MonadAsync m, IsStream t) => t m EnergyState -> t m Energy
energyS = S.postscan energyFold

timeS :: (MonadAsync m, IsStream t) => t m EnergyState -> t m Timestamp
timeS = S.postscan timeFold

powerS :: (MonadAsync m, IsStream t) => t m EnergyState -> t m Power
powerS = S.postscan powerFold

nodeS :: (MonadAsync m, IsStream t) => t m EnergyState -> t m SensorS
nodeS = S.postscan sensorFold

{--
newtype Grid n s = Grid (Map.Map n s) deriving (Show, Generic, Functor)

type GridS n = Grid n NodeS

gridS :: forall t m n . (IsStream t, MonadAsync m, Ord n) => [n] -> t m (n, EnergyState) -> t m (GridS n)
gridS ns ss = S.postscan gridMap $ ss
  where
    gridMap = Grid <$> FL.demux nodeMap
      where
        nodeMap = Map.fromList $ zip ns $ repeat nodeMonitor 
--}

{----------------------------------------------------------

                EKG Gauge
----------------------------------------------------------}

data NodeGauge = NodeGauge
  { inG :: G.Gauge
  , outG :: G.Gauge
  , generatedG :: G.Gauge
  , consumedG :: G.Gauge
  }

registerNodeG :: (MonadIO m, Show n) => Store -> n -> m NodeGauge
registerNodeG store node = do
  i <- nGauge "input"
  o <- nGauge "output"
  g <- nGauge "generated"
  c <- nGauge "consumed"
  return $ NodeGauge i o g c
  where
    nGauge metric = liftIO $ createGauge (withName metric) store  
    withName :: T.Text -> T.Text
    withName metric = (T.pack . show $ node) <> "." <> (metric)

  
updateNodeG :: forall m. (MonadIO m) => NodeGauge -> SensorS -> m ()
updateNodeG NodeGauge{..} SensorMetrics{_powerT} = do
  setG inG  txIn
  setG outG txOut
  setG generatedG generated
  setG consumedG consumed
  where
    setG g v = liftIO $ G.set g (readVal v) 
    readVal :: (Power -> Watts) -> Int64
    readVal f = fromIntegral . floor . f $ _powerT
