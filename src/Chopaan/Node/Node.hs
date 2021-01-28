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
