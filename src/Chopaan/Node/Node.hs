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


import Proto.NodeMessageSchema.NodeMessages

import Streamly
import qualified Streamly.Prelude as S


import Chopaan.Node.Folds
import Chopaan.Node.Metrics

{--------------------------------------------------------------------------------------------------------------

                                          Streams of Folds
---------------------------------------------------------------------------------------------------------------}


powerS :: (MonadAsync m, IsStream t) => t m EnergyState -> t m PowerN
powerS = S.postscan . unAppF $ powerFold

energyS :: (MonadAsync m, IsStream t) => t m EnergyState -> t m EnergyN
energyS = S.postscan . unAppF $ energyFold

nodeS :: (MonadAsync m, IsStream t) => t m EnergyState -> t m SensorS
nodeS = S.postscan sensorFold

timeS :: (MonadAsync m, IsStream t) => t m EnergyState -> t m Timestamp
timeS = S.postscan timeFold
