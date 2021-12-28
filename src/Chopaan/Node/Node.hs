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
  SensorR 
--  nodeS, energyS, powerS, timeS
  -- folds
  , sensorFold, energyFold, powerFold, timeFold
  ) where


import Control.Monad.Bayes.Class
import Proto.NodeMessageSchema.NodeMessages

import Streamly.Prelude (MonadAsync, IsStream)
import qualified Streamly.Prelude as S


import Chopaan.Node.Folds
import Chopaan.Node.Metrics

{--------------------------------------------------------------------------------------------------------------

                                          Streams of Folds
---------------------------------------------------------------------------------------------------------------}


-- powerS :: (MonadSample m, MonadAsync m, IsStream t) => t m EnergyState -> t m PowerNR
-- powerS = S.postscan powerFold
-- {-# INLINE powerS #-}


-- energyS :: (MonadSample m, MonadAsync m, IsStream t) => t m EnergyState -> t m EnergyNR
-- energyS = S.postscan energyFold
-- {-# INLINE energyS #-}

-- nodeS :: (MonadSample m, MonadAsync m, IsStream t) => t m EnergyState -> t m SensorR
-- nodeS = S.postscan sensorFold
-- {-# INLINE nodeS #-}

-- timeS :: (MonadSample m, MonadAsync m, IsStream t) => t m EnergyState -> t m Timestamp
-- timeS = S.postscan timeFold
-- {-# INLINE timeS #-}
