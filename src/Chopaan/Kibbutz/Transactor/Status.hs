{-# LANGUAGE NamedFieldPuns, OverloadedStrings, TupleSections #-}
{-# LANGUAGE DeriveFunctor, DeriveGeneric, DeriveAnyClass, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveFoldable, DeriveTraversable, DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitForAll, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE FlexibleContexts, RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances, CPP, BangPatterns #-}

module Chopaan.Kibbutz.Transactor.Status where

import Control.DeepSeq ( NFData )
import Data.Aeson (ToJSON, FromJSON)
import qualified Codec.Winery as W
import qualified Data.Time as Time
import GHC.Generics (Generic)
import Chopaan.Node.Metrics (toWattSeconds, toWatts
                            , fromWattSeconds, fromWatts
                            , Watts, WattSeconds
                            , SensorMetrics(..)
                            , Node(..)
                            , BatteryR
                            , PowerNR
                            , pToE
                            )

import Data.Greskell ( lookupAs, Key )
import Data.Greskell.Extra ( (<=:>), writeKeyValues )
import NetSpider.Found ( LinkState(..) )
import NetSpider.Graph
    ( EFinds, LinkAttributes(..), NodeAttributes(..), VFoundNode )
import Chopaan.Graph.Greskell ( decodeBin, wineryJSONWrite )

data TxStatus' e = TxStatus'
  { energyDispatched :: !e
  , energyReceived :: !e
  , energyRemaining :: !e
  , lossPerWattSecond :: !e
  , totalLoss :: !e
  , timeRemaining :: !Time.NominalDiffTime
  , startLag :: !Time.NominalDiffTime
  , endLag :: !Time.NominalDiffTime
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, NFData)
  deriving W.Serialise via (W.WineryRecord (TxStatus' e))

type TxStatus = TxStatus' WattSeconds

instance (Ord e, RealFrac e) => Semigroup (TxStatus' e) where
  tx <> tx' = TxStatus'
              { energyDispatched = energyDispatched tx + energyDispatched tx'
              , energyReceived = energyReceived tx + energyReceived tx'
              , timeRemaining = min (timeRemaining tx) (timeRemaining tx')
              , energyRemaining = min (energyRemaining tx) (energyRemaining tx')
              , lossPerWattSecond =  avg (lossPerWattSecond tx) (lossPerWattSecond tx')
              , totalLoss = totalLoss tx + totalLoss tx'
              , startLag = max (startLag tx) (startLag tx')
              , endLag = max (endLag tx) (endLag tx)
              }
              where
                avg a b = (a + b) / 2

instance  (Ord e, RealFrac e) => Monoid (TxStatus' e) where
  mempty = TxStatus'
    { energyDispatched = 0
    , energyReceived = 0
    , timeRemaining = 0
    , energyRemaining = 0
    , lossPerWattSecond = 0
    , totalLoss = 0
    , startLag = 0
    , endLag = 0
    }



txStatusKey :: forall n a. Key n a
txStatusKey = "txStatusKey"


txStatusLinkDir :: TxStatus -> LinkState
txStatusLinkDir TxStatus'{energyDispatched, energyReceived} = if energyDispatched > 0 && energyDispatched == 0
  then LinkToTarget
  else if energyReceived > 0 && energyDispatched == 0
       then LinkToSubject
       else LinkBidirectional
{-# INLINE txStatusLinkDir #-}

instance (W.Serialise n) => LinkAttributes (TxStatus' n) where
  writeLinkAttributes s = fmap writeKeyValues $
    sequence [ (txStatusKey @EFinds <=:> wineryJSONWrite s)
             ]
  parseLinkAttributes props = (decodeBin "TxStatus' Link" $ lookupAs txStatusKey props)

instance (W.Serialise n) => NodeAttributes (TxStatus' n) where
  writeNodeAttributes s = fmap writeKeyValues $
    sequence [ (txStatusKey @VFoundNode <=:> wineryJSONWrite s)
             ]
  parseNodeAttributes props = (decodeBin "TxStatus' Node" $ lookupAs txStatusKey props)


