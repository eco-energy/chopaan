{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses, GADTs, FlexibleInstances #-}
module Chopaan.Kibbutz.Kibbutzim where

import GHC.Generics
import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)

import Chopaan.Types
import Kbtz

import Control.Monad.IO.Class
import qualified Control.Concurrent.Async as A

import Streamly
import qualified Streamly.Prelude as S

import Chopaan.Kibbutz.KbtzId
import Chopaan.Kibbutz.Kibbutz

import Chopaan.Kibbutz.AWS.Things (withMqttAuth)
import Chopaan.Kibbutz.AWS.Common (newLogger, LogLevel(..))
import Chopaan.Kibbutz.Transactor (runTransactor, TransactionStatus, TxPlan)

import Chopaan.Node.NodeId (NodeMAC)
import Chopaan.Node.Folds (SensorS)
import Chopaan.Node.Node (nodeS)
import Proto.NodeMessageSchema.NodeMessages (RuntimeStats, HardwareConfig, EnergyState)
import Chopaan.Comm.Mqtt (runMqtt)
import Chopaan.Comm.Comm (MessageQs(..)
                         , mkCallback
                         , WriteChan
                         )
import System.IO (stdout)

{--
newtype Kibbutzim t m n = Kibbutzim {
  unKibbutzim :: Map KbtzName (KbtzState t m n)
  }

data KbtzState t m n = KbtzState
  { kSensors :: t m (n, SensorS)
  , kRuntime :: t m (n, RuntimeStats)
  , kTx :: t m (TxPlan n, t m TransactionStatus)
  }
--}


sensorKbtz :: forall t m. (IsStream t, MonadAsync m)
  => [NodeMAC]
  -> WriteChan NodeMAC EnergyState
  -> m (Kbtz t m NodeMAC SensorS)
sensorKbtz ns q = kbtz ns (sub @t @m @NodeMAC @EnergyState q) nodeS

rsKbtz :: forall t m. (IsStream t, MonadAsync m)
  => [NodeMAC]
  -> WriteChan NodeMAC RuntimeStats
  -> m (Kbtz t m NodeMAC RuntimeStats)
rsKbtz ns q = kbtz ns (sub @t @m @NodeMAC @RuntimeStats q) id


runKibbutz :: forall m. (KbtzM m NodeMAC) => MQTTOpts -> KbtzName -> m ()
runKibbutz mqttOpts name = do
  ns <- kbtzNodes name
  lg <- liftIO $ newLogger Debug stdout
  qs' <-  (liftIO $ A.async (liftIO $ mqtt lg ns))
  MessageQs{stateChan, statsChan, outbox} <- liftIO $ A.wait qs'
  sensors <- sensorKbtz @ParallelT ns stateChan
  runtime <- rsKbtz @ParallelT ns statsChan
  let txStatus = runTransactor outbox (10 * 60) sensors
  return ()
  where
    mqtt lg nodes = liftIO $
       withMqttAuth lg name
         (runMqtt mqttOpts nodes mkCallback)
