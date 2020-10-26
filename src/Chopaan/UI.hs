{-# LANGUAGE ExplicitForAll, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RankNTypes, FlexibleContexts#-}
module Chopaan.UI (mon) where

import Chopaan.Kibbutz.Kibbutz (Kbtz, asFRPNetwork)
import Chopaan.Kibbutz.Transactor (Tx(..), TransactionStatus(..))
import Chopaan.Comm.Comm (Address, Dispatch)

import Chopaan.Utils.StreamsInterop (toDynamic)
import Chopaan.UI.Base
import Chopaan.UI.Monitor
import Chopaan.Node.Node (NodeS)
import Proto.NodeMessageSchema.NodeMessages (RuntimeStats)

import Control.Monad.IO.Class (MonadIO)

import Reflex
import Reflex.Vty

import Streamly


mon :: forall t' t m m' n a. (IsStream t, MonadAsync m, MonadIO m', TriggerEvent t' m', UIConstraints t' m', Address n, Ord n, Show n, Show a)
  => (forall x. m x -> IO x)
  -> Kbtz t m n NodeS
  -> Kbtz t m n RuntimeStats
  -> Kbtz t m n a
  -> t m (Tx n)
  -> t m (TransactionStatus)
  -> VtyWidget t' m' (Event t' ()) --VtyWidget t' m (Event t' ())
mon h sensors runtime logs txs txStatuses = do
  s <- asFRPNetwork h sensors
  r <- asFRPNetwork h runtime
  l <- asFRPNetwork h logs
  tx <- toDynamic h txs
  txStatus <- toDynamic h txStatuses
  monitor s r l tx txStatus
