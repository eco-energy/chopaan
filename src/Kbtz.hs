{-# LANGUAGE ExistentialQuantification, QuantifiedConstraints, FlexibleContexts, FlexibleInstances #-}

module Kbtz where

import Chopaan.Types
import Chopaan.Comm.Comm (Address, Dispatch, MessageQs, initMessageQs)

import Streamly
import RIO

class (MonadAsync m) => KbtzM m where
  getNodes :: forall n. (Address n) => m [n]
  getQueues :: forall n. (Address n) => m (MessageQs n)
  subscribe :: forall t n a. (IsStream t, Address n, Dispatch a)
    => MessageQs n -> n -> t m a
  publish :: forall t n a. (IsStream t, Address n, Dispatch a)
    => MessageQs n -> t m (n, a) -> m ()
  save :: forall n a. (Address n, Dispatch a) => (n, a) -> m ()
  retrieve :: forall t n a. (IsStream t, Address n, Dispatch a) => n -> t m a


instance KbtzM (ReaderT App IO) where
  getQueues = liftIO initMessageQs
