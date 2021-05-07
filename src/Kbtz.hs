{-# LANGUAGE ExistentialQuantification
, QuantifiedConstraints
, FlexibleContexts
, FlexibleInstances
, FunctionalDependencies
#-}

module Kbtz where

import Chopaan.Types
import Chopaan.Comm.Comm (
  Address(..)
  , Dispatch
  , MessageQs
  , WriteChan
  , PubQueue
  , initMessageQs
  , writeToPubQ)
import Chopaan.Kibbutz.Kibbutz (sub, getNodes)
import Chopaan.Node.NodeId
import Chopaan.Kibbutz.KbtzId

import Streamly
import qualified Streamly.Prelude as S

import RIO

class (MonadAsync m, Address n) => KbtzM m n | m -> n where
  kbtzNodes :: KbtzName -> m [n]
  kbtzQueues :: m (MessageQs n)
  subscribe :: forall t a. (IsStream t, Dispatch a)
    => WriteChan n a -> n -> m (t m a)
  publish :: forall t a. (IsStream t, Dispatch a)
    => PubQueue -> t m (n, a) -> m ()
  save :: forall a. (Dispatch a) => (n, a) -> m ()
  retrieve :: forall t a. (IsStream t, Dispatch a) => n -> t m a


instance KbtzM (ReaderT App IO) NodeMAC where
  kbtzNodes = liftIO . getNodes
  kbtzQueues = liftIO initMessageQs
  subscribe = sub
  publish p s = S.mapM_ (\(n, a) -> liftIO $ writeToPubQ p (controlTopic n) a) $ adapt s
  save = undefined
  retrieve = undefined
