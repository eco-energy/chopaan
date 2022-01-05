{-# LANGUAGE ExistentialQuantification
, QuantifiedConstraints
, FlexibleContexts
, FlexibleInstances
, FunctionalDependencies
, TypeFamilies
#-}

module Kbtz where

import ConCat.Misc
import Chopaan.Types
import qualified Data.Map.Strict as M
import Chopaan.Comm.Comm (
  Address(..)
  , Dispatch
  , MessageQs
  , WriteChan
  , PubQueue
  , initMessageQs
  , writeToPubQ
  )
import Control.Monad.Trans.State.Strict
import qualified Streamly.Internal.Data.Fold as FL
import Chopaan.Kibbutz.FS
import Chopaan.Node.NodeId
import Chopaan.Kibbutz.KbtzId
import qualified Chopaan.Node.NodeSensors
import Chopaan.Node.HW ( HW )
import qualified Chopaan.Graph.Algebraic as AG

import qualified Codec.Winery as W
import qualified Streamly.Prelude as S

import RIO



data SaveError

class (S.MonadAsync m, Address n, HasPath n) => KbtzM m n | m -> n where
  type RTState m
  type PersistState n
  kbtzim :: m (M.Map n KbtzName)
  kbtzState :: KbtzName -> m (AG.Graph (HW R) (n, NodeModel))
  subscribeK :: forall t a. (S.IsStream t, Dispatch a)
    => RTState m -> n -> t m a
  publishK :: forall t a. (S.IsStream t, Dispatch a)
    => RTState m -> t m (n, a) -> m ()
  writeK :: forall a. W.Serialise a
    => FL.Fold (StateT (PersistState n) m) (n, a) (Either SaveError ())
  readK :: forall t a. (S.IsStream t, W.Serialise a) => n -> t m a


-- instance KbtzM (ReaderT App IO) NodeMAC where
--   kbtzNodes = liftIO . getNodes
--   kbtzQueues = liftIO initMessageQs
--   subscribeD = sub
--   publishD p s = S.mapM_ (\(n, a) -> liftIO $ writeToPubQ p (controlTopic n) a) $ adapt s
--   writeF = undefined
--   readF = undefined
