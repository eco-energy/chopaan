{-# LANGUAGE ExplicitForAll, TypeApplications, ScopedTypeVariables #-}
module Chopaan.Comm.Queues where

import Data.Hashable

import Control.Concurrent.STM
import Control.Concurrent.Chan.Unagi


import Proto.NodeMessageSchema.NodeMessages hiding (Outgoing, Incoming)
import Proto.NodeMessageSchema.NodeMessages_Fields


newtype NodeQueue a b = NodeQueue { runNodeQueue :: TBQueue (a, b) }

initNodeQueue :: forall a b. STM (NodeQueue a b)
initNodeQueue = do
  n <- newTBQueue 1000
  return $ NodeQueue n

writeNodeQ :: NodeQueue a b -> a -> b -> STM ()
writeNodeQ p = curry (writeTBQueue (runNodeQueue p))

readNodeQ :: NodeQueue a b -> STM (a, b)
readNodeQ (NodeQueue q) = readTBQueue q

