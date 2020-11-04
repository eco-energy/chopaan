{-# LANGUAGE OverloadedStrings, TypeApplications, NamedFieldPuns #-}
module Main where

import Dhall
import Chopaan.Node.NodeId
import Chopaan.Kibbutz.Transactor (Tx(..), Role(..), mkStake, dispatchTx)
import Chopaan.Comm.Mqtt (pub, client)
import Chopaan.Comm.Comm (initNodeQueue, trivialCB)
import Chopaan.Types
import qualified Data.Map.Strict as Map
import Control.Concurrent.STM (atomically)
import Control.Concurrent
import Control.Monad
import Proto.NodeMessageSchema.NodeMessages

nodeId :: NodeMAC
nodeId = NodeId "3c:71:bf:64:45:20"



main = do
  Options{mqttOpts} <- input auto "./options.dhall"
  outbox <- atomically $ initNodeQueue @NodeMAC @(MeshFrame)
  cl <- client mqttOpts trivialCB 
  let txs = txTest 1 nodeId
  _ <- forkIO $ forever $ (pub cl outbox)
  mapM_ (\t -> dispatchTx outbox t >> (threadDelay $ 1000*1000*60*3) >> print ("Dispatched! ")) txs


txTest :: Int -> NodeMAC -> [Tx NodeMAC]
txTest n addr = [ testTx addr | _ <- (enumFrom @Int) 0 ]



testTx :: NodeMAC -> Tx NodeMAC
testTx addr = Tx $ Map.fromList [(addr, mkStake Source 30 $ 60*3)]
