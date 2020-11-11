{-# LANGUAGE OverloadedStrings, TypeApplications, NamedFieldPuns #-}
module Main where

import Dhall
import Chopaan.Node.NodeId
import Chopaan.Kibbutz.Transactor (Tx(..), Role(..), mkStake, dispatchTx, Stake(..))
import Chopaan.Comm.Mqtt (pub, client)
import Chopaan.Comm.Comm (initNodeQueue, trivialCB)
import Chopaan.Types
import qualified Data.Map.Strict as Map
import Control.Concurrent.STM (atomically)
import Control.Concurrent
import Control.Monad
import Proto.NodeMessageSchema.NodeMessages

srcs :: [NodeMAC]
srcs = NodeId <$> [ "7c:9e:bd:f6:59:08"
                  , "7c:9e:bd:f5:ec:74"
                  , "7c:9e:bd:f5:07:c8"
                  , "7c:9e:bd:f6:42:68"
                  ]


main = do
  Options{mqttOpts} <- input auto "./options.dhall"
  outbox <- atomically $ initNodeQueue @NodeMAC @(MeshFrame)
  cl <- client mqttOpts trivialCB
  _ <- forkIO $ forever $ (pub cl outbox)
  mapM_ (\t -> (dispatchTx outbox t)
          >> print ("Dispatched! " <> show t)
          >> (threadDelay delay)) $ loop srcs


delay :: Int
delay = 1000*1000 * 60 * 1


loop :: [NodeMAC] -> [Tx NodeMAC]
loop addrs = fmap (txAtT addrs) stakeLL  
  
txAtT :: [NodeMAC] -> [Stake] -> Tx NodeMAC
txAtT addrs stakes = Tx $ Map.fromList $ zip addrs stakes

stakeLL :: [[Stake]]
stakeLL = fmap (\(i, s) -> case mod @Int i 2 of
                   0 -> s
                   _ -> switchStakePolarity <$> s
               ) $ zip [1..] $ repeat stakeL

stakeL :: [Stake]
stakeL = [mkStake Source p t, mkStake Sink p t, mkStake Source p t, mkStake Sink p t ]
  where
    p = 50 :: Double

t = 60 * 2


switchStakePolarity :: Stake -> Stake
switchStakePolarity (Stake (Source, p, t)) = Stake (Sink, p, t)
switchStakePolarity (Stake (Sink, p, t)) = Stake (Source, p, t)
