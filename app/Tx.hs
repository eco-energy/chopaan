{-# LANGUAGE OverloadedStrings, TypeApplications, NamedFieldPuns #-}
module Main where

import Dhall
import Chopaan.Node.NodeId
import Chopaan.Kibbutz.Transactor (Tx(..), TxPlan, Role(..), mkStake, dispatchTx, Stake(..))
import Chopaan.Comm.Mqtt (pub, client)
import Chopaan.Comm.Comm (initPubQ, trivialCB)
import Chopaan.Types
import qualified Data.Map.Strict as Map
import Control.Concurrent.STM (atomically)
import Control.Concurrent
import Control.Monad
import Proto.NodeMessageSchema.NodeMessages

srcs :: [NodeMAC]
srcs = NodeId <$> [ "7c:9e:bd:f6:5a:08"
                  , "7c:9e:bd:f5:c6:cc"
                  --, "7c:9e:bd:f5:07:c8"
                  --, "7c:9e:bd:f6:42:68"
                  ]


main = do
  Options{mqttOpts} <- input auto "./txOpts.dhall"
  outbox <- atomically $ initPubQ
  cl <- client mqttOpts trivialCB
  _ <- forkIO $ forever $ (pub cl outbox)
  mapM_ (\t -> (dispatchTx outbox t)
          >> print ("Dispatched! " <> show t)
          >> (threadDelay delay)) $ loop srcs


delay :: Int
delay = oneSec * (t + 10)

oneSec = 1000*1000 

loop :: [NodeMAC] -> [TxPlan NodeMAC]
loop addrs = fmap (txAtT addrs) $ stakeLL addrs
  
txAtT :: [NodeMAC] -> [Stake] -> TxPlan NodeMAC
txAtT addrs stakes = Tx $ Map.fromList $ zip addrs stakes

stakeLL :: [NodeMAC] -> [[Stake]]
stakeLL ns = fmap (\(i, s) -> case mod @Int i 2 of
                      0 -> s
                      _ -> switchStakePolarity <$> s
                  ) $ zip [1..] $ repeat $ stakeL ns

stakeL :: [NodeMAC] -> [Stake]
stakeL ns = [mkStake (getRole i) p t | (i, _) <- zip [1..] ns]
  where
    p = 60 :: Double
    getRole :: Int -> Role
    getRole i
      | mod i 2 == 0 = Source
      | otherwise = Sink

t = 60 * 1


switchStakePolarity :: Stake -> Stake
switchStakePolarity (Stake (Source, p, t)) = Stake (Sink, p, t)
switchStakePolarity (Stake (Sink, p, t)) = Stake (Source, p, t)


testMACs = (NodeId "7c:9e:bd:f5:07:c8")

testConvEff1 :: NodeMAC -> [TxPlan NodeMAC]
testConvEff1 f = (\p -> Tx $ Map.fromList [ (f, mkStake Source p t) ])
                                              --, (s, mkStake Sink p t)
                                              --]
                            <$> powers
  where
    powers = [10,20..120]
    t = 60
