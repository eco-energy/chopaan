{-# LANGUAGE OverloadedStrings #-}
module Main where

import Chopaan.Comm.S3
import Chopaan.Kibbutz.KbtzId
import Chopaan.Kibbutz
import Chopaan.Kibbutz.Kibbutz (getNodes)

import qualified Streamly.Prelude as S
import Streamly.Prelude (IsStream, MonadAsync, ParallelT, adapt)
import Data.Greskell


main :: IO ()
main = do
  ns <- getNodes thingType
  print ns
  let kc = mkKbtzConf thingType ns (Right bucketN) janusHost janusPort
  runKibbutz kc
  where
    thingType = KbtzId "kibbutz-pilot-node"
    janusHost = "localhost"
    janusPort = 8182
{-
addMeshFrame :: Spider n m EnergyState -> Spider NodeMAC MeshFrame -> 
addMeshFrame = undefined
o-}
