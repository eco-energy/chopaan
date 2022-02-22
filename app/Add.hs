{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Bifunctor
import Path.IO
import Path
import Chopaan.Node.NodeId
import Chopaan.Node.Components
import Chopaan.Node.HW
import Chopaan.Kibbutz.KbtzId
import Chopaan.Kibbutz.FS
import Control.Monad.Trans.Reader

mkNode :: (NodeMAC, HHId, PPUId, HW Double, (Double, Double), HHId) -> NodeModel
mkNode (m, i, p, h, loc, to) = NodeModel i m h loc (Ownership i) p to 

naruda150 :: BatteryConf Double
naruda150 = BatteryConf 11.8 13.0 12.0 150 DryBattery

ses185 :: BatteryConf Double
ses185 = BatteryConf 13.6 14.4 12 185 LeadAcidSealed

ses230 :: BatteryConf Double
ses230 = ses185 { capacityAH = 230 }

ecostar120 :: BatteryConf Double
ecostar120 = ses185 { capacityAH = 120 }

mx320PV :: PVConf Double
mx320PV = PVConf 45.95 37.54 8.534 320

withB :: BatteryConf Double -> HW Double
withB b = HW (SingBC b) (SingPC mx320PV) load50   

load50 :: LoadTop Double
load50 = SingLC $ LoadConf 50

bmNs :: [NodeModel]
bmNs = fmap mkNode $
  [ (NodeId "7c:9e:bd:48:4e:e0", HHId 1, PPUId 1, withB naruda150, (0, 1), HHId 1)
  , (NodeId "ac:67:b2:11:f3:10", HHId 2, PPUId 5, withB ses185, (0, 0), HHId 1)
  , (NodeId "8c:aa:b5:97:69:48", HHId 3, PPUId 9, withB ses230, (1, 1), HHId 1)
  , (NodeId "7c:9e:bd:49:12:d8", HHId 4, PPUId 10, withB ses230, (3, 1), HHId 3)
  , (NodeId "ac:67:b2:1c:ec:d8", HHId 5, PPUId 11, withB ses230, (4, 2), HHId 4)
  , (NodeId "7c:9e:bd:48:a2:c4", HHId 6, PPUId 12, withB naruda150, (2, 4), HHId 5) 
  , (NodeId "ac:67:b2:11:f2:30", HHId 7, PPUId 8, withB ses185, (1, 4), HHId 6)
  , (NodeId "7c:9e:bd:49:1d:80", HHId 8, PPUId 13, withB ses185, (4, 6), HHId 6)
  , (NodeId "7c:9e:bd:47:b7:e8", HHId 9, PPUId 15, withB ses185, (3, 6), HHId 8) 
  , (NodeId "7c:9e:bd:48:d1:3c", HHId 10, PPUId 3, withB naruda150, (2, 6), HHId 9)
  , (NodeId "ac:67:b2:1d:e7:f4", HHId 11, PPUId 14, withB naruda150, (5, 15), HHId 8)
  , (NodeId "78:21:84:7d:d8:6c", HHId 12, PPUId 6, withB ses185, (5, 20), HHId 11)
  ]

labNs :: [NodeModel]
labNs = fmap mkNode $
  [ (NodeId "7c:9e:bd:47:45:a4", HHId 21, PPUId 21, withB naruda150, (0, 0), HHId 23)
  , (NodeId "7c:9e:bd:49:07:80", HHId 22, PPUId 22, withB ses230, (2, 1), HHId 23)
  , (NodeId "7c:9e:bd:48:39:a8", HHId 23, PPUId 23, withB ecostar120, (2, 3), HHId 22)
  , (NodeId "7c:9e:bd:47:bf:48", HHId 24, PPUId 24, withB ecostar120, (3, 1), HHId 22)
  , (NodeId "7c:9e:bd:48:76:64", HHId 25, PPUId 25, withB naruda150, (0, 0), HHId 27)
  , (NodeId "7c:9e:bd:46:f1:fc", HHId 26, PPUId 26, withB ses230, (2, 1), HHId 27)
  , (NodeId "7c:9e:bd:49:00:08", HHId 27, PPUId 27, withB ecostar120, (2, 3), HHId 25)
  , (NodeId "7c:9e:bd:47:aa:64", HHId 28, PPUId 28, withB ecostar120, (3, 1), HHId 25)
  , (NodeId "ac:67:b2:12:09:60", HHId 29, PPUId 29, withB ses230, (2, 1), HHId 30)
  , (NodeId "94:b9:7e:d0:77:fc", HHId 30, PPUId 30, withB ecostar120, (2, 3), HHId 29)
  ]

addBismillahMor :: IO ()
addBismillahMor = do
  dir <- makeAbsolute =<< parseRelDir "data/kbtzim"
  flip runReaderT dir $ do
    createKbtz (KbtzId "bismillahMor") (fromNodeModels bmNs)
    createKbtz (KbtzId "labGrid") (fromNodeModels labNs)
    
main = addBismillahMor
