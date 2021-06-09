{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts, TypeApplications #-}

module NodeSpec (spec) where

import Chopaan.Node.Metrics
import Test.Hspec
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Test.QuickCheck
import Test.QuickCheck.Instances.Time ()
import Test.QuickCheck.Arbitrary.Generic

import qualified Streamly.Prelude as S
import Streamly
import qualified Streamly.Data.Fold as FL

import Proto.NodeMessageSchema.NodeMessages ()
import Proto.NodeMessageSchema.NodeMessages_Fields
import Lens.Micro ()
--import Data.ProtoLens.Arbitrary

import Data.Aeson as A
import Data.Text.Encoding.Base64
import Data.ProtoLens (defMessage)
import Lens.Micro
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Time as Time

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (isEmptyTChan, dupTChan)
import Control.Monad (forever, liftM)
import Chopaan.Graph.Greskell

import Numeric.Compensated

--instance Arbitrary EnergyState where
--  arbitrary = arbitraryMessage

-- instance (Arbitrary a) => Arbitrary (Node a) where
--   arbitrary = Node <$> arbitrary <*> arbitrary <*> arbitrary 


instance (Eq a) => EqProp (Node a) where
  a =-= b = eq a b

instance (Arbitrary v) => Arbitrary (Node v) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance (Arbitrary e, Arbitrary p) => Arbitrary (Battery e p) where
  arbitrary = genericArbitrary
  shrink = genericShrink

-- instance (Arbitrary e, Arbitrary p) => Arbitrary (SensorMetrics e p) where
--   arbitrary = genericArbitrary
--   shrink = genericShrink


instance (Arbitrary e, Arbitrary p) => Arbitrary (SensorMetrics e p) where
  arbitrary = genericArbitrary
  shrink = genericShrink

spec :: Spec
spec = do
  describe "This is how we use node streams" $ do
    it "Node is applicative" $ do
      verboseBatch (applicative (undefined :: Node (Double, Double, Double)))
    it "Node is monoidal" $ do
      verboseBatch (monoid (undefined :: (Node Int)))
    it "Sensor Metrics can round-trip json" $ do
      sms <- arbs @(SensorMetrics Double Double) 10
       -- let p = fromJSON . toJSON
        --    xs = p <$> sms
      -- print x
      -- print y
      (decode . encode . head $ sms) `shouldBe` (Just . head $ sms)
{--    it "a stream at a 1 sec interval with a fixed power has an energy after n steps equivalent to the sum of the powers" $ do
      let
        len = 102 :: Int
        bv = 12
        gv = 60
        b2l = 5
        b2g = 5
        g2b = 0
        ic = 10
        initTime = 1581444138
        m :: Int -> EnergyState
        m t = defMessage
                & batteryVoltage .~ bv
                & gridVoltage .~ gv
                & batteryToLoadCurrent .~ b2l
                & batteryToGridCurrent .~ b2g
                & gridToBatteryCurrent .~ g2b
                & solarInputCurrent .~ ic
                & dutyCycle .~ 0
                & cpuTime .~ (fromIntegral $ (1581444138000 + (t*1000)))
        msgStream :: (IsStream t, Monad m) => t m (EnergyState)
        msgStream = S.map m $ S.enumerateFromTo 0 (len - 1)
        tUTC = posixSecondsToUTCTime initTime
        expectedP = Power {genP=(toWatts $ bv * ic), tInP=(toWatts $ bv * g2b), tOutP=(toWatts $ bv * b2g), loadP=(toWatts $ bv * b2l)} 
        expectedE = Energy {txIn=toE tInP, txOut=toE tOutP, consumed=toE loadP, generated=toE genP}
          where
            toE = toWattSeconds . uncompensated
            Power{..}= sP
            sP = foldl (<>) expectedP $ replicate (len - 2) expectedP
      (Just expectedS) <- S.last msgStream
      let
        expectedNM = (NodeMetrics lastConn expectedP expectedE expectedS) 
          where
            lastConn = (Just $ posixSecondsToUTCTime (initTime + (fromIntegral $ len - 1)))
        a = nodeS msgStream
      pExp <- S.all (\a'-> a' == expectedP) (powerS msgStream)
      eExp <- S.last $ S.trace (print) $ energyS msgStream
      nmExp <- S.last $ a
      lenExp <- S.length a
      pExp  `shouldBe` True
      eExp `shouldBe` (Just expectedE)
      nmExp `shouldBe` (Just expectedNM)
      lenExp `shouldBe` (len)
--}
    {--
    it "mapping a gridStream over a KM should be a nice ting" $ do
      let
        ns :: [NodeId Double]
        ns = NodeId <$> [1..10 :: Double]
        msgStream = S.map m $ S.enumerateFromTo 0 100
        msgs :: (SerialT IO (NodeId Double, EnergyState))
        msgs = (,) <$> (S.fromList ns) <*> msgStream
      sub <- mkSub
      _ <- forkIO $ S.drain (parallely $ adapt $ writeSub sub msgs)
      smap <- ((subMap ns sub) :: IO (StreamMap (NodeId Double) EnergyState))
      km <- atomically $ initKM ns defNodeS
      let
        gs :: Serial (NodeId Double, NodeS)
        gs = gridStream tUTC ns smap
      S.drain $ S.mapM (\(n, s) -> atomically $ updateKM km n s) gs
      endStates <- atomically $ readKM km ns
      let
        expectedP = Power {gen=(12 * 10), tIn=(12 * 0), tOut=(12 * 5), load=(12 * 5)} 
        expectedE = Energy {txIn=tIn, txOut=tOut, consumed=load, generated=gen}
          where
            Power{..} = sP
            sP = foldl (<>) expectedP $ replicate 99 expectedP
      (Just expectedS) <- S.last msgStream
      let
        expectedNM = (NodeMetrics lastConn expectedP expectedE expectedS) 
          where
            lastConn = (Just $ posixSecondsToUTCTime (initTime + 101))
      mapM_ (\e -> (snd e) `shouldBe` expectedNM) endStates
--}
      --forkIO $ forever $ do
      --  gl <- S.length $ S.take 5 gs 
      --  print ("Length of take: " <> show gl)
      --printed <- S.length $ S.mapM print gs
      --print ("Length of printed" <> show printed)
      
