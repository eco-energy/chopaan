{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module NodeSpec (spec) where

import Node
import Test.Hspec
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Test.QuickCheck
import Test.QuickCheck.Instances.Time ()

import qualified Streamly.Prelude as S
import Streamly


import Proto.NodeMessages ()
import Proto.NodeMessages_Fields
import Lens.Micro ()
import Data.ProtoLens.Arbitrary

import Data.ProtoLens (defMessage)
import Lens.Micro
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Time as Time
import Subscriber (subStream, runSubscriber, Subscriber, StreamMap, getStream, writeSub, mkSub, subMap)

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (isEmptyTChan, dupTChan)
import Control.Monad (forever, liftM)

instance Arbitrary EnergyState where
  arbitrary = arbitraryMessage

instance (Arbitrary a) => Arbitrary (Power a) where
  arbitrary = Power <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary 

instance (Arbitrary a) => Arbitrary (Energy a) where
  arbitrary = Energy <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


instance (Arbitrary a, Arbitrary b) => Arbitrary (NodeMetrics a b) where
  arbitrary = NodeMetrics <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a) => EqProp (Power a) where
  a =-= b = eq a b

instance (Eq a) => EqProp (Energy a) where
  a =-= b = eq a b


spec :: Spec
spec = do
  describe "This is how we use node streams" $ do
    let
        initTime = 1581444138
        m :: Int -> EnergyState
        m t = defMessage
                & batteryVoltage .~ 12
                & gridVoltage .~ 60
                & batteryToLoadCurrent .~ 5
                & batteryToGridCurrent .~ 5
                & gridToBatteryCurrent .~ 0
                & solarInputCurrent .~ 10
                & dutyCycle .~ 0
                & cpuTime .~ (fromIntegral $ (1581444138 + t))
        msgStream :: (IsStream t, Monad m) => t m (EnergyState)
        msgStream = S.map m $ S.enumerateFromTo 0 101
        tUTC = posixSecondsToUTCTime initTime
    it "power is a monoid and an applicative" $ do
      verboseBatch (monoid (undefined :: (Power Int)))
      verboseBatch (applicative (undefined :: Power (Int, Int, Int)))
    it "energy is a monoid and an applicative" $ do
      verboseBatch (monoid (undefined :: (Energy Int)))
      verboseBatch (applicative (undefined :: Energy (Int, Int, Int)))
    it "a stream at a 1 sec interval with a fixed power has an energy after n steps equivalent to the sum of the powers" $ do
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
        a = runNodeMonitor (posixSecondsToUTCTime initTime) msgStream
      pExp <- S.all (\a'-> a' == expectedP) (powerStream msgStream)
      eExp <- S.last $ energyStream (powerStream msgStream) ((timeDiff $ tUTC) . timeStream $ msgStream)
      nmExp <- S.last $ a
      lenExp <- S.length a
      pExp  `shouldBe` True
      eExp `shouldBe` (Just expectedE)
      nmExp `shouldBe` (Just expectedNM)
      lenExp `shouldBe` (102)
    it "mapping a gridStream over a KM should be a nice ting" $ do
      let
        ns :: [NodeId Int]
        ns = NodeId <$> [1..10 :: Int]
        msgs :: (SerialT IO (NodeId Int, EnergyState))
        msgs = S.zipWith (,) (S.fromList $ cycle ns) msgStream
      sub <- mkSub
      _ <- forkIO $ S.drain (serially $ writeSub sub msgs)
      print ("Writing")
      smap <- ((subMap ns sub) :: IO (StreamMap SerialT IO (NodeId Int) EnergyState))
      print ("Smap")
      let
        gs :: Serial (NodeId Int, NodeS)
        gs = gridStream tUTC ns smap
      msgl <- S.length msgs
      print ("Length of Message")
      S.mapM_ print gs
      gsl <- S.length $ S.takeWhile (const True) gs
      print ("Length of gsl")
      gsl `shouldBe` msgl
