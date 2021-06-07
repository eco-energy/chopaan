{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, StandaloneDeriving, TypeApplications, TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, OverloadedStrings, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SpiderSpec (spec) where

import Streamly.Prelude (IsStream, MonadAsync)
import qualified Streamly.Prelude as S
import Test.Hspec
import Test.QuickCheck.Checkers
import Test.QuickCheck
import Test.QuickCheck.Classes
import Data.Monoid (Sum(..))
import Control.Monad.IO.Class
import qualified Data.Text as Text
import Test.QuickCheck.Arbitrary.Generic
import Data.ProtoLens.Arbitrary
import Data.ProtoLens
import Data.Word

import Chopaan.Node.NodeId
import Chopaan.Kibbutz.KbtzId (KbtzId(..))
import Chopaan.Kibbutz
import Chopaan.Comm.Comm (initQs, writeChan, MessageQs(..), readPubQ)
import Chopaan.Utils.Time (timeToUIntSeconds)
import qualified Data.Text as T
import qualified Data.Time as Ti

import qualified Proto.NodeMessageSchema.NodeMessages as NM
import qualified Proto.NodeMessageSchema.NodeMessages_Fields as NM
import Lens.Micro
import Control.Concurrent hiding (writeChan)

instance Arbitrary (NodeMAC) where
  arbitrary = do
    let el = ['a'..'z'] <> ['0'..'9']
    xs <- mapM (\_ -> elements el) [1..6]
    ys <- mapM (\_ -> elements el) [1..6]
    let cpld = fmap (\(a, b) -> [a] <> [b]) $ zip xs ys
    return $ NodeId . T.pack . tail $ foldl (\x y -> x <> ":" <> y) "" cpld

instance Arbitrary (NM.EnergyState) where
  arbitrary = arbitraryMessage

instance Arbitrary (NM.RuntimeStats) where
  arbitrary = arbitraryMessage -- do
    -- mFH <- arbitrary @Word32
    -- cFH <- arbitrary @Word32
    -- cu <-  arbitrary @Word32
    -- r <- arbitrary @Bool
    -- w <- arbitrary @Int
    -- ps <- arbitrary @Int
    -- u <- arbitrary @Word64
    -- return $ defMessage
    --   & (NM.minFreeHeap .~ mFH)
    --   & (NM.currentFreeHeap .~ cFH)
    --   & (NM.cpuUtilization .~ cu)
    --   & (NM.isRoot .~ r)


    
spec :: Spec
spec = do
  describe "Spiders are great" $ do
    it "qKbtz processor processes all messages!" $ do
      let nNodes = 10
          nMessages = 100
      ns <- arbs @NodeMAC nNodes
      es <- do
        xs'' <- mapM (\_ -> orderedES nMessages) ns
        return $ foldl S.wSerial S.nil xs''
      rs <- do
        xs'' <- mapM (\_ -> orderedRS nMessages) ns
        return $ foldl S.wSerial S.nil xs''
      qs <- initQs
      k <- runKibbutz $
        KbtzC { name = KbtzId "test"
              , nodes = ns
              , channelOpts = Left qs
              , spiderHost = "localhost"
              , spiderPort = 8182
              }
      let ns' = S.fromList $ cycle ns

      forkIO $ do
        S.mapM_ (\(n, e) -> writeChan (stateChan qs) n e)  $ S.zipWith (,) ns' es
        S.mapM_ (\(n, r) -> writeChan (statsChan qs) n r)  $ S.zipWith (,) ns' rs

      l <- S.length $ S.take ((2 * nNodes * nMessages) + 1) $ k
      l `shouldBe` (2 * nNodes * nMessages)



orderedES :: Int -> IO (S.Serial NM.EnergyState)
orderedES n = do
  xs <- arbs n
  let xs' = map updateT $ zip xs tsL
  return $ S.fromList xs'
  where
    updateT (m, t') = m & NM.cpuTime .~ (timeToUIntSeconds t') 
    
orderedRS :: Int -> IO (S.Serial NM.RuntimeStats)
orderedRS n = do
  xs <- arbs n
  let xs' = map updateT $ zip xs tsL
  return $ S.fromList xs'
  where
    updateT (m, t') = m & NM.cpuTime .~ (timeToUIntSeconds t)
                       & NM.isRoot .~ True
                       & NM.version .~ "verion1"
    
tsL = iterate (Ti.addUTCTime d) t
t = Ti.UTCTime (Ti.fromGregorian 2021 4 6) (Ti.secondsToDiffTime 0)
d = Ti.diffUTCTime t (Ti.UTCTime (Ti.fromGregorian 2021 4 6) (Ti.secondsToDiffTime 8))
