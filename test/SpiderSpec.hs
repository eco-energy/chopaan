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

import Chopaan.Monad.Env
import Control.Monad.Bayes.Class
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
      let nNodes = 5
          nMessages = 50
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
        --S.mapM_ (\(n, r) -> writeChan (statsChan qs) n r)  $ S.zipWith (,) ns' rs

      l <- S.length $ S.take ((2 * nNodes * nMessages)) $ k
      -- let txns = S.repeatM @S.SerialT (readPubQ $ outbox qs)
      --     d = S.take (nMessages) $ S.trace (print) txns
      -- l' <- S.length d
      l `shouldBe` (2 * nNodes * nMessages)
      -- l' `shouldBe` (nNodes * nMessages)
      

orderedES :: Int -> IO (S.Serial NM.EnergyState)
orderedES n = do
  xs <- arbs n
  let xs' = map updateT $ zip [1..n] (zip xs tsL)
  return $ S.fromList xs'
  where
    updateT (i, (m, t')) = m
      & NM.cpuTime .~ (timeToUIntSeconds t')
      & NM.batteryVoltage .~ v
      & NM.solarVoltage .~ 16
      & NM.solarInputCurrent .~ si
      & NM.batteryToLoadCurrent .~ li
      & NM.gridToBatteryCurrent .~ 0
      & NM.batteryToGridCurrent .~ 0
      & NM.gridVoltage .~ 60
      where
        isOdd i = mod i 2 == 0
        v = if isOdd i then 14.8 else 8.0
        si = if isOdd i then 15 else 0
        li = if isOdd i then 0 else 10.0
        
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
d = Ti.diffUTCTime (Ti.UTCTime (Ti.fromGregorian 2021 4 6) (Ti.secondsToDiffTime 60)) t


-- nodeStream :: forall t. (IsStream t) => t MonadEnv NM.EnergyState
-- nodeStream = S.map snd $ S.iterateM (\xs -> do
--                                         threadDelay 1000
--                                         nodeStep @MonadEnv xs) (pure (startDay $ TimeOfDay 0 0 0, defMessage))


-- nodeStep :: forall m. (MonadSample m) => (Ti.UTCTime, NM.EnergyState) -> m (Ti.UTCTime, NM.EnergyState)
-- nodeStep (t, oldState) = do
--   -- note that outflow of current is assumed to be positive 
--   loadCurrent <- abs <$> normal 30 20
--   gridCurrent <- normal 0 20
--   solarCurrent <- biGauss daytime (30, 10) (0, 0.3) t
--   --solarVoltage <- biGauss daytime (17, 3) (0, 1) t
--   batteryVoltageDiff <- normal 0.01 0.001
--   gridVoltageDiff <- normal 0.03 0.03 
  
--   let
--     t' = addUTCTime (1 :: Ti.NominalDiffTime) t
--     batteryV = oldState ^. NM.batteryVoltage + batteryVoltageDiff
--     gridV = oldState ^. NM.gridVoltage + gridVoltageDiff
    
--     newState = (defMessage :: NM.EnergyState)
--       & NM.batteryVoltage .~ batteryV
--       & NM.gridVoltage .~ gridV
--       & NM.batteryToLoadCurrent .~ loadCurrent
--       & NM.batteryToGridCurrent .~ (if gridCurrent > 0 then gridCurrent else 0)
--       & NM.gridToBatteryCurrent .~ (if gridCurrent < 0 then gridCurrent else 0)
--       & NM.solarInputCurrent    .~ solarCurrent
--       & NM.temperature          .~ (26 :: Double)
--       & NM.cpuTime             .~  timeToUIntSeconds t
--   return $ (t', newState)
--   where
--     biGauss :: (MonadSample m) => (t -> Bool) -> (Double, Double) -> (Double, Double) -> t -> m Double 
--     biGauss choice (mu, theta) (mu', theta') chooser = case choice chooser of
--       True -> normal mu theta
--       False -> normal mu' theta'
--     daytime :: Ti.UTCTime -> Bool
--     daytime tx = t' > sunrise && t' < sunset
--       where
--         t' = Ti.utcTimeOfDay tx
--     (sunrise, sunset) = (TimeOfDay 6 0 0, TimeOfDay 18 0 0)


-- startDay :: TimeOfDay -> Ti.UTCTime
-- startDay = Ti. $ fromGregorian 1 1 2020
