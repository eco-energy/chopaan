{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, StandaloneDeriving, TypeApplications, TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, OverloadedStrings, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SpiderSpec (spec) where

import Streamly.Prelude (IsStream, MonadAsync)
import qualified Streamly.Prelude as S
import Test.Hspec
import Test.QuickCheck.Checkers
import Test.QuickCheck
import Test.QuickCheck.Classes
import Control.Monad.IO.Class
import qualified Data.Text as Text
import Test.QuickCheck.Arbitrary.Generic
import Data.ProtoLens.Arbitrary
import Data.ProtoLens
import Data.Word
import Data.Maybe

import NetSpider.Snapshot
import Chopaan.Monad.Env
import Control.Monad.Bayes.Class
import Chopaan.Node.NodeId
import Chopaan.Kibbutz.KbtzId (KbtzId(..))
import Chopaan.Kibbutz
import Chopaan.Graph.Spider
import Chopaan.Comm.Comm (initQs, writeChan, MessageQs(..), readPubQ)
import Chopaan.Utils.Time (timeToUIntSeconds)
import qualified Data.Text as T
import qualified Data.Time as Ti

import qualified Proto.NodeMessageSchema.NodeMessages as NM
import qualified Proto.NodeMessageSchema.NodeMessages_Fields as NM
import Lens.Micro
import Control.Concurrent hiding (writeChan)
import Control.Applicative
import NetSpider.Spider
  (withSpider, clearAll)

instance Arbitrary (NodeMAC) where
  arbitrary = do
    let el = ['a'..'z']
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
  let
    nNodes = 5
    nMessages = 10
    t0 = t
    tn = Ti.addUTCTime (d * (fromIntegral $ nNodes * nMessages)) t0
    
  beforeAll (liftIO $ arbs @NodeMAC nNodes) $ do
    describe "Spiders are great" $ do
      it "qKbtz processor processes all messages!" $ \ns -> do
        es <- do
          xs'' <- mapM (\i ->
                          orderedES (if (mod i 2 == 0) then Source else Sink) nMessages)
                  $ [1..nNodes]
          return $ foldl S.wSerial S.nil xs''
        rs <- do
          xs'' <- mapM (\i ->
                          orderedRS (if (i == 1) then Root else Child) nMessages (head ns))
                  $ [1..nNodes]
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

        l <- S.length $ S.take ((2 * nNodes * nMessages) + 1) k
        l `shouldBe` (2 * nNodes * nMessages)


    it "RS snapshot graph has the right number of nodes and links" $ \ns -> do
      (gotNs, gotLs) <- snapDebug meshSnapshot ns t0 tn
      oneSnapNodePerNodeMAC gotNs nNodes
      -- $ for a tree structure with one root node, each node should have the root as its parent,
      -- $ while the root node should be linked to router
      oneLinkPerNodeMAC gotLs nNodes
      
    it "Stake snapshot graph has the right number of nodes and links" $ \ns -> do
      (gotNs, gotLs) <- snapDebug stakeSnapshot ns t0 tn
      oneSnapNodePerNodeMAC gotNs nNodes
      oneLinkPerNodeMAC gotLs nNodes

    it "Status snapshot graph has the right number of nodes and links" $ \ns -> do
      
      (gotNs, gotLs) <- snapDebug statusSnapshot ns t0 tn
      oneSnapNodePerNodeMAC gotNs nNodes
      oneLinkPerNodeMAC gotLs nNodes


oneSnapNodePerNodeMAC sn nNodes = ((length $ filter (isJust . nodeAttributes) sn)
                                    `shouldBe` (nNodes))
oneLinkPerNodeMAC sl nNodes = ((length $ sl) `shouldBe` (nNodes))

snapDebug :: (Show n, Show v, Show l)
  => ([n] -> Ti.UTCTime -> Ti.UTCTime -> IO (SnapshotGraph n v l))
  -> [n] -> Ti.UTCTime -> Ti.UTCTime -> IO (SnapshotGraph n v l) 
snapDebug snapfn ns t0 tn = do
  print $ "Total Nodes: " <> (show . length $ ns)
  (gotNs, gotLs) <-  (snapfn ns t0 tn)
  print $ "Num Nodes: " <> (show . length  $ gotNs)
  print $ "Num Links: " <> (show . length  $ gotLs)
  print $ (fmap nodeId gotNs)
  print $ (fmap nodeAttributes gotNs)
  print $ gotLs
  return $ (gotNs, gotLs)


data ESType = Source | Sink deriving (Eq, Ord, Show, Bounded, Enum)

data RSType = Root | Child deriving (Eq, Ord, Show, Bounded, Enum)

orderedES :: ESType -> Int -> IO (S.Serial NM.EnergyState)
orderedES et n = do
  xs <- arbs n
  let xs' = map updateT $ (zip xs tsL)
  return $ S.fromList xs'
  where
    updateT (m, t') = m
      & NM.cpuTime .~ (timeToUIntSeconds t')
      & NM.batteryVoltage .~ v et
      & NM.solarVoltage .~ sv et
      & NM.solarInputCurrent .~ si et
      & NM.batteryToLoadCurrent .~ li et
      & NM.gridToBatteryCurrent .~ 0
      & NM.batteryToGridCurrent .~ 0
      & NM.gridVoltage .~ 60
      where
        v Source = 14.8
        v Sink = 8.0
        si Source = 15
        si Sink = 0
        li Source = 0
        li Sink = 10.0
        sv Source = 18
        sv Sink = 9
        
orderedRS :: RSType -> Int -> NodeMAC -> IO (S.Serial NM.RuntimeStats)
orderedRS r n (NodeId root) = do
  xs <- arbs n
  let xs' = map updateT $ zip xs tsL
  return $ S.fromList xs'
  where
    updateT (m, t') = m & NM.cpuTime .~ (timeToUIntSeconds t')
                       & NM.isRoot .~ (x r)
                       & NM.version .~ "version1"
                       & (NM.parent . NM.macAddr) .~ (p r)  
      where
        p (Root) = "router"
        p (Child) = root
        x (Root) = True
        x (Child) = False

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
