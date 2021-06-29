{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, StandaloneDeriving, TypeApplications, TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, OverloadedStrings, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SpiderSpec (spec, hydrateKbtz) where

import Streamly as S

import qualified Streamly.Prelude as S
import Test.Hspec
import Test.QuickCheck.Checkers
import Test.QuickCheck
import Test.QuickCheck.Classes
import Control.Monad.IO.Class
import qualified Data.Text as Text

import Data.ProtoLens
import Data.Word
import Data.Maybe

import NetSpider.Snapshot
import Chopaan.Monad.Env
import Control.Monad.Bayes.Class
import Chopaan.Node.NodeId
import Chopaan.Kibbutz.KbtzId (KbtzId(..), KbtzName)
import Chopaan.Kibbutz
import Chopaan.Graph.Spider
import Chopaan.Comm.Comm (initQs, writeChan, MessageQs(..), readPubQ)
import Chopaan.Utils.Time (timeToUIntSeconds)

import Chopaan.Node.Folds
import Chopaan.Node.Storage (defBatteryParams)
import Chopaan.Graph
import Chopaan.Graph.Kbtz
import Chopaan.API.History
import Chopaan.Kibbutz.KbtzId


import qualified Data.Text as T
import qualified Data.Time as Ti

import qualified Network.Wai.Handler.Warp         as Warp

import           Servant
import           Servant.Client

import qualified Streamly.Internal.Data.Fold as FL
import Common
import qualified Proto.NodeMessageSchema.NodeMessages as NM
import qualified Proto.NodeMessageSchema.NodeMessages_Fields as NM
import Lens.Micro
import Control.Concurrent hiding (writeChan)
import Control.Applicative
import NetSpider.Spider
  (withSpider, clearAll)
import Control.Monad.Catch
import Data.Pool
    
spec :: Spec
spec = do
  let
    nNodes = 10
    nMessages = 400
    kId = KbtzId "test"
    t0 = t
    tn = Ti.UTCTime (Ti.fromGregorian 2021 8 8) (Ti.secondsToDiffTime 0)
  beforeAll (do
                let c = mkConfG ("localhost", 8182)
                withSpider (unConf $ meshG c) clearAll
                withSpider (unConf $ txG c) clearAll
                withSpider (unConf $ flowG c) clearAll
                withSpider (unConf $ statusG c) clearAll
                kp <- kbtzPool "localhost" 8182                 
                ns <- liftIO $ arbs @NodeMAC nNodes
                withResource kp (\c -> addKbtz c kId)
                mapM_ (\n -> withResource kp (\c -> addNodeToKbtz c kId n)) ns
                sp <- mkSpool c
                return (ns, sp)
            ) $ do
    describe "Spiders are great" $ do
      it "Check each fold" $ \(ns, sp) -> do
        es <- orderedES Source nMessages
        let
          tf = S.postscan timeFold es
          pf = S.postscan powerFold es
          ef = S.postscan energyFold es
          bf = S.postscan (batteryFold defBatteryParams) es
          df = S.postscan demandFold es
          lc f = do
            l <- S.length f
            l `shouldBe` nMessages
        lc tf
        lc pf
        lc ef
        lc bf
        lc df
        
      it "Sensor Fold works" $ \(ns, sp) -> do
        es <- do
          xs'' <- mapM (\(i, n) ->
                          (return . (S.map (\x -> (n, x))))
                          =<<
                          orderedES (if (mod i 2 == 0) then Source else Sink) nMessages)
                  $ zip [1..nNodes] ns
          return $ foldl S.wSerial S.nil xs''
        let s = S.postscan (FL.classify sensorFold) es
        print =<< (S.last s)
        l <- S.length s
        l `shouldBe` (nMessages * nNodes)
        
      it "qKbtz processor processes all messages!" $ \(ns, sp) -> do
        es <- do
          xs'' <- mapM (\i ->
                          orderedES (if (mod i 2 == 0) then Source else Sink) nMessages)
                  $ [1..nNodes]
          return $ foldl S.wAsync S.nil xs''
        rs <- do
          xs'' <- mapM (\i ->
                          orderedRS (if (i == 1) then Root else Child) nMessages (head ns))
                  $ [1..nNodes]
          return $ foldl S.wAsync S.nil xs''
        qs <- initQs
        k <- (S.avgRate 1000) <$> (runKibbutz $
          KbtzC { name = kId
                , nodes = ns
                , channelOpts = Left qs
                , spiderHost = "localhost"
                , spiderPort = 8182
                })
        let ns' = S.fromList $ cycle ns
        forkIO $ do
          S.mapM_ (\(n, e) -> writeChan (stateChan qs) n e)  $ S.zipWith (,) ns' es
          S.mapM_ (\(n, r) -> writeChan (statsChan qs) n r)  $ S.zipWith (,) ns' rs
          print "Messages Queued"
        l <- S.length $ S.take ((2 * nNodes * nMessages) + 0) k
        l `shouldBe` (2 * nNodes * nMessages)

    it "RS snapshot graph has the right number of nodes and links" $ \(ns, sp) -> do
      (gotNs, gotLs) <- snapDebug meshNodesSnapshot sp ns t0 tn
      oneNodePerMACPlusRoot gotNs nNodes
      -- $ for a tree structure with one root node, each node should have the root as its parent,
      -- $ while the root node should be linked to router
      -- treePlusStructure gotLs nNodes
      
    it "Stake snapshot graph has the right number of nodes and links" $ \(ns, sp) -> do
      (gotNs, gotLs) <- snapDebug txNodesSnapshot sp ns t0 tn
      oneNodePerMACPlusRoot gotNs nNodes
      constHypergraphLinks gotLs nNodes

    it "Status snapshot graph has the right number of nodes and links" $ \(ns, sp) -> do
      (gotNs, gotLs) <- snapDebug statusNodesSnapshot sp ns t0 tn
      oneNodePerMACPlusRoot gotNs nNodes
      constHypergraphLinks gotLs nNodes


oneNodePerMACPlusRoot sn nNodes = ((length $ sn)
                                    `shouldBe` (nNodes + 1))
constHypergraphLinks sl nNodes = ((length $ sl)
                                  `shouldBe` (nNodes))
treePlusStructure sl nNodes = ((length $ sl)
                                  `shouldBe` (nNodes + 1))

-- snapDebug :: (Show n, Show v, Show l)
--   => ([n] -> Ti.UTCTime -> Ti.UTCTime -> IO (SnapshotGraph n v l))
--   -> [n] -> Ti.UTCTime -> Ti.UTCTime -> IO (SnapshotGraph n v l) 
snapDebug snapfn sp ns t0 tn = do
  --print $ "Total Nodes: " <> (show . length $ ns)
  (gotNs, gotLs) <-  runSpider sp (snapfn ns t0 tn)
  print $ "Num Nodes: " <> (show . length  $ gotNs)
  print $ "Num Links: " <> (show . length  $ gotLs)
  -- print $ (fmap nodeId gotNs)
  -- print $ (fmap nodeAttributes gotNs)
  -- print $ (fmap linkNodeTuple gotLs)
  return $ (gotNs, gotLs)


data ESType = Source | Sink deriving (Eq, Ord, Show, Bounded, Enum)

data RSType = Root | Child deriving (Eq, Ord, Show, Bounded, Enum)

hydrateKbtz :: (IsStream t, Monad (t IO)) => KbtzName -> [NodeMAC] -> Int -> Int -> IO (t IO Bool)
hydrateKbtz kId ns nNodes nMessages = do
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
    qs <- liftIO $ initQs
    let ns' = S.fromList $ cycle ns
    S.mapM_ (\(n, e) -> writeChan (stateChan qs) n e)  $ S.zipWith (,) ns' es
    S.mapM_ (\(n, r) -> writeChan (statsChan qs) n r)  $ S.zipWith (,) ns' rs
    runKibbutz KbtzC { name = kId
                     , nodes = ns
                     , channelOpts = Left qs
                     , spiderHost = "localhost"
                     , spiderPort = 8182
                     }


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
      -- & NM.gridToBatteryCurrent .~ 0
      -- & NM.batteryToGridCurrent .~ 0
      & NM.gridCurrent .~ 0
      & NM.gridVoltage .~ 60
      & NM.temperature .~ 0
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
        p (Root) = "grid_test"
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
