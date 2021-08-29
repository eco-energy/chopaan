{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, StandaloneDeriving, TypeApplications, TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, OverloadedStrings, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SpiderSpec (spec) where

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.IsStream as S
import Test.Hspec
import Test.QuickCheck.Checkers
import Test.QuickCheck
import Test.QuickCheck.Classes
import qualified TestContainers as TC
import qualified TestContainers.Hspec as TC
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.STM.TBQueue
import Control.Concurrent.STM
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
import Chopaan.Hydrate
import Chopaan.Graph.Spider
import Chopaan.Comm.Comm (initQs, writeChan, MessageQs(..), readPubQ)
import Chopaan.Comm.Queues
import Chopaan.Utils.Time (timeToUIntSeconds)

import Chopaan.Node.Folds
import Chopaan.Node.Storage (defBatteryParams)
import Chopaan.Graph
import Chopaan.Graph.Kbtz
import Chopaan.API.History
import Chopaan.Kibbutz.KbtzId
import Chopaan.Types (PoolConf(..))

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
import qualified NetSpider.Spider as NS
  (withSpider, clearAll)
import Control.Monad.Catch
import Data.Pool
    
spec :: Spec
spec = do
  foldSpec
  kbtzSpec
  --hydrationSpec

nNodes = 10
nMessages = 100
kId = KbtzId "test"
t0 = t
tn = Ti.UTCTime (Ti.fromGregorian 2021 8 8) (Ti.secondsToDiffTime 0)

foldSpec :: Spec
foldSpec = do
  describe "Validate ES processing folds and their composition" $ do
    it "Check each fold" $ do
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
    it "Sensor Fold works" $ do
      ns <- liftIO $ arbs @NodeMAC nNodes
      es <- do
        xs'' <- mapM (\(i, n) ->
                        (return . (S.map (\x -> (n, x))))
                       =<<
                       orderedES (if (mod i 2 == 0) then Source else Sink) nMessages)
                $ zip [1..nNodes] ns
        return $ foldl S.wSerial S.nil xs''
      let s = S.postscan (FL.classify sensorFold) es
      l <- S.length s
      l `shouldBe` (nMessages * nNodes)


runWithDBPools :: (TC.MonadDocker m) => m ([NodeMAC], DBPools)
runWithDBPools = do
  (host, port) <- runJanus "kbtzSpec"
  let c = mkConfG (host, port)
  let pc = PoolConf 1 20000 1
  sp <- mkDBPools pc host port
  let kp = gremlinPool sp
  ns <- liftIO $ arbs @NodeMAC nNodes
  liftIO $ withResource kp  (\c -> addKbtz c kId)
  liftIO $ mapM_ (\n -> withResource kp (\c -> addNodeToKbtz c kId n)) ns
  return (ns, sp)

kbtzSpec :: Spec
kbtzSpec = do
  aroundAll (TC.withContainers (runWithDBPools)) $ describe "Spiders are great" $ do
    it "qKbtz processor processes all messages!" $ \(ns, db) -> do
      let sp = (spools db)
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
      k <- S.hoist (runGraphWithDB db) <$> (runGraphWithDB db $ do
        runKibbutz KbtzC { name = kId
                         , nodes = ns
                         , channelOpts = (Right qs)
                         , s3Opts = Nothing 
                         })
      let ns' = S.fromList $ cycle ns
      forkIO $ do
        S.mapM_ (\(n, e) -> writeChan (stateChan qs) n e)  $ S.zipWith (,) ns' es
        S.mapM_ (\(n, r) -> writeChan (statsChan qs) n r)  $ S.zipWith (,) ns' rs
        print "Messages Queued"
          --let o = runNodeQueue (outbox qs)
          -- atomically $ do
          --   e <- isEmptyTBQueue o
          --   case e of
          --     True -> retry
          --     False -> void (flushTBQueue o)
      l <- S.length $ S.take ((2 * nNodes * nMessages) + 0) k
      l `shouldBe` (2 * nNodes * nMessages)

    it "RS snapshot graph has the right number of nodes and links" $ \(ns, db) -> do
      (gotNs, gotLs) <- snapDebug meshNodesSnapshot (spools db) ns t0 tn
      oneNodePerMACPlusRoot gotNs nNodes
      
    it "Stake snapshot graph has the right number of nodes and links" $ \(ns, db) -> do
      (gotNs, gotLs) <- snapDebug txNodesSnapshot (spools db) ns t0 tn
      oneNodePerMACPlusRoot gotNs nNodes
      constHypergraphLinks gotLs nNodes

    it "Status snapshot graph has the right number of nodes and links" $ \(ns, db) -> do
      (gotNs, gotLs) <- snapDebug statusNodesSnapshot (spools db) ns t0 tn
      oneNodePerMACPlusRoot gotNs nNodes
      constHypergraphLinks gotLs nNodes
    it "Flow snapshot graph has the right number of nodes and links" $ \(ns, db) -> do
      (gotNs, gotLs) <- snapDebug flowNodesSnapshot (spools db) ns t0 tn
      oneNodePerMACPlusRoot gotNs nNodes
      constHypergraphLinks gotLs nNodes

-- hydrationSpec :: Spec
-- hydrationSpec = aroundAll (TC.withContainers (runJanus "hydrationSpec")) $ describe "hydration tests" $ do
--   it "Hydration Works" $ \(host, port) -> do
--     let labNodes = [ "7c:9e:bd:f5:ec:74", "c4:4f:33:67:ea:69"
--                      , "ac:67:b2:11:e5:c4", "7c:9e:bd:f6:43:88" ]
--         s3op = "dosti-datastream"
--     qs <- initQs
--     ps <- mkDBPools host port
--     let kc = KbtzC { name = KbtzId "labKbtz"
--                    , nodes = labNodes
--                    , channelOpts = Right qs
--                    , s3Opts = Just s3op }
--     h <- hydrateKbtzM ps kc (t0, tn)
--           --h = s3Stream (zip labNodes (repeat Nothing)) s3op
--     S.drain h
--     1 `shouldBe` 1
--     where
--       t0 = Ti.UTCTime (Ti.fromGregorian 2021 8 9) (Ti.secondsToDiffTime 0)
--       tn = Ti.UTCTime (Ti.fromGregorian 2021 8 11) (Ti.secondsToDiffTime 0)

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
        v Sink = 7.0
        si Source = 15
        si Sink = 0
        li Source = 0
        li Sink = 50.0
        sv Source = 50
        sv Sink = 0
        
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
et = Ti.UTCTime (Ti.fromGregorian 2021 8 10) (Ti.secondsToDiffTime 0)
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
