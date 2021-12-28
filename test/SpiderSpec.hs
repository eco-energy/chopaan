{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, StandaloneDeriving, TypeApplications, TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, OverloadedStrings, FlexibleContexts, ViewPatterns #-}
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
import qualified Data.Aeson as A
import qualified Data.Aeson.Parser as A
import qualified Data.Aeson.Types as A

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Network.HTTP.Client as NC (brConsume, responseBody)
import Database.InfluxDB.Query (Query, withQueryResponse)
import Database.InfluxDB (WriteParams, QueryParams)
import Database.InfluxDB.JSON (parseSeriesObject, parseSeriesBody, parseResultsObject, parseErrorObject)
import qualified Data.Vector as V
import Data.Influxable
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
import Chopaan.Types (PoolConf(..), InfluxConn(..))
import Streamly.Binary (encodeFold, toWino)

import qualified Data.Text as T
import qualified Data.Time as Ti

import qualified Network.Wai.Handler.Warp         as Warp

--import           Servant
--import           Servant.Client

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

nNodes = 100
nMessages = 1000
kId = KbtzId "testK"
t0 = t
tn = Ti.UTCTime (Ti.fromGregorian 2021 8 8) (Ti.secondsToDiffTime 0)

foldSpec :: Spec
foldSpec = do
  describe "Validate ES processing folds and their composition" $ do
    it "Check each fold" $ do
      let
        es = orderedES Source nMessages
        tf = sampleStream $ S.postscan timeFold es
        pf = sampleStream $ S.postscan powerFold es
        ef = sampleStream $ S.postscan energyFold es
        bf = sampleStream $ S.postscan (batteryFold undefined defBatteryParams) es
        df = sampleStream $ S.postscan demandFold es
        lc f = do
          l <- S.length $ f
          l `shouldBe` nMessages
      lc tf
      lc pf
      lc ef
      lc bf
      lc df
    it "Sensor Fold works" $ do
      ns <- liftIO $ arbs @NodeMAC nNodes
      l <- S.length
           $ S.tapRate 1 (\r -> liftIO $ print ("sensorFold rate: " <> (show r)))
           $ (sampleStream $ S.postscan (FL.classify (sensorFold undefined)) (esStreams nMessages nNodes ns))
      l `shouldBe` (nMessages * nNodes)
      

kbtzSpec :: Spec
kbtzSpec = do
  aroundAll (TC.withContainers (runWithDBPools)) $ describe "Spiders are great" $ do
    it "qKbtz processor processes all messages!" $ \(ns, db, ic) -> do
      let sp = (spools db)
          es = sampleStream $ esStreams nMessages nNodes ns 
          rs = rsStreams nMessages nNodes ns
      qs <- initQs
      k <- S.hoist (runGraphWithDB db) <$> (runGraphWithDB db $ do
        runKibbutz KbtzC { name = kId
                         , structure = undefined
                         , channelOpts = (Right qs)
                         , s3Opts = Nothing
                         , influxCon = ic
                         })
      forkIO $ do
        S.drain $
          S.mapM (\(n, e) -> writeChan (stateChan qs) n e) es
          `S.wAsync`
          S.mapM (\(n, r) -> writeChan (statsChan qs) n r) rs
        print "Messages Queued"
      l <- S.length -- S.fold (FL.tee FL.length (encodeFold "testFile"))
           --- $ fmap toWino
           $ S.take ((2 * nNodes * nMessages) + 0) k
      l `shouldBe` (2 * nNodes * nMessages)

    xit "RS snapshot graph has the right number of nodes and links" $ \(ns, db, _) -> do
      (gotNs, gotLs) <- snapDebug meshNodesSnapshot (spools db) ns t0 tn
      oneNodePerMACPlusRoot gotNs nNodes
      
    xit "Stake snapshot graph has the right number of nodes and links" $ \(ns, db, _) -> do
      (gotNs, gotLs) <- snapDebug txNodesSnapshot (spools db) ns t0 tn
      oneNodePerMACPlusRoot gotNs nNodes
      constHypergraphLinks gotLs nNodes

    xit "Status snapshot graph has the right number of nodes and links" $ \(ns, db, _) -> do
      (gotNs, gotLs) <- snapDebug statusNodesSnapshot (spools db) ns t0 tn
      oneNodePerMACPlusRoot gotNs nNodes
      constHypergraphLinks gotLs nNodes
    xit "Flow snapshot graph has the right number of nodes and links" $ \(ns, db, _) -> do
      (gotNs, gotLs) <- snapDebug flowNodesSnapshot (spools db) ns t0 tn
      oneNodePerMACPlusRoot gotNs nNodes
      constHypergraphLinks gotLs nNodes
    it "NodeQueries should yield errythang" $ \(ns, db, ic) -> do
       let kns = asKbtzNode kId <$> ns
           mqttDB = "chopaanMQTT"
           wp' = wp ic mqttDB
           qp' = qp ic mqttDB
           qgp = QueryGenParams mqttDB "\"autogen\"" Nothing Nothing 
           nqs = nodeQueries qgp <$> kns
           eqNM l = (abs (l - nMessages)) < 2
       t <- qResultTest qp' eqNM nqs
       t `shouldBe` (True)

qResultTest :: forall m. (S.MonadAsync m) => QueryParams -> (Int -> Bool) -> [NodeQueries] -> m (Bool)
qResultTest qp' eqNM nqs = do
  ls <- mapM (chkNodeQs qp') nqs
  return $ all (\(a, b, c, d) -> eqNM a && eqNM b && eqNM c && eqNM d) ls

chkNodeQs :: forall m. (S.MonadAsync m) => QueryParams -> NodeQueries -> m (Int, Int, Int, Int)
chkNodeQs qp nq = do
  ps <- resLen (powerQ nq)
  es <- resLen (energyQ nq)
  bs <- resLen (batteryQ nq)
  ms <- resLen (meshQ nq)
  --liftIO . print $ (ps, es, bs, ms)
  return (ps, es, bs, ms)
  where
    resLen = (pure . fromMaybe 0) <=< (S.the . S.mapM mkQ . S.fromList)
    lengthParser :: A.Value -> A.Parser Int
    lengthParser val0 = do
      results <- parseResultsObject val0
      series <- V.forM results $ \val -> do
        r <- foldr1 (<|>)
          [ Left <$> parseErrorObject val
          , Right <$> parseSeriesObject val
          ]
        case r of
          Left err -> fail err
          Right vec -> return $ vec
      (join -> values) <- V.forM (join series) $ \val -> do
        (name, tags, columns, values) <- parseSeriesBody val
        return values
      return $ V.length values
          
    countm _ resp = do
      chunks <- (NC.brConsume $ NC.responseBody resp)
      let body = BL.fromChunks chunks
      case A.eitherDecode' body of
        Left message -> error message
        Right val -> do 
          case A.parse lengthParser val of
            A.Success veclen -> return $ veclen
            A.Error message -> error message
    mkQ :: Query -> m Int
    mkQ q = liftIO $ withQueryResponse qp Nothing q countm
       --(V.length (fst rs)) `shouldBe` nMessages

runWithDBPools :: (TC.MonadDocker m) => m ([NodeMAC], DBPools, InfluxConn)
runWithDBPools = do
  -- ((tHost, tPort), (iHost, iPort)) <- runDBs "kbtzSpec" --
  let (tHost, tPort) = ("localhost", 8182)
      (iHost, iPort) = ("localhost", 8086) 
  --let c = mkConfG (host, port)
  let pc = PoolConf 10 100 20
  sp <- mkDBPools pc tHost tPort
  let kp = gremlinPool sp
      mqttDB = "chopaanMQTT"
      ic = (InfluxConn (T.pack iHost) iPort)
  -- Create Influx DB!
  liftIO $ createDB ic mqttDB
  ns <- liftIO $ arbs @NodeMAC nNodes
  liftIO $ withResource kp  (\c -> addKbtz c kId)
  liftIO $ mapM_ (\n -> withResource kp (\c -> addNodeToKbtz c kId n)) ns
  return (ns, sp, ic)


oneNodePerMACPlusRoot sn nNodes = ((length $ sn)
                                    `shouldBe` (nNodes + 1))
constHypergraphLinks sl nNodes = ((length $ sl)
                                  `shouldBe` (nNodes))
treePlusStructure sl nNodes = ((length $ sl)
                                  `shouldBe` (nNodes + 1))

snapDebug snapfn sp ns t0 tn = do
  (gotNs, gotLs) <-  runSpider sp (snapfn ns t0 tn)
  print $ "Num Nodes: " <> (show . length  $ gotNs)
  print $ "Num Links: " <> (show . length  $ gotLs)
  return $ (gotNs, gotLs)


data ESType = Source | Sink deriving (Eq, Ord, Show, Bounded, Enum)

data RSType = Root | Child deriving (Eq, Ord, Show, Bounded, Enum)


esStreams :: forall m. (S.MonadAsync m, MonadSample m) => Int -> Int -> [NodeMAC] -> S.SerialT m (NodeMAC, NM.EnergyState) 
esStreams nMessages nNodes ns = S.concatMapWith S.wSerial es
                  (S.fromList (zip [1..nNodes] ns))
  where
    es :: (Int, NodeMAC) -> S.SerialT m (NodeMAC, NM.EnergyState)
    es (i, n) = withTag <$> (orderedES ty nMessages)
      where
        ty = if (mod i 2 == 0) then Source else Sink
        withTag x = (n, x)

rsStreams :: forall m. (S.MonadAsync m) => Int -> Int -> [NodeMAC] -> S.SerialT m (NodeMAC, NM.RuntimeStats) 
rsStreams nMessages nNodes ns = S.concatMapWith S.wSerial rs
                  (S.fromList (zip [1..nNodes] ns))
  where
    rs :: (Int, NodeMAC) -> S.SerialT m (NodeMAC, NM.RuntimeStats)
    rs (i, n) = withTag <$> (orderedRS ty nMessages (head ns))
      where
        ty = (if (i == 1) then Root else Child)
        withTag x = (n, x)


orderedES :: (S.MonadAsync m, MonadSample m) => ESType -> Int -> S.SerialT m NM.EnergyState
orderedES et n = S.take n $ S.map snd $ S.iterateM nodeStep (pure (t, start))
  where
    start = defMessage
      & NM.cpuTime .~ (timeToUIntSeconds t)
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
        
orderedRS :: MonadIO m => RSType -> Int -> NodeMAC -> S.SerialT m NM.RuntimeStats
orderedRS r n (NodeId root) = S.concatM . liftIO $ do
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


nodeStep :: forall m. (MonadSample m) => (Ti.UTCTime, NM.EnergyState) -> m (Ti.UTCTime, NM.EnergyState)
nodeStep (t, oldState) = do
  -- note that outflow of current is assumed to be positive 
  loadCurrent <- abs <$> normal 30 20
  gridCurrent <- normal 0 20
  solarCurrent <- biGauss daytime (30, 10) (0, 0.3) t
  --solarVoltage <- biGauss daytime (17, 3) (0, 1) t
  batteryVoltageDiff <- normal 0.01 0.001
  gridVoltageDiff <- normal 0.03 0.03 
  
  let
    t' = Ti.addUTCTime (1 :: Ti.NominalDiffTime) t
    batteryV = oldState ^. NM.batteryVoltage + batteryVoltageDiff
    gridV = oldState ^. NM.gridVoltage + gridVoltageDiff
    
    newState = (defMessage :: NM.EnergyState)
      & NM.batteryVoltage .~ batteryV
      & NM.gridVoltage .~ gridV
      & NM.batteryToLoadCurrent .~ loadCurrent
      & NM.batteryToGridCurrent .~ (if gridCurrent > 0 then gridCurrent else 0)
      & NM.gridToBatteryCurrent .~ (if gridCurrent < 0 then gridCurrent else 0)
      & NM.solarInputCurrent    .~ solarCurrent
      & NM.temperature          .~ (26 :: Double)
      & NM.cpuTime             .~  timeToUIntSeconds t
  return $ (t', newState)
  where
    biGauss :: (MonadSample m) => (t -> Bool) -> (Double, Double) -> (Double, Double) -> t -> m Double 
    biGauss choice (mu, theta) (mu', theta') chooser = case choice chooser of
      True -> normal mu theta
      False -> normal mu' theta'
    daytime :: Ti.UTCTime -> Bool
    daytime tx = t' > sunrise && t' < sunset
      where
        t' = Ti.timeToTimeOfDay (Ti.utctDayTime tx)
    (sunrise, sunset) = (Ti.TimeOfDay 6 0 0, Ti.TimeOfDay 18 0 0)


-- startDay :: TimeOfDay -> Ti.UTCTime
-- startDay = Ti. $ fromGregorian 1 1 2020
