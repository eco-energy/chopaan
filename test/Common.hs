{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, StandaloneDeriving, TypeApplications, TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, OverloadedStrings, FlexibleContexts, QuantifiedConstraints, UndecidableInstances #-}
module Common (
  module Test.Hspec,
  module Test.QuickCheck.Checkers,
  module Test.QuickCheck,
  almostEqual,
  runJanus,
  runDBs
  ) where

import Test.Hspec
import Test.QuickCheck.Checkers
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Instances.Time
import Data.ProtoLens.Arbitrary
import Data.Function ((&))
import qualified TestContainers.Docker as TC
import qualified TestContainers.Hspec as TC
import qualified TestContainers.Image as TC

import GHC.IO.Handle
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Reader hiding (ask)
import Control.Monad.Trans.Resource
import System.Directory
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Time as Ti
import Numeric.Compensated
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Affine
import Linear.Matrix
import Linear.Quaternion

import Chopaan.Hydration.Prefix
import Chopaan.Types
import Chopaan.Graph
import Chopaan.Node.NodeId
import Chopaan.Node.NodeSensors
import Chopaan.Node.Metrics
import Chopaan.Node.Storage.Battery
import Chopaan.Node.Mesh
import Chopaan.Kibbutz.Transactor.Stake
import Chopaan.Kibbutz.Transactor.Status
import Chopaan.Kibbutz.Transactor
import Chopaan.Comm.S3
import qualified Chopaan.Node.HW as HW
import qualified Chopaan.Node.Components as C
import Chopaan.Kibbutz.KbtzId (KbtzId(..))
import Chopaan.Kibbutz
import Chopaan.Graph.Spider
import Chopaan.Graph.Snapshot

-- import Chopaan.Ui.Interaction
-- import Chopaan.Ui.Base
-- import Chopaan.Ui.Events
-- import Chopaan.Ui.ThreeD

import qualified Proto.NodeMessageSchema.NodeMessages as NM
import qualified Proto.NodeMessageSchema.NodeMessages_Fields as NM

-- tc = TC.newTracer print


tr :: TC.MonadDocker m => TC.Container -> m ()
tr = \c -> TC.withLogs c ((\stdout stderr -> liftIO $ do
                 print =<< hGetLine stdout
                 print =<< hGetLine stderr
                 ))

runDBs :: TC.MonadDocker m => T.Text -> m ((String, Int), (String, Int))
runDBs name = do
  j <- runJanus name
  i <- runInflux name
  return $ (j, i)

runJanus :: (TC.MonadDocker m) => T.Text -> m (String, Int)
runJanus name = do
  c <- ask
  --let t = TC.newTracer print
  --let c' = c { TC.configTracer = t }
  jC <- (flip runReaderT $ c) $ TC.run =<< (janus name)
  tr jC
  --liftIO . print $ c
  pure ("localhost", TC.containerPort jC 8182)

runInflux :: (TC.MonadDocker m) => T.Text -> m (String, Int)
runInflux name = do
  c <- ask
  let t = TC.newTracer print
  iC <- (flip runReaderT $ c) $ TC.run (influx name)
  pure ("localhost", TC.containerPort iC 8182)

withTC :: (forall a. (ReaderT TC.Config ResIO a) -> IO a)
withTC a = TC.runResourceT $ runReaderT a TC.defaultDockerConfig

janusC :: (TC.MonadDocker m) => T.Text -> m (TC.Container)
janusC n = TC.run =<< janus n

influx :: T.Text -> TC.ContainerRequest
influx name = TC.containerRequest (TC.fromTag "influxdb:1.8")
              & TC.setName ("influx-test-" <> name)
              & TC.setExpose [ 8086 ]
              & TC.setWaitingFor waiter
  where
    waiter = TC.waitForLogLine TC.Stdout (TL.isInfixOf readyLog)
    readyLog = "lvl=info msg=\"Listening for signals\""

janus :: (MonadIO m) => T.Text -> m TC.ContainerRequest
janus name = do
  conf <- liftIO $ makeAbsolute confRel
  idx <- liftIO $ makeAbsolute idxRel
  liftIO . print $ (conf, idx)
  return $ withMounts janus' conf idx
  where
    confRel = "./janusgraph-config/config/"
    idxRel = "./janusgraph-config/indexes/net-spider-index.groovy"
    janus' = TC.fromTag "janusgraph/janusgraph:0.6.0"
    withMounts toImg conf idx = TC.containerRequest toImg
                       & TC.setName ("janus-test-" <> name)          
                       & TC.setVolume vols
                       & TC.setExpose [ 8182 ]
                     --  & TC.setWaitingFor waiter
      where
        waiter = TC.waitUntilTimeout 120 $ TC.waitForLogLine TC.Stdout (TL.isInfixOf readyLog)
        readyLog = "Channel started at port 8182"
        confF f = T.pack (conf <> f)
        vols =
          [ ( confF "janusgraph.properties"
            , "/etc/opt/janusgraph/janusgraph.properties:ro"
            )
          , ( confF "gremlin-server-0.6.yaml"
            , "/etc/opt/janusgraph/janusgraph-server.yaml:ro"
            )
          , ( T.pack idx
            , "/files/net-spider-index.groovy"
            )
          ]

almostEqual :: (Show a, Eq a, Num a, Ord a) => a -> a -> a -> Expectation
almostEqual eta a b = do
  ((abs $ a - b) < eta) `shouldBe` True

instance Arbitrary Prefix where
  arbitrary = (pure . Prefix . getPositive) =<< arbitrary

instance Arbitrary Resolution where
  arbitrary = genericArbitrary

instance (Num a, Arbitrary a, Compensable a) => (Arbitrary (Compensated a)) where
  arbitrary =  (\a -> pure $ add a 0 compensated) =<< arbitrary

instance Arbitrary Watts where
  arbitrary = genericArbitrary

instance Arbitrary WattSeconds where
  arbitrary = genericArbitrary

instance (Arbitrary v) => Arbitrary (Node v) where
  arbitrary = genericArbitrary


instance (Arbitrary e, Arbitrary p) => Arbitrary (Battery e p) where
  arbitrary = genericArbitrary

instance Arbitrary s => Arbitrary (Sec s) where
  arbitrary = genericArbitrary
instance Arbitrary s => Arbitrary (I s) where
  arbitrary = genericArbitrary
instance Arbitrary s => Arbitrary (V s) where
  arbitrary = genericArbitrary
instance Arbitrary s => Arbitrary (Res s) where
  arbitrary = genericArbitrary


instance Arbitrary a => Arbitrary (NodeSensors a) where
  arbitrary = genericArbitrary

instance Arbitrary a => Arbitrary (NodeT' a) where
  arbitrary = genericArbitrary

instance (Arbitrary e, Arbitrary p) => Arbitrary (SensorMetrics e p) where
  arbitrary = genericArbitrary

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
  arbitrary = arbitraryMessage


instance (Arbitrary n, Arbitrary v) => Arbitrary (SnapshotNode n v) where
  arbitrary = genericArbitrary

instance (Arbitrary n, Arbitrary l) => Arbitrary (SnapshotLink n l) where
  arbitrary = genericArbitrary

instance Arbitrary NodeVersion where
  arbitrary = (pure . NodeVersion . T.pack) =<< arbitrary

instance Arbitrary MeshNode where
  arbitrary = genericArbitrary

instance Arbitrary RxSignal where
  arbitrary = genericArbitrary

instance Arbitrary Role where
  arbitrary = genericArbitrary

instance (Arbitrary n) => Arbitrary (TxStatus' n) where
  arbitrary = genericArbitrary

instance (Arbitrary n) => Arbitrary (Stake' n) where
  arbitrary = genericArbitrary

instance (Arbitrary n, Arbitrary v, Arbitrary e) => Arbitrary (SG' n v e) where
  arbitrary = genericArbitrary

instance (forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (k n a b)) => Arbitrary (G k n) where
  arbitrary = genericArbitrary

-- instance Arbitrary Button where
--   arbitrary = genericArbitrary

-- instance Arbitrary PointerType where
--   arbitrary = genericArbitrary

-- instance Arbitrary Pointer where
--   arbitrary = genericArbitrary

instance (Arbitrary a) => Arbitrary (V2 a) where
  arbitrary = genericArbitrary

instance (Arbitrary a) => Arbitrary (V3 a) where
  arbitrary = genericArbitrary

instance (Arbitrary a) => Arbitrary (V4 a) where
  arbitrary = genericArbitrary

instance (Arbitrary a) => Arbitrary (Quaternion a) where
  arbitrary = genericArbitrary

instance (Arbitrary a) => Arbitrary (Point V2 a) where
  arbitrary = genericArbitrary

instance (Arbitrary a) => Arbitrary (Point V3 a) where
  arbitrary = genericArbitrary

