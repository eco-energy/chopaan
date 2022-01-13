{-# LANGUAGE TypeApplications, ScopedTypeVariables, RecordWildCards, FlexibleContexts, OverloadedStrings, ConstraintKinds #-}
module Chopaan where

import Control.Applicative
import Control.Monad.IO.Class ( MonadIO(liftIO) )

import Chopaan.Kibbutz
import Chopaan.Hydration.Prefix
import Chopaan.Hydrate ( hConfDef, mkTKbtz, mkConfig, runHydration, HConS, TKbtzim)
import Chopaan.Kibbutz.KbtzId ( KbtzId(KbtzId) )
import Chopaan.Kibbutz.FS
import Chopaan.Node.NodeId ( NodeId(NodeId), NodeMAC )
import Chopaan.API.History ()
import Chopaan.Types
    ( App(appOptions),
      Options(Options, influxConn, poolConf, hydrationOpts, dbOpts,
              kibbutzOpts, nodeOpts, mqttOpts, logVerbose),
      HydrationOpts(s3BucketName),
      KibbutzOpts(KibbutzOpts, name),
      MQTTOpts,
      InfluxConn,
      icOptions )
import Chopaan.Graph.Kbtz ( getKbtzim )
import Chopaan.Graph
    ( GraphM, runGraphM, withKbtzPool, tkOptions, getKNs, addzim )
import Data.Influxable (createDB)
import Data.Bifunctor ( Bifunctor(..) )
import Data.Pool (stats)
import qualified Data.Map.Strict as M
import Data.Maybe

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Stream.IsStream.Generate as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Pipe as Pipe
import qualified Streamly.Internal.FileSystem.Handle as H
import qualified Streamly.Internal.FileSystem.File as File


import System.Remote.Monitoring (forkServer)

import Network.AWS.S3 (BucketName(..))
import qualified Streamly.Internal.Data.Stream.IsStream as S

import Options.Applicative ( execParser )
import RIO
    ( MonadIO(liftIO),
      stdout,
      ReaderT,
      MonadReader(ask),
      BufferMode(LineBuffering),
      RIO,
      hSetBuffering,
      atomically )
import qualified Data.Time as Ti

import ConCat.Graphics.Image
import ConCat.Graphics.Color
import ConCat.Synchronous
import Chopaan.Ui
import Path.IO
import Path


type Sources = (MQTTOpts, HydrationOpts)
type Sink = InfluxConn 

runKbtzim :: forall t m.
  (HConS t m, KConS t m)
  => Ti.UTCTime
  -> MQTTOpts
  -> HydrationOpts
  -> InfluxConn
  -> Kbtzim
  -> t m (Either (NodeMAC, Prefix) (KbtzScene NodeMAC))
runKbtzim t0 mq hydrationOpts influxCon kbtzim = S.concatM $ do
  tKbtzim <- atomically $ mkConfig kbtzim
  return $
    (Left <$> s3Hydration tKbtzim)
    `S.parallel`
    (Right <$> mqttStream kbtzim)
  where
    s3Hydration :: TKbtzim -> t m (NodeMAC, Prefix)
    s3Hydration kns = S.concat $ S.unfold (runHydration t0 influxCon hConfDef kns) ()
    mqttStream :: Kbtzim -> t m (KbtzScene NodeMAC)
    mqttStream kns = S.concatMapWith S.parallel (runKibbutz @t) (confss kns)
    confss :: (S.IsStream t, S.MonadAsync m) => Kbtzim -> t m (KbtzC NodeMAC)
    confss = S.fromList . fmap (sConf . second toKbtzG) . M.toList
    sConf (k, kns) = KbtzC { Chopaan.Kibbutz.name = k
                           , structure = kns
                           , channelOpts = Left mq
                           , s3Opts = Just (BucketName (s3BucketName hydrationOpts))
                           , influxCon = influxCon
                           }
    deployKbtz = (KbtzId "Bismillah_Mor", fmap fst deployNodes)



run :: RIO App ()
run = do
  hSetBuffering stdout LineBuffering
  app <- ask
  let
    Options{..} = appOptions app
    KibbutzOpts{..} = kibbutzOpts
  tc <- liftIO $ execParser tkOptions
  --ic <- liftIO $ execParser icOptions
  dir <- liftIO $ getXdgDir XdgData . Just =<< parseRelDir "kbtzim"
  liftIO . print $ "Chopaan Kbtzim Path: " <> (show dir) 
  liftIO $ ensureDir dir
  liftIO $ createDB influxConn mqttDB
  liftIO $ createDB influxConn hydrationDB
  t0 <- liftIO Ti.getCurrentTime
  liftIO $ runGraphM poolConf tc $ do
    kbtzim0 <- readKbtzim dir
    let kEvs = watchKbtzim @GraphM dir
    kbtzim <- fromJust <$> S.head (kbtzimEnv dir)
    S.drain . S.fromAhead $ runKbtzim @S.AheadT t0 mqttOpts hydrationOpts influxConn kbtzim
  where
    mqttDB = "chopaanMQTT"
    hydrationDB = "chopaanS3"


deployNodes :: [(NodeMAC, Int)]
deployNodes = first NodeId <$>
  [ ("7c:9e:bd:48:4e:e0",  1)
  , ("7c:9e:bd:f5:ec:74",  3)
  , ("ac:67:b2:11:f3:10",  5)
  , ("7c:9e:bd:49:07:68",  6)
  , ("ac:67:b2:11:f2:30",  8)
  , ("8c:aa:b5:97:69:48",  9)
  , ("8c:aa:b5:95:8f:9c", 10)
  , ("ac:67:b2:1c:ec:d8", 11)
  , ("7c:9e:bd:47:8a:5c", 12)
  , ("7c:9e:bd:49:1d:80", 13)
  , ("ac:67:b2:1d:e7:f4", 14)
  , ("7c:9e:bd:47:b7:e8", 15)
  ]

-- labNodes = NodeId <$> [ "ac:67:b2:11:f3:20",
--                         "ac:67:b2:1d:e7:f4",
--                         "8c:aa:b5:97:69:48",
--                         "8c:aa:b5:95:97:c8",
--                         "8c:aa:b5:95:8f:9c",
--                         "ac:67:b2:1c:ec:d8",
--                         "7c:9e:bd:f5:ec:74",
--                         "ac:67:b2:11:f0:28"
--                       ]
-- labNodes1 :: [NodeMAC]
-- labNodes1 = NodeId <$>
--   [ "ac:67:b2:11:f3:10"
--   , "ac:67:b2:12:07:b0"
--   , "7c:9e:bd:47:61:bc"
--   , "7c:9e:bd:47:b7:e8"
--   , "7c:9e:bd:48:4e:e0"
--   , "7c:9e:bd:48:a2:c4"
--   , "ac:67:b2:11:e6:e4"
--   ]
